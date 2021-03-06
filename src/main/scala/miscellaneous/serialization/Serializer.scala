package miscellaneous.serialization

import java.io.{ OutputStream, InputStream }
import scala.annotation.unchecked.uncheckedStable

trait Serializer {

  val introspector: Introspector

  /** Writes and reads the provided instance thus making a copy of it. */
  def copy[T: ClassManifest](t: T): T = {
    val baos = new java.io.ByteArrayOutputStream(150)
    write(introspector.introspect("root")(manifest), t, baos)
    val copy = read(new java.io.ByteArrayInputStream(baos.toByteArray))(manifest)
    try {
      baos.close()
    } catch {
      case _ =>
    }
    copy
  }
  
  def write[T](ev: ClassManifest[T], t: T, out: OutputStream) {
    write(introspector.introspect("root")(ev), t, out)
  }

  def write(any: Any, out: OutputStream) {
    write(introspector.introspect("root", any.getClass), any, out)
  }

  def write(name: String, any: Any, out: OutputStream) {
    write(introspector.introspect(name, any.getClass), any, out)
  }

  def write(name: String, typeInfo: ClassManifest[_], any: Any, out: OutputStream) {
    write(introspector.introspect(TypeInsight(name, typeInfo)), any, out)
  }

  def read[R: ClassManifest](in: InputStream): R = {
    read(introspector.introspect[R]("root"), None, in).asInstanceOf[R]
  }
  def read[R](any: Any, in: InputStream): R = {
    read(introspector.introspect("root", any.getClass), Some(any.asInstanceOf[AnyRef]), in).asInstanceOf[R]
  }
  def read[R: ClassManifest](name: String, in: InputStream): R = {
    read(introspector.introspect[R](name), None, in).asInstanceOf[R]
  }
  def read[R](name: String, any: Any, in: InputStream): R = {
    read(introspector.introspect(name, any.getClass), Some(any.asInstanceOf[AnyRef]), in).asInstanceOf[R]
  }

  /**
   * Makes the serializer write a block start with the given name, a size, and that size written according to the sizeUnitLength.<p/>
   * For example, a List of size 10 could invoke: writeBlockStart("List", 10, 1);
   * @param blockName
   * @param size
   * @param sizeUnitLength
   */
  def writeBlockStart(blockName: String, size: Int, lengthDescriptorSize: Int, out: OutputStream)

  /**
   * An end corresponding to writeBlockStart
   * @param blockName
   */
  def writeBlockEnd(blockName: String, out: OutputStream)

  def write(name: String, value: Array[Byte], out: OutputStream)
  def writeByte(name: String, value: Byte, out: OutputStream)
  def writeShort(name: String, value: Short, out: OutputStream)
  def writeInt(name: String, value: Int, out: OutputStream)
  def writeLong(name: String, value: Long, out: OutputStream)
  def writeFloat(name: String, value: Float, out: OutputStream)
  def writeDouble(name: String, value: Double, out: OutputStream)
  def writeBoolean(name: String, value: Boolean, out: OutputStream)
  def writeChar(name: String, value: Char, out: OutputStream)
  def writeString(name: String, value: String, out: OutputStream)

  def write(node: NodeDef, obj: Any, out: OutputStream) {
    @inline def valueforNode(node: NodeDef) = {
      val res = if (node.fieldProxy.isDefined) {
        //        println("Retrieving value from field " + node + " using " + node.fieldProxy)
        node.fieldProxy.get.getValue(obj.asInstanceOf[AnyRef])
      } else obj
      node.adapter match {
        case Some(adapter) =>
          //          println("Marshalling " + res + " through " + adapter + " on node " + node)
          adapter.asInstanceOf[Adapter[Any, Any]].marshall(res)
        case _ => res
      }
    }
    try {
      node match {
        case node: TypeDef =>
          node.typeHandler.serialize(node, obj, this, out)
        case struct =>
          writeBlockStart(node.name, -1, -1, out)
          writeStructNodes(struct, valueforNode, out)
          writeBlockEnd(node.name, out)
      }
    } catch {
      case other => throw new SerializationException("Exception ocurred writing node: " + node.description + ", value = " + obj, other)
    }
  }

  /**
   * Writes the fields of the given struct. This method is called by write(node: introspector.tree.Fork, obj: Any, out: OutputStream)
   * when it needs to write the fields. It is declared like this so that it can be overriden by serializers
   * that need it.
   */
  protected def writeStructNodes(nodes: Seq[NodeDef], valueForNode: NodeDef => Any, out: OutputStream) {
    for (node <- nodes) {
      write(node, valueForNode(node), out)
    }
  }

  /**
   * Reads a block start according to a writeBlockStart. This method returns the size of the list.
   * @param blockName
   * @param size
   * @param lengthDescriptorSize
   * @return
   */
  def readBlockStart(blockName: String, lengthDescriptorSize: Int, in: InputStream): Int
  /**
   * Equivalent to writeBlockEnd
   */
  def readBlockEnd(blockName: String, in: InputStream)

  def read(name: String, bytes: Array[Byte], in: InputStream): Array[Byte]
  def readByte(name: String, in: InputStream): Byte
  def readShort(name: String, in: InputStream): Short
  def readInt(name: String, in: InputStream): Int
  def readLong(name: String, in: InputStream): Long
  def readFloat(name: String, in: InputStream): Float
  def readDouble(name: String, in: InputStream): Double
  def readBoolean(name: String, in: InputStream): Boolean
  def readChar(name: String, in: InputStream): Char
  def readString(name: String, in: InputStream): String

  private[this] val noArgs = new Array[AnyRef](0)
  def read(node: NodeDef, target: Option[AnyRef], in: InputStream): Any = {
    try {
      val res = node match {
        case node: TypeDef => node.typeHandler.deserialize(node, this, in)
        case struct =>
          val valueObj = target.getOrElse {
            val objType = node.adapter map (_.toTypeInfo) getOrElse node.typeInfo
            introspector.sourceProvider.constructorSource(objType.erasure).getValue(noArgs).asInstanceOf[AnyRef]
          }
          readBlockStart(struct.name, -1, in)
          struct foreach {
            case f =>
              val fieldProxy = f.fieldProxy
              if (fieldProxy.isEmpty) throw new IllegalStateException("Field '" + f + "' in " + struct + " does not define a fieldProxy?")
              fieldProxy.get.setValue(valueObj, read(f, None, in))
          }
          readBlockEnd(struct.name, in)
          valueObj
      }
      node.adapter match {
        case Some(adapter) => adapter.asInstanceOf[Adapter[Any, Any]].unmarshall(res)
        case _ => res
      }
    } catch {
      case se: SerializationException => throw se
      case other => throw new SerializationException("Exception ocurred reading node: " + node.description, other)
    }
  }

}

class SerializationException(msg: String = null, cause: Throwable = null) extends RuntimeException(msg, cause)
