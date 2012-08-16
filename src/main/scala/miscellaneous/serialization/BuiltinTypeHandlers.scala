package miscellaneous
package serialization

import scala.collection.mutable.Queue
import java.io.{ OutputStream, InputStream }

/**
 * This class is picked up by the TypeHandlerProvider service loader and registers the default handlers.
 */
class BuiltinTypeHandlers extends TypeHandlerProvider {
  private[this] var handlers0 = Vector.empty[TypeHandler]
  private[BuiltinTypeHandlers] trait TypeHandler extends miscellaneous.serialization.TypeHandler {
    handlers0 :+= this
  }

  @inline private def casted[R](a: Any) = a.asInstanceOf[R]
  
  private[BuiltinTypeHandlers] trait BasicIntrospecImpl extends TypeHandler {
    def introspect(introspector: Introspector, i: TypeInsight): NodeDef =
      ValueNode(i.nodeName, i.typeInfo, None, i.fieldProxy, i.lengthDescriptorSize, this)
  }

  val ByteHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.lang.Byte], ClassManifest.classType(java.lang.Byte.TYPE))

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeByte(node.name, casted[Number](obj).byteValue, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readByte(node.name, in)
    }
  }
  val ShortHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.lang.Short], ClassManifest.classType(java.lang.Short.TYPE))

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeShort(node.name, casted[Number](obj).shortValue, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readShort(node.name, in)
    }
  }
  val CharHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.lang.Character], ClassManifest.classType(java.lang.Character.TYPE))

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeChar(node.name, casted(obj), out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readChar(node.name, in)
    }
  }
  val IntHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.lang.Integer], ClassManifest.classType(java.lang.Integer.TYPE))

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeInt(node.name, casted[Number](obj).intValue, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readInt(node.name, in)
    }
  }
  val LongHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.lang.Long], ClassManifest.classType(java.lang.Long.TYPE))

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeLong(node.name, casted(obj), out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readLong(node.name, in)
    }
  }
  val FloatHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.lang.Float], ClassManifest.classType(java.lang.Float.TYPE))

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeFloat(node.name, casted[Number](obj).floatValue, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readFloat(node.name, in)
    }
  }
  val DoubleHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.lang.Double], ClassManifest.classType(java.lang.Double.TYPE))

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeDouble(node.name, casted[Number](obj).doubleValue, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readDouble(node.name, in)
    }
  }
  val BooleanHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.lang.Boolean], ClassManifest.classType(java.lang.Boolean.TYPE))

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeBoolean(node.name, casted(obj), out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readBoolean(node.name, in)
    }
  }
  val StringHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[String])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeString(node.name, casted(obj), out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readString(node.name, in)
    }
  }

  val StdJavaDateHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.util.Date])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeLong(node.name, obj.asInstanceOf[java.util.Date].getTime(), out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      new java.util.Date(serializer.readLong(node.name, in))
    }
  }

  val StdJavaCalendarHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.util.Calendar])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeBlockStart(node.name, -1, -1, out)
      val c = obj.asInstanceOf[java.util.Calendar]
      serializer.writeLong("time", c.getTimeInMillis(), out)
      serializer.writeString("timeZone", c.getTimeZone().getID(), out)
      serializer.writeBlockEnd(node.name, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readBlockStart(node.name, -1, in)
      val time = serializer.readLong("time", in)
      val zone = serializer.readString("timeZone", in)
      serializer.readBlockEnd(node.name, in)
      val res = java.util.Calendar.getInstance(java.util.TimeZone.getTimeZone(zone))
      res.setTimeInMillis(time)
      res
    }
  }

  val BigIntegerHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.math.BigInteger])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeString(node.name, obj.asInstanceOf[java.math.BigInteger].toString(), out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      new java.math.BigInteger(serializer.readString(node.name, in))
    }
  }

  val BigDecimalHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[java.math.BigDecimal])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      serializer.writeString(node.name, obj.asInstanceOf[java.math.BigDecimal].toString(), out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      new java.math.BigDecimal(serializer.readString(node.name, in))
    }
  }

  val EnumerationHandler = new TypeHandler with BasicIntrospecImpl {
    val handledTypes = Seq(classManifest[Enumeration#Value])
    
    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      val enum = obj.asInstanceOf[Enumeration#Value]
      
      
      serializer.writeBlockStart(node.name, -1, -1, out)
      serializer.writeString("enum", enum.getClass.getField("$outer").get(enum).getClass.getName, out)
      serializer.writeInt("id", enum.id, out)
      serializer.writeBlockEnd(node.name, out)
    }

    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      serializer.readBlockStart(node.name, -1, in)
      
      val className = serializer.readString("enum", in)
      val companion = Class.forName(className) //find the object
      require(companion != null, "companion object for class " + className + " not found!")
      val moduleField = companion.getField("MODULE$")
      require(moduleField != null, "coult not find companion singleton for class " + className)
      val enumClass = moduleField.get().asInstanceOf[Enumeration]
      
      val res = enumClass(serializer.readInt("id", in))
      serializer.readBlockEnd(node.name, in)
      res
    }
  }
  
  val Tuple2Handler = new TypeHandler with GenericInstrospection {
    val handledTypes = Seq(classManifest[Tuple2[Any, Any]])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      val tuple = obj.asInstanceOf[Tuple2[_, _]]
      
      val _1 = node.head
      val _2 = node.tail.head
      
      serializer.writeBlockStart(node.name, -1, -1, out)
      serializer.write(_1, tuple._1, out)
      serializer.write(_2, tuple._2, out)
      serializer.writeBlockEnd(node.name, out)
    }

    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      val _1 = node.head
      val _2 = node.tail.head

      serializer.readBlockStart(node.name, -1, in)
      val res = (serializer.read(_1, None, in), serializer.read(_2, None, in))
      serializer.readBlockEnd(node.name, in)
      res
    }
  }

  val OptionHandler = new TypeHandler with GenericInstrospection {
    val handledTypes = Seq(classManifest[Option[_]])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      val opEntry = node.head
      val option = obj.asInstanceOf[Option[_]]

      serializer.writeBlockStart(node.name, if (option.isDefined) 1 else 0, 1, out)
      option match {
        case Some(_) => serializer.write(opEntry, option.get, out)
        case _ =>
      }
      serializer.writeBlockEnd(node.name, out)
    }
    
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      val opEntry = node.head

      val res = serializer.readBlockStart(node.name, 1, in) match {
        case 1 => Some(serializer.read(opEntry, None, in))
        case _ => None
      }
      serializer.readBlockEnd(node.name, in)

      res
    }
  }

  val EitherHandler = new TypeHandler with GenericInstrospection {
    val handledTypes = Seq(classManifest[Either[_, _]])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      val leftEntry = node.head
      val rightEntry = node.tail.head
      val either = obj.asInstanceOf[Either[_, _]]

      serializer.writeBlockStart(node.name, -1, -1, out)
      serializer.writeBoolean("isRight", either.isRight, out)
      either match {
        case Left(v) => serializer.write(leftEntry, v, out)
        case Right(v) => serializer.write(rightEntry, v, out)
      }
      serializer.writeBlockEnd(node.name, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      val leftEntry = node.head
      val rightEntry = node.tail.head

      serializer.readBlockStart(node.name, -1, in)
      val res = serializer.readBoolean("isRight", in) match {
        case false => Left(serializer.read(leftEntry, None, in))
        case true => Right(serializer.read(rightEntry, None, in))
      }
      serializer.readBlockEnd(node.name, in)

      res
    }
  }

  //**********Collections***********

  val ArrayHandler = new TypeHandler {
    val handledTypes = Seq[ClassManifest[_]](classManifest[Array[Any]])

    def introspect(introspector: Introspector, i: TypeInsight): NodeDef = {
      val entry = introspector.introspect(TypeInsight("entryTemplate",
        ReflectionUtilities.calculateManifest(i.typeInfo.erasure.getComponentType), Set.empty, i.adapters))
      ValueNode(i.nodeName, i.typeInfo, None, i.fieldProxy, i.lengthDescriptorSize, this) += entry
    }

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      val arr = obj.asInstanceOf[Array[_]]
      val entryNode = node.head

      serializer.writeBlockStart(node.name, arr.length, node.lengthDescriptorSize, out)
      if (entryNode.typeInfo.erasure == classOf[Byte]) {
        serializer.write(node.name, arr.asInstanceOf[Array[Byte]], out)
      } else {
        var i = -1
        while ({ i += 1; i < arr.length }) serializer.write(entryNode, arr(i), out)
      }

      serializer.writeBlockEnd(node.name, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      val entryNode = node.head
      val length = serializer.readBlockStart(node.name, node.lengthDescriptorSize, in)
      val res = entryNode.typeInfo.newArray(length)
      if (entryNode.typeInfo.erasure == classOf[Byte]) {
        serializer.read(node.name, res.asInstanceOf[Array[Byte]], in)
      } else {
        var i = -1
        while ({ i += 1; i < length }) res(i) = casted(serializer.read(entryNode, None, in))
      }
      serializer.readBlockEnd(node.name, in)
      res
    }
  }

  val JListHandler = new TypeHandler with GenericInstrospection {
    val handledTypes = Seq[ClassManifest[_]](classManifest[java.util.List[_]], classManifest[java.util.Set[_]])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      val list = obj.asInstanceOf[java.util.Collection[_]]
      val entryNode = node.head

      serializer.writeBlockStart(node.name, list.size(), node.lengthDescriptorSize, out)

      var it = list.iterator
      while (it.hasNext()) serializer.write(entryNode, it.next, out)

      serializer.writeBlockEnd(node.name, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      val res = if (classOf[java.util.List[_]].isAssignableFrom(node.typeInfo.erasure)) new java.util.ArrayList[Any]()
      else new java.util.HashSet[Any]

      val entryNode = node.head
      val length = serializer.readBlockStart(node.name, node.lengthDescriptorSize, in)
      var i = -1
      while ({ i += 1; i < length }) res.add(serializer.read(entryNode, None, in))
      serializer.readBlockEnd(node.name, in)
      res
    }
  }

  val JMapHandler = new TypeHandler with GenericInstrospection {
    val handledTypes = Seq(classManifest[java.util.Map[_, _]])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      val map = obj.asInstanceOf[java.util.Map[_, _]]
      val keyNode = node.head
      val valueNode = node.tail.head

      serializer.writeBlockStart(node.name, map.size(), node.lengthDescriptorSize, out)

      var it = map.entrySet().iterator()
      while (it.hasNext()) {
        serializer.writeBlockStart("entry", -1, -1, out)
        val entry = it.next
        serializer.write(keyNode, entry.getKey, out)
        serializer.write(valueNode, entry.getValue, out)
        serializer.writeBlockEnd("entry", out)
      }

      serializer.writeBlockEnd(node.name, out)
    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      val res = new java.util.HashMap[Any, Any]
      val keyNode = node.head
      val valueNode = node.tail.head

      val length = serializer.readBlockStart(node.name, node.lengthDescriptorSize, in)
      var i = -1
      while ({ i += 1; i < length }) {
        //read the entry object
        serializer.readBlockStart("entry", -1, in)
        res.put(serializer.read(keyNode, None, in), serializer.read(valueNode, None, in))
        serializer.readBlockEnd("entry", in)
      }
      serializer.readBlockEnd(node.name, in)
      res
    }
  }

  val TraversableHandler = new TypeHandler with GenericInstrospection {
    val handledTypes = Seq(classManifest[collection.Traversable[Any]])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      val trav = obj.asInstanceOf[Traversable[Any]]
      val entryNode = node.head

      serializer.writeBlockStart(node.name, trav.size, node.lengthDescriptorSize, out)
      for (entry <- trav) serializer.write(entryNode, entry, out)
      serializer.writeBlockEnd(node.name, out)

    }
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      val builder = newBuilder(node.typeInfo.erasure, serializer.introspector)
      val entryNode = node.head

      val length = serializer.readBlockStart(node.name, node.lengthDescriptorSize, in)
      var i = -1
      while ({ i += 1; i < length }) builder += serializer.read(entryNode, None, in)
      serializer.readBlockEnd(node.name, in)
      builder.result
    }
  }

  val MapHandler = new TypeHandler with GenericInstrospection {
    val handledTypes = Seq(classManifest[collection.Map[_, _]])

    def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream) {
      val map = obj.asInstanceOf[Map[_, _]]
      val keyNode = node.head
      val valueNode = node.tail.head

      serializer.writeBlockStart(node.name, map.size, node.lengthDescriptorSize, out)

      for (entry <- map) {
        serializer.writeBlockStart("entry", -1, -1, out)
        serializer.write(keyNode, entry._1, out)
        serializer.write(valueNode, entry._2, out)
        serializer.writeBlockEnd("entry", out)
      }

      serializer.writeBlockEnd(node.name, out)
    }
    
    def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any = {
      val res = newBuilder(node.typeInfo.erasure, serializer.introspector)
      val keyNode = node.head
      val valueNode = node.tail.head

      val length = serializer.readBlockStart(node.name, node.lengthDescriptorSize, in)
      var i = -1
      while ({ i += 1; i < length }) {
        //read the entry object
        serializer.readBlockStart("entry", -1, in)
        res += (serializer.read(keyNode, None, in) -> serializer.read(valueNode, None, in))
        serializer.readBlockEnd("entry", in)
      }
      serializer.readBlockEnd(node.name, in)
      res.result
    }
  }

  private[this] var cachedBuilders: Map[Class[_], () => scala.collection.mutable.Builder[Any, _]] = Map.empty
  private[this] def newBuilder(c: Class[_], introspector: Introspector) = {
    (cachedBuilders.get(c) getOrElse {
      val companion = Class.forName(c.getName + "$") //find the object
      require(companion != null, "companion object for class " + c.getName + " not found!")
      val moduleField = companion.getField("MODULE$")
      require(moduleField != null, "coult not find companion singleton for class " + c.getName)
      val module = moduleField.get()
      val source = introspector.sourceProvider.methodSource(companion.getMethod("newBuilder"))
      val res = () => source.getValue(module).asInstanceOf[scala.collection.mutable.Builder[Any, _]]
      cachedBuilders = cachedBuilders.updated(c, res)
      res
    })()
  }

  def handlers = handlers0
}