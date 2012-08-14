package miscellaneous
package serialization

/**
 * Adapters allow dynamic object transformation during serialization.
 */
abstract class Adapter[A, B](implicit val fromTypeInfo: ClassManifest[A], val toTypeInfo: ClassManifest[B]) {
  override def toString = fromTypeInfo + " => " + toTypeInfo
  def marshall(a: A): B
  def unmarshall(b: B): A
}

trait NodeDef extends collection.mutable.Buffer[NodeDef] {
  def name: String
  def typeInfo: ClassManifest[_]
  def adapter: Option[Adapter[_, _]]
  def fieldProxy: Option[FieldProxy]
  def lengthDescriptorSize: Int

  override def toString = description
  def description = name + ": " + typeInfo + adapter.map(" {" + _ + "}").getOrElse("")

  def traverse(f: PartialFunction[(NodeDef, Int), Boolean]) {
    def recurse(node: NodeDef, depth: Int) {
      if (f.isDefinedAt(node, depth)) {
        if (f(node, depth)) node foreach (recurse(_, depth + 1))
      }  
    }
    recurse(this, 0)
  }
  
  //  def deepCopy(name: String = this.name,
  //                typeInfo: ClassManifest[_] = this.typeInfo,
  //                adapter: Option[Adapter[_, _]] = this.adapter,
  //                fieldProxy: Option[FieldProxy] = this.fieldProxy,
  //                lengthDescriptorSize: Int = this.lengthDescriptorSize): this.type
}
object NodeDef {
  def treeDebugString(n: NodeDef): String = {
    val sb = new StringBuilder
    n traverse {
      case (node, depth) =>
        sb append "  "*depth append node.description append "\n"
        node match {case nr: NodeRef => false; case _ => true}
    }
    sb.toString
  }
}
trait TypeDef extends NodeDef {
  def typeHandler: TypeHandler
}

case class SNode(name: String,
                 typeInfo: ClassManifest[_],
                 adapter: Option[Adapter[_, _]],
                 fieldProxy: Option[FieldProxy],
                 lengthDescriptorSize: Int) extends NodeDef with collection.mutable.BufferProxy[NodeDef] {
  lazy val self = new collection.mutable.ArrayBuffer[NodeDef]
  
  override def toString = description
}

object ValueNode {
  def apply(name: String,
            typeInfo: ClassManifest[_],
            adapter: Option[Adapter[_, _]],
            fieldProxy: Option[FieldProxy],
            lengthDescriptorSize: Int,
            typeHandler0: TypeHandler) = new SNode(name, typeInfo, adapter, fieldProxy, lengthDescriptorSize) with TypeDef {
    val typeHandler = typeHandler0
  }
}

case class NodeRef(name: String, ref: NodeDef) extends NodeDef with collection.mutable.BufferProxy[NodeDef] {
  def typeInfo = ref.typeInfo
  def adapter = ref.adapter
  def fieldProxy = ref.fieldProxy
  def lengthDescriptorSize = ref.lengthDescriptorSize

  lazy val self = ref
}