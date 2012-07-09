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
//trait SerializationTreeDef extends Tree {
//  type Node <: super.NodeLike with NodeDef
//
//  def Struct(name: String, typeInfo: ClassManifest[_], adapter: Option[Adapter[_, _]], fieldProxy: Option[FieldProxy], lengthDescriptorSize: Int): Fork
//  def StructRef(name: String, struct: Fork): Fork
//  def Value(name: String, typeInfo: ClassManifest[_], adapter: Option[Adapter[_, _]], fieldProxy: Option[FieldProxy], lengthDescriptorSize: Int, typeHandler: TypeHandler): Fork with TypeDef
//}
//
///**
// * Direct instance of SerializationTreeDef
// */
//object SerializationTree extends Tree with SerializationTreeDef with TreeImpl {
//  type Node = NodeDef with NodeLikeImpl
//  type Fork = Node with ForkLikeImpl
//  type Leaf = SNode with Nothing
//
//  private[SerializationTree] case class SNode(
//      name: String,
//      typeInfo: ClassManifest[_],
//      adapter: Option[Adapter[_, _]],
//      fieldProxy: Option[FieldProxy],
//      lengthDescriptorSize: Int) extends NodeDef with ForkLikeImpl {
//    override def toString = description
//    
//    def deepCopy(name: String,
//                  typeInfo: ClassManifest[_],
//                  adapter: Option[Adapter[_, _]],
//                  fieldProxy: Option[FieldProxy],
//                  lengthDescriptorSize: Int) =
//      (SNode(name, typeInfo, adapter, fieldProxy, lengthDescriptorSize) ++= this.map(_.deepCopy())).asInstanceOf[this.type]
//  }
//  def Struct(name: String, typeInfo: ClassManifest[_], adapter: Option[Adapter[_, _]], fieldProxy: Option[FieldProxy], lengthDescriptorSize: Int) =
//    SNode(name, typeInfo, adapter, fieldProxy, lengthDescriptorSize)
//  def StructRef(name0: String, node: Fork): Fork = new NodeDef with ForkLikeImpl {
//    def name = name0
//    def typeInfo = node.typeInfo
//    def adapter = node.adapter
//    def fieldProxy = node.fieldProxy
//    def lengthDescriptorSize = node.lengthDescriptorSize
//    
//    override lazy val self = node.self
//    
//    def deepCopy(name: String,
//                  typeInfo: ClassManifest[_],
//                  adapter: Option[Adapter[_, _]],
//                  fieldProxy: Option[FieldProxy],
//                  lengthDescriptorSize: Int) = StructRef(name, node.deepCopy()).asInstanceOf[this.type]
//    override def toString = description
//  }
//
//  def Value(name: String, typeInfo: ClassManifest[_], adapter: Option[Adapter[_, _]], fieldProxy: Option[FieldProxy], lengthDescriptorSize: Int, handler: TypeHandler) =
//    new SNode(name, typeInfo, adapter, fieldProxy, lengthDescriptorSize) with TypeDef {
//      lazy val typeHandler = handler
//      
//      override def deepCopy(name: String,
//                             typeInfo: ClassManifest[_],
//                             adapter: Option[Adapter[_, _]],
//                             fieldProxy: Option[FieldProxy],
//                             lengthDescriptorSize: Int) =
//        (Value(name, typeInfo, adapter, fieldProxy, lengthDescriptorSize, handler) ++= this.map(_.deepCopy())).asInstanceOf[this.type]
//    }
//}
//
///**
// * This implementation of SerializationTreeDef makes the nodes compatible
// * with a swing JTree.
// */
//object SerializationTreeForSwing extends SerializationTreeDef with SwingTree {
//  deafTo(this)
//
//  type Node = NodeDef with SwingNode
//  type Fork = Node with PublisherForkLike
//  type Leaf = SNode with Nothing
//  private[SerializationTreeForSwing] case class SNode(
//      name: String,
//      typeInfo: ClassManifest[_],
//      adapter: Option[Adapter[_, _]],
//      fieldProxy: Option[FieldProxy],
//      lengthDescriptorSize: Int) extends NodeDef with SwingNode with PublisherForkLike {
//    def equals(that: this.type) = nodeHash == that.nodeHash
//    override def equals(that: Any) = that match {
//      case that: this.type => equals(that)
//      case _ => false
//    }
//    lazy val nodeHash = System.identityHashCode(this)
//    override def hashCode = nodeHash
//    override def toString = description
//    
//    def deepCopy(name: String,
//                  typeInfo: ClassManifest[_],
//                  adapter: Option[Adapter[_, _]],
//                  fieldProxy: Option[FieldProxy],
//                  lengthDescriptorSize: Int) =
//      (SNode(name, typeInfo, adapter, fieldProxy, lengthDescriptorSize) ++= this.map(_.deepCopy())).asInstanceOf[this.type]
//  }
//
//  def Struct(name: String, typeInfo: ClassManifest[_], adapter: Option[Adapter[_, _]], fieldProxy: Option[FieldProxy], lengthDescriptorSize: Int) =
//    SNode(name, typeInfo, adapter, fieldProxy, lengthDescriptorSize)
//  def StructRef(name0: String, node: Fork): Fork = new NodeDef with SwingNode with PublisherForkLike {
//    def name = name0
//    def typeInfo = node.typeInfo
//    def adapter = node.adapter
//    def fieldProxy = node.fieldProxy
//    def lengthDescriptorSize = node.lengthDescriptorSize
//    
//    override lazy val self = node.self
//    
//    def deepCopy(name: String,
//                  typeInfo: ClassManifest[_],
//                  adapter: Option[Adapter[_, _]],
//                  fieldProxy: Option[FieldProxy],
//                  lengthDescriptorSize: Int) = StructRef(name, node.deepCopy()).asInstanceOf[this.type]
//    lazy val nodeHash = node.nodeHash
//    def equals(that: this.type) = nodeHash == that.nodeHash
//    override def toString = description
//  }
//
//  def Value(name: String, typeInfo: ClassManifest[_], adapter: Option[Adapter[_, _]], fieldProxy: Option[FieldProxy], lengthDescriptorSize: Int, handler: TypeHandler) =
//    new SNode(name, typeInfo, adapter, fieldProxy, lengthDescriptorSize) with TypeDef {
//      lazy val typeHandler = handler
//      
//      override def deepCopy(name: String,
//                             typeInfo: ClassManifest[_],
//                             adapter: Option[Adapter[_, _]],
//                             fieldProxy: Option[FieldProxy],
//                             lengthDescriptorSize: Int) =
//        (Value(name, typeInfo, adapter, fieldProxy, lengthDescriptorSize, handler) ++= this.map(_.deepCopy())).asInstanceOf[this.type]
//    }
//
//  def show(node: Node, modalDialog: Boolean = true) {
//    import swing._
//    import javax.swing._
//    new JDialog {
//      setTitle(node.name)
//      setModal(modalDialog)
//      setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
//
//      new JTree(new miscellaneous.swing.tree.TreeModel(SerializationTreeForSwing.this) {
//        lazy val root = node match {
//          case fork: Fork => fork
//        }
//      }) -> (new JScrollPane center getContentPane)
//
//      pack()
//      setLocationRelativeTo(null)
//    }.setVisible(true)
//  }
//}