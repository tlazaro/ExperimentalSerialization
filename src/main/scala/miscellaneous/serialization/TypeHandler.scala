package miscellaneous.serialization

import java.io.{OutputStream, InputStream}
import scala.collection.mutable.Queue

abstract class TypeHandler {
  val handledTypes: Seq[ClassManifest[_]]
  
  def introspect(introspector: Introspector, nodeName: String, typeInfo: ClassManifest[_],
      adapters: Queue[Option[Adapter[_, _]]], fieldProxy: Option[FieldProxy], lengthDescriptorSize: Int): NodeDef
  
  def serialize(node: NodeDef, obj: Any, serializer: Serializer, out: OutputStream): Unit
  def deserialize(node: NodeDef, serializer: Serializer, in: InputStream): Any
}