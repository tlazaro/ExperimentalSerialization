package miscellaneous.serialization

import java.lang.reflect._
import java.lang.annotation.Annotation
import scala.collection.mutable.{ Queue, Stack, HashMap }

case class TypeInsight(nodeName: String, typeInfo: ClassManifest[_], annotations: Set[Annotation] = Set.empty,
  adapters: Queue[Option[Adapter[_, _]]] = Queue.empty, fieldProxy: Option[FieldProxy] = None,
  lengthDescriptorSize: Int = 4)
