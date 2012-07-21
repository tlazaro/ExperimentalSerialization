package miscellaneous.serialization

import java.lang.reflect._
import scala.collection.mutable.{ Queue, Stack, HashMap }
import scala.annotation.Annotation

case class TypeInsight(nodeName: String, typeInfo: ClassManifest[_],
  adapters: Queue[Option[Adapter[_, _]]] = Queue.empty, fieldProxy: Option[FieldProxy] = None,
  lengthDescriptorSize: Int = 4, annotations: Set[Annotation] = Set.empty)