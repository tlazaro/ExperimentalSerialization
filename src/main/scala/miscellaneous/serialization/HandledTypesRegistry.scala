package miscellaneous
package serialization

import java.util.ServiceLoader
import scala.collection.JavaConversions._

object HandledTypesRegistry {
  reload()

  private[this] var registeredTypes: Map[ClassManifest[_], TypeHandler] = _
  private[this] var memoAssociations: Map[ClassManifest[_], Option[TypeHandler]] = _

  def reload() {
    memoAssociations = Map.empty
    registeredTypes = Map.empty
    for {
      provider <- ServiceLoader.load(classOf[TypeHandlerProvider]) ++ List(new BuiltinTypeHandlers)
      typeHandler <- provider.handlers
      handledType <- typeHandler.handledTypes
    } {
      registeredTypes = registeredTypes.updated(handledType, typeHandler)
    }
  }

  def handlerFor(type0: ClassManifest[_]): Option[TypeHandler] = {
    memoAssociations.get(type0) getOrElse {
      val res = registeredTypes find (_._1 == type0) orElse
        (registeredTypes find (p => ReflectionUtilities.isSubtype(type0, p._1))) map (_._2)

      memoAssociations = memoAssociations.updated(type0, res)
      res
    }
  }
}
