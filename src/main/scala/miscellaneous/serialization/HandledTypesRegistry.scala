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
    for {provider <- ServiceLoader.load(classOf[TypeHandlerProvider])
         typeHandler <- provider.handlers
         handledType <- typeHandler.handledTypes} {
           registeredTypes = registeredTypes.updated(handledType, typeHandler)
    }
     
  }
  
  def handlerFor(type0: ClassManifest[_]): Option[TypeHandler] = {
    memoAssociations.get(type0) getOrElse {
      val res = registeredTypes find (_._1 == type0) orElse 
      (registeredTypes find (p => isSubtype(type0, p._1))) map (_._2)
      
      memoAssociations = memoAssociations.updated(type0, res)
      res
    }
  }
  
  private[this] def isSubtype(a: ClassManifest[_], b: ClassManifest[_]): Boolean = {
    a <:< b || (b.erasure.isAssignableFrom(a.erasure) && (a.typeArguments.length == b.typeArguments.length
        && a.typeArguments.zip(b.typeArguments).forall {
      case (type1: ClassManifest[_], type2: ClassManifest[_]) => isSubtype(type1, type2)
      case (type1: ClassManifest[_], NoManifest) => true // _ matches any class
      case other => false 
    })) || (a.erasure.isArray() && b.erasure.isArray() && 
        (b.erasure.getComponentType == classOf[AnyRef] || b.erasure.getComponentType.isAssignableFrom(a.erasure.getComponentType)))
  }
}
