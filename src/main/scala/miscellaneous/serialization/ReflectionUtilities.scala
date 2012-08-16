package miscellaneous.serialization

import java.lang.reflect.{ Type, ParameterizedType }

private[serialization] object ReflectionUtilities {
  def calculateManifest(typpe: Type): ClassManifest[_] = {
    typpe match {
      case pt: ParameterizedType => new ClassManifest[AnyRef] {
        def erasure = pt.getRawType().asInstanceOf[Class[_]]
        override def typeArguments = pt.getActualTypeArguments().map(calculateManifest).toList
        override def toString = (if (erasure.isArray) "Array" else erasure.getName) + argString
      }
      case clazz: Class[_] => ClassManifest.classType(clazz)
      case other           => throw new IllegalArgumentException("Cannot calculate the manifest for types which are not ParameterizedTypes or Classes. Found: " + other)
    }
  }

  def isSubtype(a: ClassManifest[_], b: ClassManifest[_]): Boolean = {
    a <:< b || (b.erasure.isAssignableFrom(a.erasure) && (a.typeArguments.length == b.typeArguments.length
      && a.typeArguments.zip(b.typeArguments).forall {
        case (type1: ClassManifest[_], type2: ClassManifest[_]) => isSubtype(type1, type2)
        case (type1: ClassManifest[_], NoManifest) => true // _ matches any class
        case other => false
      })) || (a.erasure.isArray() && b.erasure.isArray() &&
        (b.erasure.getComponentType == classOf[AnyRef] || b.erasure.getComponentType.isAssignableFrom(a.erasure.getComponentType)))
  }
}