package miscellaneous.serialization

import java.lang.reflect.{ Type, ParameterizedType }

private[serialization] object ReflectionUtilities {
  def calculateManifest(typpe: Type): ClassManifest[_] = {
    println("type: " + typpe)
    typpe match {
      case pt: ParameterizedType => new ClassManifest[AnyRef] {
        def erasure = pt.getRawType().asInstanceOf[Class[_]]
        override def typeArguments = pt.getActualTypeArguments().map(calculateManifest).toList
        override def toString = (if (erasure.isArray) "Array" else erasure.getName) + argString
      }
      case clazz: Class[_] => ClassManifest.classType(clazz)
      case _ => throw new IllegalArgumentException
    }
  }
}