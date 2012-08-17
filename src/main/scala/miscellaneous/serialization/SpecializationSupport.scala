package miscellaneous.serialization

trait SpecializationSupport extends Introspector {
  override def introspect(i: TypeInsight): NodeDef = {
    var ti = i
    for (sf <- i.annotations.collect {case sf: specializedFor => sf} if sf.target == i.typeInfo.erasure) {
      ti = i.copy(typeInfo = new ClassManifest[AnyRef] {
        def erasure = i.typeInfo.erasure
        override def typeArguments = sf.params.map(ClassManifest.classType(_)).toList
        override def toString = (if (erasure.isArray) "Array" else erasure.getName) + argString
      })
    }
    super.introspect(ti)
  }
}