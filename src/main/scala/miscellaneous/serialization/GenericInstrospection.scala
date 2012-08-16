package miscellaneous.serialization

trait GenericInstrospection extends TypeHandler {
  def introspect(introspector: Introspector, i: TypeInsight): NodeDef = {

    (ValueNode(i.nodeName, i.typeInfo, None, i.fieldProxy, i.lengthDescriptorSize, this) ++=
      i.typeInfo.typeArguments.zipWithIndex.map(zip =>
        introspector.introspect(TypeInsight("T" + zip._2, zip._1.asInstanceOf[ClassManifest[_]], i.annotations, i.adapters, null))))
  }
}