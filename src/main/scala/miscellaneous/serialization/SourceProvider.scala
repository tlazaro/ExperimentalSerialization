package miscellaneous.serialization

import java.lang.reflect.{ Array => _, _ }

trait FieldProxy extends Source with Sink

/**
 * Defines the source for a SNode.
 */
trait Source {
  def getValue(obj: AnyRef): Any
}

trait Sink {
  def setValue(obj: AnyRef, value: Any)
}

trait SourceProvider {
  def fieldSource(field: Field): FieldProxy
  def constructorSource(clazz: Class[_]): Source
  def fieldProxy(getter: Method): FieldProxy = {
    val decClass = getter.getDeclaringClass()
    val name = getter.getName()
    def setterAnnotated() =
      throw new IllegalArgumentException("Only getters can be annotated as SField. Invalid annotated method " +
        name + " in " + decClass)
    def noSetterFound() =
      throw new IllegalArgumentException("No setter found for getter " + name + " in " + decClass)
    if (name endsWith "_$eq") setterAnnotated()
    if (name startsWith "set") setterAnnotated()

    val setterName = if (name startsWith "get") { //find java setter
      "set" + name.stripPrefix("get")
    } else { //find a scala setter
      name + "_$eq"
    }

    val methodModifiers = getter.getModifiers()
    val setterOp = (if (Modifier.isPublic(methodModifiers) || Modifier.isProtected(methodModifiers))
      decClass.getMethods()
    else
      decClass.getDeclaredMethods()).find(_.getName == setterName)
    val setter = setterOp getOrElse noSetterFound()

    //validate that the return type of the getter is the same as the argument of the setter
    val gpt = setter.getGenericParameterTypes()
    if (gpt.length != 1) throw new IllegalArgumentException("Setter for " + name + " in " + decClass + " doesn't take just one argument. Setter: " + setter.toGenericString())
    if (getter.getGenericReturnType() != gpt(0))
      throw new IllegalArgumentException("Setter for " + name + " in " + decClass +
        " takes an argument of different type than the getter. getter(" + getter.getGenericReturnType() + ") != setter(" + gpt(0) + ")")
    getter.setAccessible(true)
    setter.setAccessible(true)
    new FieldProxy {
      val acc = methodSource(getter)
      val mut = methodSink(setter)
      def getValue(obj: AnyRef): Any = acc.getValue(obj)
      def setValue(obj: AnyRef, value: Any) = mut.setValue(obj, value)
    }
  }
  def methodSource(m: Method): Source
  def methodSink(m: Method): Sink
}

object ReflectionSourceProvider extends SourceProvider {
  def fieldSource(field: Field) = new FieldProxy {
    def setValue(obj: AnyRef, value: Any) = field.set(obj, value)
    def getValue(obj: AnyRef): Any = field.get(obj)
  }

  def constructorSource(clazz: Class[_]) = new Source {
    val constructor = clazz.getDeclaredConstructors.find(_.getParameterTypes().length == 0).get
    constructor setAccessible true
    def getValue(obj: AnyRef) = obj match {
      case arr: Array[_] => constructor.newInstance(arr.asInstanceOf[Array[AnyRef]]: _*)
      case seq: Seq[_] => constructor.newInstance(seq.asInstanceOf[Seq[AnyRef]]: _*)
      case other => constructor.newInstance(other)
    }
  }

  def methodSource(m: Method) = new Source {
    def getValue(obj: AnyRef): Any = m.invoke(obj)
  }
  def methodSink(m: Method) = new Sink {
    def setValue(obj: AnyRef, value: Any) = m.invoke(obj, value.asInstanceOf[AnyRef])
  }
}

object MethodHandleSourceProvider extends SourceProvider {
  import java.lang.invoke._

  private[this] val lookup = MethodHandles.lookup()
  private[this] val unreflectedConstructors = new collection.mutable.HashMap[Class[_], Source]
  def fieldSource(field: Field) = new FieldProxy {
    private[this] val mhgetter = lookup.unreflectGetter(field)
    private[this] val mhsetter = lookup.unreflectSetter(field)
    def setValue(obj: AnyRef, value: Any) = mhsetter.invokeWithArguments(obj, value.asInstanceOf[AnyRef])
    def getValue(obj: AnyRef): Any = mhgetter.invokeWithArguments(Array(obj): _*)
  }

  def methodSource(m: Method) = new Source {
    private[this] val mhgetter = lookup.unreflect(m)
    def getValue(obj: AnyRef): Any = mhgetter.invokeWithArguments(obj)
  }
  def methodSink(m: Method) = new Sink {
    private[this] val mhsetter = lookup.unreflect(m)
    def setValue(obj: AnyRef, value: Any) = mhsetter.invokeWithArguments(obj, value.asInstanceOf[AnyRef])
  }
  
  def constructorSource(clazz: Class[_]) = {
    unreflectedConstructors.get(clazz) getOrElse {
      val constructor = clazz.getDeclaredConstructors.find(_.getParameterTypes().length == 0).get
      constructor setAccessible true
      val unreflectedCons = lookup.unreflectConstructor(constructor)
      val res = new Source {
        def getValue(obj: AnyRef) = obj match {
          case arr: Array[_] => unreflectedCons.invokeWithArguments(arr.asInstanceOf[Array[AnyRef]]: _*)
          case seq: Seq[_] => unreflectedCons.invokeWithArguments(seq.asInstanceOf[Seq[AnyRef]]: _*)
          case other => unreflectedCons.invokeWithArguments(other)
        }
      }
      unreflectedConstructors(clazz) = res
      res
    }
  }
}