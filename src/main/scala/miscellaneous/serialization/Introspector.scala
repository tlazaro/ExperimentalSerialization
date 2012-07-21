package miscellaneous
package serialization

import java.lang.reflect._
import scala.collection.mutable.{ Queue, Stack, HashMap }
import scala.annotation.Annotation

class Introspector {

  val sourceProvider = {
    try {
      Class.forName("java.lang.invoke.MethodHandles")
      MethodHandleSourceProvider
    } catch { case _ => ReflectionSourceProvider }
  }
  private[this] val sfieldOnly: PartialFunction[AnnotatedElement, (FieldSettings, AnnotatedElement)] = {
    case e: AnnotatedElement if e.isAnnotationPresent(classOf[SField]) => getFieldSettings(e) -> e
  }

  private case class FieldSettings(
    order: Int,
    hookType: Option[Class[_]],
    adapters: Queue[Option[Adapter[_, _]]],
    lengthDescriptorSize: Int)

  private[this] def getFieldSettings(e: AnnotatedElement): FieldSettings = {
    val ann = e.getAnnotation(classOf[SField])
    val adaptersClassOp = ann.adapters map Option.apply
    val adaptersOp = adaptersClassOp map (_ map (_.newInstance()))
    val hookType = if (ann.`type` == classOf[Object]) None else Some(ann.`type`)
    FieldSettings(ann.value, hookType, Queue(adaptersOp: _*), ann.lengthDescriptorSize)

  }

  def introspect(name: String, typpe: Type): NodeDef = {
    val typeInfo = typpe match {
      case pt: ParameterizedType =>
        ReflectionUtilities.calculateManifest(pt)
      case clazz: Class[_] =>
        require(clazz.getTypeParameters().length == 0, "Cannot introspect a raw class which takes type parameters. Please use the introspect version that takes a Manifest instead. Invalid class: " + clazz)
        ReflectionUtilities.calculateManifest(clazz)
      case _ => throw new IllegalArgumentException("Only classes and parameterized types are supported")
    }
    introspect(name)(typeInfo)
  }

  def introspect[T: ClassManifest](name: String): NodeDef = {
    introspect(TypeInsight(name, classManifest[T]))
  }

  def introspect(i: TypeInsight): NodeDef = {
    HandledTypesRegistry.handlerFor(i.typeInfo) match {
      case Some(handler) => handler.introspect(this, i)
      case _ =>
        //if it wasn't a type, then consider it a struct.
        introspectStruct(i)
    }
  }

  private[this] val analysisStackThreadLocal = new ThreadLocal[HashMap[Seq[Class[_]], NodeDef]] {
    override protected def initialValue = new HashMap[Seq[Class[_]], NodeDef]
  }

  protected def introspectStruct(i: TypeInsight): NodeDef = {
    //Search the analysis stack for an instance of this typeInfo. If the stack already contains a node
    //for this typeInfo, it means it is a recursive structure, and we should return that reference.
    val analysisStack = analysisStackThreadLocal.get
    val linearizedManifest = manifestToClassSeq(i.typeInfo)
    analysisStack.get(linearizedManifest) foreach { n => return NodeRef(i.nodeName, n) }

    val adapter = if (i.adapters.isEmpty) None else i.adapters.dequeue
    val res = adapter match {
      case a @ Some(adapter) =>
        val res = SNode(i.nodeName, i.typeInfo, a, i.fieldProxy, i.lengthDescriptorSize)
        analysisStack(linearizedManifest) = res
        introspect(TypeInsight("adaptedValue", adapter.toTypeInfo)) match {
          case fork: collection.mutable.Buffer[_] => res ++= fork
          case leaf => res += leaf
        }
        res
      case _ =>
        //before analyzing its structure, it must prove to be serializable.
        if (!i.typeInfo.erasure.isAnnotationPresent(classOf[Serializable]))
          throw new IllegalArgumentException("Class " + i.typeInfo + " is not annotated Serializable")
        // validate that the struct has a no-arg constructor
        val noArgConstructor = i.typeInfo.erasure.getDeclaredConstructors() find (_.getParameterTypes().length == 0) getOrElse
          (throw new IllegalArgumentException("Class " + i.typeInfo.erasure.getName + " has no no-arg constructor."))
        noArgConstructor.setAccessible(true) // will need it

        val res = SNode(i.nodeName, i.typeInfo, None, i.fieldProxy, i.lengthDescriptorSize)
        analysisStack(linearizedManifest) = res

        /* TODO: annotations on methods defined in traits are not inherited, so defining
         * a getter and setter from a trait to be implemented with a var for example, won't
         * work as one would expect when you annotate it with SField. It would be nice if we
         * could build a model of the methods of the class, and find weather those are being
         * implemented on behalf of some interface, so that we could go grab the annotations
         * from the interface.
         * 
         * As a side node, maybe it is ok if we always force the class to declare what it will implement?
         * Further analysis required if it is ever going to be implemented.
         */
        val fields = (i.typeInfo.erasure.getFields() ++ i.typeInfo.erasure.getDeclaredFields() toSet: Set[Field]).collect(sfieldOnly)
        val methods = (i.typeInfo.erasure.getMethods() ++ i.typeInfo.erasure.getDeclaredMethods() toSet: Set[Method]).collect(sfieldOnly)
        val allFields = fields.toSeq ++ methods.toSeq
        val nodes = allFields.sortBy(_._1.order) map {
          case (settings, f: Field) =>
            f.setAccessible(true)
            introspect(TypeInsight(f.getName, ReflectionUtilities.calculateManifest(settings.hookType.getOrElse(f.getGenericType)),
              Set.empty, settings.adapters, Some(sourceProvider.fieldSource(f)), settings.lengthDescriptorSize))
          case (settings, m: Method) =>
            introspect(TypeInsight(m.getName, ReflectionUtilities.calculateManifest(settings.hookType.getOrElse(m.getGenericReturnType)),
              Set.empty, settings.adapters, Some(sourceProvider.fieldProxy(m)), settings.lengthDescriptorSize))
        }

        res ++= nodes
        res
    }
    analysisStack.remove(linearizedManifest)
    res
  }

  protected def manifestToClassSeq(m: OptManifest[_]): Seq[Class[_]] = m match {
    case m: ClassManifest[_] =>
      var res = Vector[Class[_]](m.erasure)
      if (m.typeArguments.nonEmpty) res ++= (m.typeArguments flatMap manifestToClassSeq)
      res
    case _ => Vector.empty
  }
}