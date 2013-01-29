package miscellaneous.serialization

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import scala.annotation.target.field
import scala.collection.immutable.HashMap

object WeekDay extends Enumeration {
  val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
}

@Serializable
class ClassWithEnumeration(@(SField @field)(0) val p: WeekDay.Value) {
  private def this() = this(null)
}

@Serializable
class ClassWithEnumerationList(@(SField @field)(0) val p: List[WeekDay.Value]) {
  private def this() = this(null)
}

@Serializable
class ClassWithEnumerationMap(
  @(SField @field)(0)
  @(specializedFor @field)(target = classOf[HashMap[_, _]], params = Array(classOf[WeekDay.Value], classOf[Int])) val p: HashMap[WeekDay.Value, Int]) {
  private def this() = this(null)
}

@Serializable
class ClassWithMap(@(SField @field)(0) val p: Map[String, String]) {
  private def this() = this(null)
}

class Introspection extends FlatSpec with ShouldMatchers {
  import AdaptersSpec._

  "An introspector" should "be able to introspect a class with a Map[String, String]" in {
    val introspector = new Introspector with SpecializationSupport
    info(NodeDef.treeDebugString(introspector.introspect[ClassWithMap]("root")))
  }
  it should "be able to introspect a class with an Enumeration" in {
    val introspector = new Introspector with SpecializationSupport
    info(NodeDef.treeDebugString(introspector.introspect[ClassWithEnumeration]("root")))
  }
  it should "be able to introspect a class with a List[Enumeration]" in {
    val introspector = new Introspector with SpecializationSupport
    info(NodeDef.treeDebugString(introspector.introspect[ClassWithEnumerationList]("root")))
  }
  it should "be able to introspect a class with a Map[Enumeration, Int]" in {
    val introspector = new Introspector with SpecializationSupport
    info(NodeDef.treeDebugString(introspector.introspect[ClassWithEnumerationMap]("root")))
  }

}