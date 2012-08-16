package miscellaneous.serialization

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import scala.annotation.target.field

@Serializable
class ClassWithTuples(@(SField @field)(0)@(specializedFor @field)(target=classOf[Tuple2[_, _]], params=Array(classOf[Int], classOf[Int])) val p: (Int, Int)) {
  private def this() = this(null)
}
@Serializable
class ClassWithListOfTuples(@(SField @field)(0)@(specializedFor @field)(target=classOf[Tuple2[_, _]], params=Array(classOf[Int], classOf[Int])) val p: List[(Int, Int)]) {
  private def this() = this(null)
}

class SpecializationSpec extends FlatSpec with ShouldMatchers {
  import AdaptersSpec._
  "An introspector without SpecializationSupport" should "not be able to introspect (Int, Int)" in {
    val introspector = new Introspector
    intercept[IntrospectionException] {
      info(NodeDef.treeDebugString(introspector.introspect[ClassWithTuples]("root")))
    }
  }
  "An introspector with SpecializationSupport" should "be able to introspect (Int, Int)" in {
    val introspector = new Introspector with SpecializationSupport
    println(NodeDef.treeDebugString(introspector.introspect[ClassWithTuples]("root")))
  }
  it should "be able to introspect List[(Int, Int)]" in {
    val introspector = new Introspector with SpecializationSupport
    println(NodeDef.treeDebugString(introspector.introspect[ClassWithListOfTuples]("root")))
    val jsonSerializer = new serializers.JsonSerializer(introspector)
    jsonSerializer.write(new ClassWithListOfTuples(List(1->4, 6->3)), System.out)
    println
  }
}