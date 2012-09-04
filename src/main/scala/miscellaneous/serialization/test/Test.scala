package miscellaneous.serialization
package test

import scala.annotation.target._

trait Base {

  def foo: Long
  def foo_=(l: Long)
  def getBar: String
  def setBar(s: String)
}

trait Base2 {
  @SField(100)
  var qux: String = _
}

@Serializable
class InstrospectionTestClass extends Base with Base2 {
  @SField(0)
  var bippy: String = _
  @SField(value = 1, `type` = classOf[Int])
  var foo: Long = _
  var bar: String = _
  @SField(2)
  def getBar = bar
  def setBar(s: String) = bar = s
  @SField(3)
  var extraInfo: ExtraInfo = _
  @SField(value = 4, adapters = Array(classOf[URLAdapter]))
  var url: java.net.URL = _
  @SField(5)
  val javaList: java.util.List[String] = new java.util.ArrayList[String]
  @SField(6)
  val labels: Vector[String] = Vector.empty
  @SField(7)
  var bookmarks: Map[String, ExtraInfo] = Map.empty

  //evil recursion here!
  @SField(8)
  val listsOfTestClasses = scala.collection.mutable.Buffer[InstrospectionTestClass]()
}

@Serializable
class ExtraInfo(@(SField @field)(0) val when: Long, @(SField @field)(1) val description: String) {
  private[this] def this() = this(0, null) //compat with serialization api
}

class URLAdapter extends Adapter[java.net.URL, ExtraInfo] {
  def marshall(u: java.net.URL) = new ExtraInfo(System.currentTimeMillis(), u.toString)
  def unmarshall(e: ExtraInfo) = new java.net.URL(e.description)
}

private[test] trait SerializationBench {
  def bench(times: Int, intr: Introspector) = {
    var i = -1
    val prev = System.currentTimeMillis()
    while ({ i += 1; i < times }) {
      val res = intr.introspect[InstrospectionTestClass]("")
    }
    val total = System.currentTimeMillis() - prev
    (total, total / times.toFloat)
  }
}

object SerializationBenchMemoFirst extends App with SerializationBench {

  println("Not warmed up vm with memo: " + bench(10000, new Introspector with Memoization with SpecializationSupport))
  println("Warmed up vm with memo: " + bench(10000, new Introspector with Memoization with SpecializationSupport))
  println("Not warmed up vm without memo: " + bench(10000, new Introspector with SpecializationSupport))
  println("Warmed up vm without memo: " + bench(10000, new Introspector with SpecializationSupport))
}

object SerializationBenchNoMemoFirst extends App with SerializationBench {

  println("Not warmed up vm without memo: " + bench(10000, new Introspector with SpecializationSupport))
  println("Warmed up vm without memo: " + bench(10000, new Introspector with SpecializationSupport))
  println("Not warmed up vm with memo: " + bench(10000, new Introspector with Memoization with SpecializationSupport))
  println("Warmed up vm with memo: " + bench(10000, new Introspector with Memoization with SpecializationSupport))
}

object SerializationBenchMemoization extends App with SerializationBench {
  println("Not warmed up vm with memo: " + bench(10000, new Introspector with Memoization with SpecializationSupport))
  println("Warmed up vm with memo: " + bench(10000, new Introspector with Memoization with SpecializationSupport))
}

object BinarySerializerBench extends App {
  import SerializersTest._
  import scala.collection.JavaConversions._
  val s = new serializers.BinarySerializer(new Introspector with Memoization with SpecializationSupport)
  val baos = new java.io.ByteArrayOutputStream(52 * 1024 * 1024)

  val obj = AllTypes(200.asInstanceOf[Byte], 1000, Int.MaxValue, Long.MaxValue, 34.532f, Double.MaxValue,
    true, 'ó', "Crazy string with \"weird\" utf-8 characters: Áéíóú", SubType("subtype here"),
    Seq(SubType("firstElem"), SubType("secondElem")),
    Map("attr1" -> "awesome", "attr2" -> "cool"),
    collection.mutable.HashSet("elem1", "elem2"),
    collection.immutable.HashMap("0" -> "zero", "1" -> "one", "2" -> "two"),
    new java.util.Date,
    java.util.Calendar.getInstance,
    "Pelada cool".getBytes,
    Array("arr1", "arr2", "arr3"),
    WeekDay.Fri,
    WeekDay2.Fri,
    (5, 10),
    (14, "yeah"))

  val times = 200000
    
  var i = -1
  var prev = System.currentTimeMillis()
  while ({ i += 1; i < times }) {
    s.write(obj, baos)
  }
  var total = System.currentTimeMillis() - prev
  println("Writing times: " + (total, total / times.toFloat))
  
  val in = new java.io.ByteArrayInputStream(baos.toByteArray())
  i = -1
  prev = System.currentTimeMillis()
  while ({ i += 1; i < times }) {
    s.read[AllTypes](in)
  }
  in.close()
  total = System.currentTimeMillis() - prev
  println("Reading times: " + (total, total / times.toFloat))
}

object SerializationTest extends App {
  import java.util.logging._
  val logger = Logger.getLogger("miscellaneous.serialization")
  logger.setLevel(Level.ALL)
  logger.addHandler {
    val res = new ConsoleHandler
    res.setLevel(Level.ALL)
    res
  }
 
  val testClass = new InstrospectionTestClass
  testClass.bippy = "bippy is setted"
  testClass.foo = 34
  testClass.bar = "bar is setted"
  testClass.extraInfo = new ExtraInfo(System.currentTimeMillis, "extraInfo is setted")
  testClass.url = new java.net.URL("http://this/is/da/url")
  testClass.javaList add "someElement"
  testClass.qux = "qux is now setted"
  
  val intr = new Introspector with Memoization
  val res = intr.introspect[InstrospectionTestClass]("testClass")
  println(NodeDef.treeDebugString(res))
  
  
  import scala.collection.JavaConversions._
  val s = new serializers.JsonSerializer(intr)
  val baos = new java.io.ByteArrayOutputStream(52 * 1024 * 1024)
  s.write(testClass, baos)
  baos.writeTo(System.out)
  println()
  val read = s.read[InstrospectionTestClass](new java.io.ByteArrayInputStream(baos.toByteArray))
  println("read: ")
  s.write(read, System.out)
  //  SerializationTreeForSwing.show(res)
}
