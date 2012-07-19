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
  val javaList: java.util.List[String] = null
  @SField(6)
  val labels: Vector[String] = Vector.empty
  @SField(7)
  var bookmarks: Map[String, ExtraInfo] = Map.empty

  //evil recursion here!
  @SField(8)
  val listsOfTestClasses: Vector[InstrospectionTestClass] = null
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

  println("Not warmed up vm with memo: " + bench(10000, new Introspector with Memoization))
  println("Warmed up vm with memo: " + bench(10000, new Introspector with Memoization))
  println("Not warmed up vm without memo: " + bench(10000, new Introspector))
  println("Warmed up vm without memo: " + bench(10000, new Introspector))
}

object SerializationBenchNoMemoFirst extends App with SerializationBench {

  println("Not warmed up vm without memo: " + bench(10000, new Introspector))
  println("Warmed up vm without memo: " + bench(10000, new Introspector))
  println("Not warmed up vm with memo: " + bench(10000, new Introspector with Memoization))
  println("Warmed up vm with memo: " + bench(10000, new Introspector with Memoization))
}

object SerializationBenchMemoization extends App with SerializationBench {
  println("Not warmed up vm with memo: " + bench(10000, new Introspector with Memoization))
  println("Warmed up vm with memo: " + bench(10000, new Introspector with Memoization))
}

object BinarySerializerBench extends App {
  import SerializersTest._
  import scala.collection.JavaConversions._
  val s = new serializers.BinarySerializer(new Introspector with Memoization)
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
    ("puto", "puteco"),
    (5, 10))

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

  val intr = new Introspector with Memoization
  val res = intr.introspect[InstrospectionTestClass]("")
  //  SerializationTreeForSwing.show(res)
}
