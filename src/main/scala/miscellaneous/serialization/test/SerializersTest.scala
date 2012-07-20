package miscellaneous.serialization
package test
import scala.annotation.target.field

object SerializersTest {

  object WeekDay extends Enumeration {
    type WeekDay = Value
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }
  
  object WeekDay2 extends Enumeration {
    type WeekDay2 = Value
    val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value
  }

  @Serializable
  case class Group(users: Seq[AllTypes] = Seq.empty){
    private def this() = this(null)
  }
  @Serializable
  case class SubType(@(SField @field)(0) text: String) {
    private def this() = this(null)
  }
  @Serializable
  case class AllTypes(@(SField @field)(0) byte: Byte,
                      @(SField @field)(1) short: Short,
                      @(SField @field)(2) int: Int,
                      @(SField @field)(3) long: Long,
                      @(SField @field)(4) float: Float,
                      @(SField @field)(5) double: Double,
                      @(SField @field)(6) boolean: Boolean,
                      @(SField @field)(7) char: Char,
                      @(SField @field)(8) string: String,
                      @(SField @field)(9) subType: SubType,
                      @(SField @field)(10) javaList: java.util.List[SubType],
                      @(SField @field)(11) javaMap: java.util.Map[String, String],
                      @(SField @field)(12) scalaTraversable: collection.mutable.HashSet[String],
                      @(SField @field)(13) scalaMap: collection.immutable.HashMap[String, String],
                      @(SField @field)(14) date: java.util.Date,
                      @(SField @field)(15) cal: java.util.Calendar,
                      @(SField @field)(16) specializedByteArray: Array[Byte],
                      @(SField @field)(17) simpleArray: Array[String],
                      @(SField @field)(18) weekDay: WeekDay.WeekDay,
                      @(SField @field)(19) weekDay2: WeekDay2.WeekDay2,
                      @(SField @field)(20) tuple2: (String, String),
                      @(SField @field)(value=21, `type`=classOf[scala.Tuple2$mcII$sp]) rawTuple2: (Int, Int)) {
    private def this() = this(0, 0, 0, 0, 0, 0, false, '0', null, null, null, null, null, null, null, null, null, null,
        WeekDay.Mon, WeekDay2.Mon, null, null)
  }
}

object JsonSerializerTest extends App {
  import SerializersTest._
  import scala.collection.JavaConversions._
  
  val s = new serializers.JsonSerializer(new Introspector with Memoization)
  val baos = new java.io.ByteArrayOutputStream(150)
  val orig = AllTypes(200.asInstanceOf[Byte], 1000, Int.MaxValue, Long.MaxValue, 34.532f, Double.MaxValue,
      true, 'ó', "Crazy string with \"weird\" utf-8 characters: Áéíóú", SubType("subtype here"),
      Seq(SubType("firstElem"), SubType("secondElem")),
      Map("attr1"->"awesome", "attr2"->"cool"),
      collection.mutable.HashSet("elem1", "elem2"),
      collection.immutable.HashMap("0"->"zero", "1"->"one", "2"->"two"),
      new java.util.Date,
      java.util.Calendar.getInstance,
      Array(2,3,4,5,6,7,2,4,3),
      Array("arr1", "arr2", "arr3"),
      WeekDay.Fri,
      WeekDay2.Fri,
      ("puto", "puteco"),
      (5, 10))
      
  s.write(orig, baos)
  print("Writen: ")
  baos.writeTo(sys.process.stdout)
  println
  val res = s.read[AllTypes](new java.io.ByteArrayInputStream(baos.toByteArray))
  println("Orig: " + orig)
  println("Res: " + res)
}

object BinarySerializerTest extends App {
  import SerializersTest._
  import scala.collection.JavaConversions._
  
  val s = new serializers.BinarySerializer(new Introspector with Memoization) {
    override def read(node: NodeDef, target: Option[AnyRef], in: java.io.InputStream): Any = {
      val res = super.read(node, target, in)
      res match {
        case someArr: Array[_] => println(node + " = " + someArr.mkString("[", ", ", "]"))
        case _ => println(node + " = " + res) 
      }
      res
    }
  }
  val baos = new java.io.ByteArrayOutputStream(150)
  val orig = AllTypes(200.asInstanceOf[Byte], 1000, Int.MaxValue, Long.MaxValue, 34.532f, Double.MaxValue,
      true, 'ó', "Crazy string with \"weird\" utf-8 characters: Áéíóú", SubType("subtype here"),
      Seq(SubType("firstElem"), SubType("secondElem")),
      Map("attr1"->"awesome", "attr2"->"cool"),
      collection.mutable.HashSet("elem1", "elem2"),
      collection.immutable.HashMap("0"->"zero", "1"->"one", "2"->"two"),
      new java.util.Date,
      java.util.Calendar.getInstance,
      "Pelada cool".getBytes,
      Array("arr1", "arr2", "arr3"),
      WeekDay.Fri,
      WeekDay2.Fri,
      ("puto", "puteco"),
      (5, 10))
      
  s.write(orig, baos)
  print(Console.BLUE + "Writen: ")
  for (byte <- baos.toByteArray) println(byte + ": " + byte.toChar)
  println(Console.RESET)
  val res = s.read[AllTypes](new java.io.ByteArrayInputStream(baos.toByteArray))
  println("Orig: " + orig)
  println("Res: " + res)
  println("Total bytes: " + baos.toByteArray.length)
}