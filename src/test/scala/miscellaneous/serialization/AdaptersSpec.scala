package miscellaneous.serialization

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import scala.annotation.target._
import java.util.Random
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.File

@Serializable
class ExtraInfo(@(SField @field)(0) val when: Long, @(SField @field)(1) val description: String) {
  private[this] def this() = this(0, null) //compat with serialization api
}

class URLAdapter extends Adapter[java.net.URL, ExtraInfo] {
  def marshall(u: java.net.URL) = new ExtraInfo(System.currentTimeMillis(), u.toString)
  def unmarshall(e: ExtraInfo) = new java.net.URL(e.description)
}

object AdaptersSpec {
  import BuiltinAdapters._

  @Serializable
  class ArraysMock(
    @(SField @field)(0) val strings: Array[String],
    @(SField @field)(1) val booleans: Array[Boolean],
    @(SField @field)(2) val bytes: Array[Byte],
    @(SField @field)(3) val chars: Array[Char],
    @(SField @field)(4) val shorts: Array[Short],
    @(SField @field)(5) val ints: Array[Int],
    @(SField @field)(6) val longs: Array[Long],
    @(SField @field)(7) val floats: Array[Float],
    @(SField @field)(8) val doubles: Array[Double]) {

    private def this() = this(null, null, null, null, null, null, null, null, null)

    override def toString() = {
      List(strings, booleans, bytes, chars, shorts, ints, longs, floats, doubles).foldLeft("ArraysMock:")(
        (acc, list) => acc + "\n(" + list.mkString(", ") + ")")
    }

    override def equals(obj: Any): Boolean = {
      if (obj == null || !obj.isInstanceOf[ArraysMock]) {
        return false
      }
      val o = obj.asInstanceOf[ArraysMock]
      strings.deep == o.strings.deep && booleans.deep == o.booleans.deep && bytes.deep == o.bytes.deep &&
        chars.deep == o.chars.deep && shorts.deep == o.shorts.deep && ints.deep == o.ints.deep &&
        longs.deep == o.longs.deep && floats.deep == o.floats.deep && doubles.deep == o.doubles.deep
    }
  }

  @Serializable
  class BasicObjects(
    @(SField @field)(0) val seed: Long,
    @(SField @field)(value = 1, adapters = Array(classOf[URLAdapter])) val url: java.net.URL,
    @(SField @field)(value = 2, adapters = Array(classOf[SerializableAdapterTest])) val rng: Random) {

    private def this() = this(0, null, null)
    override def toString() = "BasicObjects: " + url + " " + rng

    override def equals(obj: Any): Boolean = {
      if (obj == null || !obj.isInstanceOf[BasicObjects]) {
        return false
      }
      val o = obj.asInstanceOf[BasicObjects]
      //      rng == o.rng
      true
    }
  }
}

class AdaptersSpec extends FlatSpec with ShouldMatchers {
  import AdaptersSpec._

  val j = new serializers.JsonSerializer(new Introspector with Memoization with SpecializationSupport)
  val b = new serializers.BinarySerializer(new Introspector with Memoization with SpecializationSupport)
  val testDir = new java.io.File("target/test/AdaptersSpec")
  testDir.mkdirs()

  def writeFile(obj: AnyRef, name: String, s: Serializer): FileInputStream = {
    val file = new File(testDir, name)
    s.write(obj, new FileOutputStream(file))
    new FileInputStream(file)
  }

  "An Adapter" should "work for arrays" in {
    val orig = new ArraysMock(
      Array("hello", "goodbye"),
      Array(true, false),
      Array(0x0A.toByte, 0x0C.toByte),
      Array('a', 'b'),
      Array(1234.toShort, 4321.toShort),
      Array(123456, 123456),
      Array(1234567L, 1234567L),
      Array(123456.7f, 123456.7f),
      Array(123456.78, 123456.78))

    val res = j.read[ArraysMock](writeFile(orig, "ArraysMock.json", j))
    orig should equal(res)

    val res2 = b.read[ArraysMock](writeFile(orig, "ArraysMock.dat", b))
    orig should equal(res2)
  }

  it should "pretty much work" in {
    val orig = new BasicObjects(12345L, new java.net.URL("http://www.google.com"), new Random(12345L))

    val res = j.read[BasicObjects](writeFile(orig, "BasicObjects.json", j))
    orig should equal(res)

    val res2 = b.read[BasicObjects](writeFile(orig, "BasicObjects.dat", b))
    orig should equal(res2)
  }
}
