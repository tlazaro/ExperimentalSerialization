package miscellaneous.serialization
package serializers

import java.io._

class BinarySerializer(val introspector: Introspector, bigEndian: Boolean = true) extends Serializer {

  def readBlockStart(blockName: String, lengthDescriptorSize: Int, in: java.io.InputStream): Int = {
    lengthDescriptorSize match {
      case -1 => -1 //on struct header do nothing
      case 1 => readByte(blockName, in)
      case 2 => readShort(blockName, in)
      case 4 => readInt(blockName, in)
      case _ =>
        throw new IllegalArgumentException("Requested lengthDescriptorSize = " + lengthDescriptorSize + ". Only 1, 2 or 4 are supported." +
          "Illegal block: " + blockName)
    }
  }
  def readBlockEnd(blockName: String, in: InputStream) {}

  def read(name: String, bytes: Array[Byte], in: InputStream): Array[Byte] = {
    var read = 0
    var count = 0
    while ({ read = in.read(bytes, count, bytes.length - count); read != -1 && count < bytes.length }) {
      count += read
    }
    if (count < bytes.length) throw new EOFException(
      "Not enough bytes reading " + name + ", desired " + bytes.length + " bytes, read before EOF: " + count + " bytes"
    )
    bytes
  }
  
  @inline
  private[this] def eof(i: Int) = if (i == -1) throw new EOFException else i
  
  def readByte(name: String, in: InputStream): Byte = eof(in.read()).toByte
  def readShort(name: String, in: InputStream): Short =
    if (bigEndian) (eof(in.read()) << 8) + eof(in.read()) toShort
    else eof(in.read()) + (eof(in.read()) << 8) toShort
  def readInt(name: String, in: InputStream): Int =
    if (bigEndian) (eof(in.read()) << 24) + (eof(in.read()) << 16) + (eof(in.read()) << 8) + eof(in.read())
    else eof(in.read()) + (eof(in.read()) << 8) + (eof(in.read()) << 16) + (eof(in.read()) << 24)
  def readLong(name: String, in: InputStream): Long =
    if (bigEndian) (eof(in.read()).toLong << 56) + (eof(in.read()).toLong << 48) + (eof(in.read()).toLong << 40) + (eof(in.read()).toLong << 32) + (eof(in.read()).toLong << 24) + (eof(in.read()).toLong << 16) + (eof(in.read()).toLong << 8) + eof(in.read())
    else eof(in.read()).toLong + (eof(in.read()).toLong << 8) + (eof(in.read()).toLong << 16) + (eof(in.read()).toLong << 24) + (eof(in.read()).toLong << 32) + (eof(in.read()).toLong << 40) + (eof(in.read()).toLong << 48) + (eof(in.read()).toLong << 56)
  def readFloat(name: String, in: InputStream): Float =
    java.lang.Float.intBitsToFloat(readInt(name, in))
  def readDouble(name: String, in: InputStream): Double =
    java.lang.Double.longBitsToDouble(readLong(name, in))
  def readBoolean(name: String, in: InputStream): Boolean = if (eof(in.read()) == 0) false else true
  def readChar(name: String, in: InputStream): Char = readShort(name, in).toChar
  def readString(name: String, in: InputStream): String = {
    val baos = new java.io.ByteArrayOutputStream
    var read = 0
    while ({ read = in.read(); read != 0 }) {
      if (read == -1) throw new EOFException("EOF while reading string " + name)
      baos.write(read)
    }
    new String(baos.toByteArray)
  }

  def writeBlockStart(blockName: String, size: Int, lengthDescriptorSize: Int, out: OutputStream) {
    lengthDescriptorSize match {
      case -1 => //on struct header do nothing
      case 1 => writeByte(blockName, size.toByte, out)
      case 2 => writeShort(blockName, size.toShort, out)
      case 4 => writeInt(blockName, size, out)
      case _ =>
        throw new IllegalArgumentException("Requested lengthDescriptorSize = " + lengthDescriptorSize + ". Only 1, 2 or 4 are supported." +
          "Illegal block: " + blockName)
    }
  }

  def writeBlockEnd(blockName: String, out: OutputStream) {}
  
  def write(name: String, value: Array[Byte], out: OutputStream) = out.write(value)
  def writeByte(name: String, value: Byte, out: OutputStream) = out.write(value)
  def writeShort(name: String, value: Short, out: OutputStream) = 
    if (bigEndian) {out.write(value >>> 8); out.write(value.toByte)}
    else {out.write(value.toByte); out.write(value >>> 8)}
  def writeInt(name: String, value: Int, out: OutputStream) = 
    if (bigEndian) {out.write(value >>> 24); out.write(value >>> 16); out.write(value >>> 8); out.write(value.toByte)}
    else {out.write(value.toByte); out.write(value >>> 8); out.write(value >>> 16); out.write(value >>> 24)}
  def writeLong(name: String, value: Long, out: OutputStream) = 
    if (bigEndian) {out.write(value >>> 56 toByte); out.write(value >>> 48 toByte); out.write(value >>> 40 toByte); out.write(value >>> 32 toByte); out.write(value >>> 24 toByte); out.write(value >>> 16 toByte); out.write(value >>> 8 toByte); out.write(value.toByte)}
    else {out.write(value.toByte); out.write(value >>> 8 toByte); out.write(value >>> 16 toByte); out.write(value >>> 24 toByte); out.write(value >>> 32 toByte); out.write(value >>> 40 toByte); out.write(value >>> 48 toByte); out.write(value >>> 56 toByte)}
  def writeFloat(name: String, value: Float, out: OutputStream) = writeInt(name, java.lang.Float.floatToRawIntBits(value), out)
  def writeDouble(name: String, value: Double, out: OutputStream) = writeLong(name, java.lang.Double.doubleToRawLongBits(value), out)
  def writeBoolean(name: String, value: Boolean, out: OutputStream) = out.write(if (value) 1 else 0)
  def writeChar(name: String, value: Char, out: OutputStream) = writeShort(name, value.toShort, out)
  def writeString(name: String, value: String, out: OutputStream) {
    write(name, value.getBytes, out)
    out.write(0)
  }
}