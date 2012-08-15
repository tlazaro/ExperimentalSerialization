package miscellaneous.serialization

import java.io.{ ObjectOutputStream, ObjectInputStream, ByteArrayInputStream }

object BuiltinAdapters {
  class SerializableAdapter extends Adapter[java.io.Serializable, Array[Byte]] {
    override def marshall(obj: java.io.Serializable) = {
      val baos = new java.io.ByteArrayOutputStream(100)
      val oos = new ObjectOutputStream(baos)
      oos.writeObject(obj)
      oos.close()
      baos.toByteArray
    }

    override def unmarshall(e: Array[Byte]) = {
      val ois = new ObjectInputStream(new ByteArrayInputStream(e))
      val obj = ois.readObject()
      obj.asInstanceOf[java.io.Serializable]
    }
  }
}