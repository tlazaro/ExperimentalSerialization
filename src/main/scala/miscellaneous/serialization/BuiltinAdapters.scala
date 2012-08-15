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
  
  class SerializableAdapterTest extends Adapter[java.io.Serializable, Array[java.lang.Integer]] {
    override def marshall(obj: java.io.Serializable) = {
      (1 to 10).toArray map (new java.lang.Integer(_)) toArray
    }

    override def unmarshall(e: Array[java.lang.Integer]) = {
      new java.util.Random(e(0)+0)
    }
  }
}