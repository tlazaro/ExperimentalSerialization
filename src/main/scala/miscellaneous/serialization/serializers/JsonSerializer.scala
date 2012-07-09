package miscellaneous.serialization
package serializers

import org.json.simple._, parser.JSONParser

class JsonSerializer(val introspector: Introspector) extends Serializer {
  
  
  /* *****************
   * Reading logic   * 
   * *****************/
  
  private[this] val readingStackTL = new ThreadLocal[ReadingStack]
  
  override def read(node: NodeDef, target: Option[AnyRef], in: java.io.InputStream): Any = {
    try super.read(node, target, in)
    catch {case ex => 
      readingStackTL set null
      throw ex
    }
  }
  
  private[this] def notNull[T](key: String, in: java.io.InputStream): T = {
    readingStackTL.get match {
      case null => new JSONParser().parse(new java.io.InputStreamReader(in)).asInstanceOf[T]
      case stack =>
        val res = stack.get(key)
        if (res == null) throw new IllegalArgumentException("Field " + key + " not found in deserialized object: " + stack.current)
        res.asInstanceOf[T]   
    }
  }
  def readString(name: String, in: java.io.InputStream): String = notNull[String](name, in)
  def readChar(name: String, in: java.io.InputStream): Char = notNull[String](name, in)(0)
  def readBoolean(name: String, in: java.io.InputStream): Boolean = notNull[Boolean](name, in)
  def readDouble(name: String, in: java.io.InputStream): Double = notNull[Number](name, in).doubleValue()
  def readFloat(name: String, in: java.io.InputStream): Float = notNull[Number](name, in).floatValue()
  def readLong(name: String, in: java.io.InputStream): Long = notNull[Long](name, in)
  def readInt(name: String, in: java.io.InputStream): Int = notNull[Number](name, in).intValue()
  def readShort(name: String, in: java.io.InputStream): Short = notNull[Number](name, in).shortValue()
  def readByte(name: String, in: java.io.InputStream): Byte = notNull[Number](name, in).byteValue()
  def read(name: String, bytes: Array[Byte], in: java.io.InputStream): Array[Byte] = {
    var i = -1
    while ({i +=1; i < bytes.length}) bytes(i) = notNull[Long](name, in).toByte
    bytes
  }
  def readBlockStart(blockName: String, lengthDescriptorSize: Int, in: java.io.InputStream): Int = {
    val rs = readingStackTL.get
    def length(obj: Any) = obj match {case arr: JSONArray => arr.size(); case other => -1}
    if (rs == null) {
      val parser = new JSONParser
      val root = parser.parse(new java.io.InputStreamReader(in))
      readingStackTL.set(new ReadingStack(root.asInstanceOf[JSONAware with JSONStreamAware]))
      length(root)
    } else {
      rs.push(blockName)
      length(rs.current)
    }
  }
  def readBlockEnd(blockName: String, in: java.io.InputStream): Unit = {
    val rs = readingStackTL.get
    rs.pop()
    if (rs.current == null) readingStackTL set null
  }
  
  private class ReadingStack(root: JSONAware with JSONStreamAware) {
    type JsonObj = JSONAware with JSONStreamAware
    import collection.mutable.Stack
    val stack = new Stack[JsonObj]()
    var current: JsonObj = root
    
    def get(key: String): Any = {
      current match {
        case arr: JSONArray =>
          arr.remove(0)
        case obj: JSONObject =>
          obj.get(key)
      }
    }
    
    def push(name: String) {
      val prev = current
      current = get(name).asInstanceOf[JsonObj]
      if (current == null) throw new IllegalStateException("Field " + name + " not found")
      stack push prev
    }
    
    def pop() {
      if (stack.nonEmpty) current = stack.pop()
      else current = null
    }
  }
  
  
  /* *****************
   * Writing logic   * 
   * *****************/
  
  private[this] val writingStackTL = new ThreadLocal[WritingStack] {
    override protected def initialValue = new WritingStack
  }
  override def write(node: NodeDef, obj: Any, out: java.io.OutputStream) {
    try super.write(node, obj, out)
    catch {case ex => 
      writingStackTL set null
      throw ex
    }
  }
  
  def writeString(name: String, value: String, out: java.io.OutputStream): Unit = {
    writingStackTL.get.put(name, value, out)
  }
  def writeChar(name: String, value: Char, out: java.io.OutputStream): Unit = {
    writingStackTL.get.put(name, value.toString, out)
  } 
  def writeBoolean(name: String, value: Boolean, out: java.io.OutputStream): Unit = {
    writingStackTL.get.put(name, value, out)
  } 
  def writeDouble(name: String, value: Double, out: java.io.OutputStream): Unit = {
    writingStackTL.get.put(name, value, out)
  }
  def writeFloat(name: String, value: Float, out: java.io.OutputStream): Unit = {
    writingStackTL.get.put(name, value, out)
  } 
  def writeLong(name: String, value: Long, out: java.io.OutputStream): Unit = {
    writingStackTL.get.put(name, value, out)
  } 
  def writeInt(name: String, value: Int, out: java.io.OutputStream): Unit = {
    writingStackTL.get.put(name, value, out)
  } 
  def writeShort(name: String, value: Short, out: java.io.OutputStream): Unit = {
    writingStackTL.get.put(name, value, out)
  } 
  def writeByte(name: String, value: Byte, out: java.io.OutputStream): Unit = {
    writingStackTL.get.put(name, value, out)
  } 
  def write(name: String, bytes: Array[Byte], out: java.io.OutputStream): Unit = {
    var i = -1
    while ({i +=1; i < bytes.length}) writeByte(name, bytes(i), out)
  } 
  def writeBlockEnd(blockName: String, out: java.io.OutputStream): Unit = {
    val ws = writingStackTL.get
    val res = ws.pop()
    if (ws.stack.size == 0) {
      val bout = new java.io.OutputStreamWriter(out)
      res.writeJSONString(bout)
      bout.flush()
    }
  }
  def writeBlockStart(blockName: String, size: Int, lengthDescriptorSize: Int, out: java.io.OutputStream): Unit = {
    writingStackTL.get.push(blockName, lengthDescriptorSize > 0)
  }

  private class WritingStack {
    type JsonObj = JSONAware with JSONStreamAware
    import collection.mutable.Stack
    val stack = new Stack[JsonObj]()
    def current = stack.top
    
    def put(key: String, value: Any, out: java.io.OutputStream) {
      if (stack.nonEmpty) put(current, key, value)
      else {
        val obj = new JSONObject
        put(obj, key, value)
        obj.writeJSONString(new java.io.OutputStreamWriter(out))
      }
    }
    
    private[this] def put(target: JsonObj, key: String, value: Any) {
      target match {
        case arr: JSONArray => arr.asInstanceOf[java.util.List[Any]].add(value)
        case obj: JSONObject => obj.asInstanceOf[java.util.Map[String, Any]].put(key, value) 
      }
    }
    
    def push(name: String, array: Boolean) {
      val newTop = if (array) new JSONArray else new JSONObject()
      if (stack.nonEmpty) put(current, name, newTop)
      stack push newTop
    }
    
    def pop() = stack.pop()
  }
}