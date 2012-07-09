package miscellaneous
package serialization

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.Queue
import scala.collection.JavaConversions._

trait Memoization extends Introspector {
  lazy val memo = new ConcurrentHashMap[Seq[Class[_]], NodeDef]
  
  abstract override def introspectStruct(nodeName: String, typeInfo: ClassManifest[_],
      adapters: Queue[Option[Adapter[_, _]]], fieldProxy: Option[FieldProxy], lengthDescriptorSize: Int): NodeDef = {
    val flattenTypeInfo = manifestToClassSeq(typeInfo)
    memo.get(flattenTypeInfo) match {
      case null =>
	    val res = super.introspectStruct(nodeName, typeInfo, adapters, fieldProxy, lengthDescriptorSize)
	    memo.putIfAbsent(flattenTypeInfo, res)
	    res
      case node => NodeRef(nodeName, node) 
    }
  }
}
