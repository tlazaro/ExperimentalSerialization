package miscellaneous
package serialization

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.Queue
import scala.collection.JavaConversions._

trait Memoization extends Introspector {
  lazy val memo = new ConcurrentHashMap[Seq[Class[_]], NodeDef]
  
  abstract override def introspectStruct(i: TypeInsight): NodeDef = {
    val flattenTypeInfo = manifestToClassSeq(i.typeInfo)
    memo.get(flattenTypeInfo) match {
      case null =>
	    val res = super.introspectStruct(i)
	    memo.putIfAbsent(flattenTypeInfo, res)
	    res
      case node => NodeRef(i.nodeName, node) 
    }
  }
}
