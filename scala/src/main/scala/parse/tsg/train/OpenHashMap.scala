package parse.tsg.train

import scala.collection.mutable.{HashMap,DefaultEntry}

class OpenHashMap[A,B] extends HashMap[A,B] {
  def fetchInternal(a : A) : Option[DefaultEntry[A,B]] = {
    val e = findEntry(a)
    if(e == null) None
    else Some(e)
  }
}
