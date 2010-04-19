package butil

object Util {
	def shuffle[A](data : Array[A]) {
		val rando = javaRandom
		for (n <- Iterator.range(data.length - 1,0,-1)) {
			val k = rando.nextInt(n + 1)
			val t = data(k);data(k) = data(n);data(n) = t
		}
	}
 
	def javaRandom = new java.util.Random(1234)
	def malletRandom = new cc.mallet.util.Randoms()
 
	import scala.collection.mutable.HashMap
	class AnalHashMap[A,B](hm : HashMap[A,B]) extends HashMap[A,B] {
	  hm.elements.foreach(a => this += a._1 -> a._2)
	  def analyze : Unit = {
		println("Size = " + size)
		val count = HashMap[Int,Int]()
		hm.keys.foreach(k => {
			val ind = index(k.hashCode)
			val nc = count.getOrElse(ind,0) + 1
			count += ind -> nc
		})
		val total = (0 /: count.elements)((a,b) => a + b._2)
		val avg = total.toDouble / count.size.toDouble
		println("total buckets = " + table.length)
		println("Num empty buckets = " + (table.length - count.size))
		println("Avg non empty bucket size = " + avg)
	  }
	}
 
	def analyzeHashMap[A,B](hm : HashMap[A,B]) : Unit = {
	  val analHM = new AnalHashMap(hm)
	  analHM.analyze
	}
 
}
