package parse

import io.BytePickle._
import java.io.{FileOutputStream,DataInputStream,File,FileInputStream,DataOutputStream}
import java.io.ByteArrayInputStream
import collection.mutable.HashMap
import corpora.treebank.TreebankData

class PickleBasedPCFG extends PCFG {
	/**
	def this(filename : String) = {
	  this()
	  readPickledPCFG(filename)
	}
  
	def readPickledPCFG(filename : String) = {
		var stream = new DataInputStream(new FileInputStream(filename + ".ntrules"))
		rules.clear
		while(stream.available() > 0) {
			var dbl = stream.readDouble
			var arrsize = stream.readInt
			var barr = new Array[Byte](arrsize)
			stream.read(barr,0,arrsize);
			rules += (unpickle(Pickler.rulePickler,barr).asInstanceOf[NTRule] -> dbl)
		}
		stream.close
		println("!")
		stream = new DataInputStream(new FileInputStream(filename + ".lexrules"))
		lexiconRules.clear
		while(stream.available() > 0) {
			var dbl = stream.readDouble
			var arrsize = stream.readInt
			var barr = new Array[Byte](arrsize)
			stream.read(barr,0,arrsize);
			lexiconRules += (unpickle(Pickler.rulePickler,barr).asInstanceOf[TerminalRule] -> dbl)
		}
		stream.close
		println("!")
		initSymTerm()
		stream = new DataInputStream(new FileInputStream(filename + ".symbols"))
		while(stream.available() > 0) {
			var arrsize = stream.readInt
			var barr = new Array[Byte](arrsize)
			stream.read(barr,0,arrsize);
			addSymbol(unpickle(string,barr))
		}
		stream.close
  
		println("!")
		stream = new DataInputStream(new FileInputStream(filename + ".lexicon"))
		while(stream.available() > 0) {
			var arrsize = stream.readInt
			var barr = new Array[Byte](arrsize)
			stream.read(barr,0,arrsize);
			addTerm(unpickle(string,barr))
		}
		stream.close
		println("!")
		stream = new DataInputStream(new FileInputStream(filename + ".splits"))
		var pickledSplits = List[List[List[Tuple2[ParseTypes.Split,Int]]]]()
		while(stream.available() > 0) {
			var arrsize = stream.readInt
			var barr = new Array[Byte](arrsize)
			stream.read(barr,0,arrsize);
			pickledSplits ::= unpickle(Pickler.splitTreePickler,barr)
		}
		stream.close
		splitTrees = pickledSplits.reverse.toArray
		maxSplit = splitTrees(1).size - 1
		println("!")
	}
 */
}

object Pickler {
/**
  def save(pcfg : PCFG,filename : String) = {
    println("START")
    var stream = new DataOutputStream(new FileOutputStream(filename + ".ntrules"))
    pcfg.rules.foreach(_ match {case (rule,dbl) => {
    	var barr = pickle(rulePickler,rule) 
    	stream.write(pickle(doublePickler,dbl) ++ pickle(intPickler,barr.size) ++ barr)
    }})
    stream.close
    println("1")
    stream = new DataOutputStream(new FileOutputStream(filename + ".lexrules"))
    pcfg.lexiconRules.foreach(_ match {case (rule,dbl) => {
    	var barr = pickle(rulePickler,rule) 
    	stream.write(pickle(doublePickler,dbl) ++ pickle(intPickler,barr.size) ++ barr)
    }})
    stream.close
    println("2")
    stream = new DataOutputStream(new FileOutputStream(filename + ".symbols"))
    pcfg.symbolStrings.foreach(s => {
    	var barr = pickle(string,s) 
    	stream.write(pickle(intPickler,barr.size) ++ barr)
    })
    stream.close
    println("3")
    stream = new DataOutputStream(new FileOutputStream(filename + ".lexicon"))
    pcfg.terminalStrings.foreach(s => {
    	var barr = pickle(string,s)  
    	stream.write(pickle(intPickler,barr.size) ++ barr)
    })
    stream.close
    println("4")
    stream = new DataOutputStream(new FileOutputStream(filename + ".splits"))
    pcfg.splitTrees.foreach(s => {
    	var barr = pickle(splitTreePickler,s)
    	stream.write(pickle(intPickler,barr.size) ++ barr)
    })
    stream.close
    println("5")
  }  
  
  def bigListPickler[A](pickler : SPU[A], num : Int) : SPU[List[A]] = {
    wrap((s : List[List[A]]) => s.flatten(a => a), 
    	 (s : List[A]) => {
    		 var sp = s
    		 var ret = List[List[A]]()
    		 while(sp.length > num) {
    			 ret ::= sp.take(num)
    			 sp = sp.drop(num)
    		 }
    		 ret
         },
    	 list(list(pickler))
	  )
  }
  
  def splitTreePickler : SPU[List[List[Pair[ParseTypes.Split,Int]]]] = {
    	list(list(pair(splitPickler,nat)))
  }
  
  def stringListPickler : SPU[List[String]] = 
	  wrap((s : List[String]) => s, 
		   (s : List[String]) => s,
		   list(string)
	  )
  
  val DOUBLEBYTES : Int = 8
  val INTBYTES : Int = 4
  
  def doublePickler : SPU[Double] = new SPU[Double] {
    def appP(d : Double, s : PicklerState) : PicklerState = {
      new PicklerState(Array.concat(s.stream,doubleToBytes(d)),s.dict)
    }
    def appU(s : UnPicklerState) : (Double,UnPicklerState) = {
      (doubleFromBytes(s.stream.take(DOUBLEBYTES)),
       new UnPicklerState(s.stream.subArray(DOUBLEBYTES,s.stream.length),s.dict))
    }
  }
  
  def intPickler : SPU[Int] = new SPU[Int] {
    def appP(d : Int, s : PicklerState) : PicklerState = {
      new PicklerState(Array.concat(s.stream,intToBytes(d)),s.dict)
    }
    def appU(s : UnPicklerState) : (Int,UnPicklerState) = {
      (intFromBytes(s.stream.take(INTBYTES)),
       new UnPicklerState(s.stream.subArray(INTBYTES,s.stream.length),s.dict))
    }
  }
  
  def doubleToBytes(d : Double) = java.nio.ByteBuffer.allocate(DOUBLEBYTES).putDouble(d).array
  def doubleFromBytes(barr : Array[Byte]) = java.nio.ByteBuffer.wrap(barr).getDouble
  def intToBytes(d : Int) = java.nio.ByteBuffer.allocate(INTBYTES).putInt(d).array
  def intFromBytes(barr : Array[Byte]) = java.nio.ByteBuffer.wrap(barr).getInt
  
  def ruleMapPickler : SPU[HashMap[NTRule ,Double]] = 
    wrap((d : List[Tuple2[TreeRule,Double]]) => {
    		var ret = new HashMap[NTRule,Double]()
    		ret ++= d.map(_.asInstanceOf[(NTRule,Double)])
      		ret
    	 },
    	 (h : HashMap[NTRule,Double]) => {h.elements.toList},
    	 bigListPickler(pair(rulePickler,doublePickler),10000)
    )
  
  def lexMapPickler : SPU[HashMap[TerminalRule ,Double]] = 
    wrap((d : List[Tuple2[TreeRule,Double]]) => {
    		var ret = new HashMap[TerminalRule,Double]()
    		ret ++= d.map(_.asInstanceOf[(TerminalRule,Double)])
      		ret
    	 },
    	 (h : HashMap[TerminalRule,Double]) => h.elements.toList,
    	 list(pair(rulePickler,doublePickler))
    )
  
  def rulePickler : SPU[TreeRule] = 
    data((t : TreeRule) => t match {
      case u : UnaryRule => 0
      case b : BinaryRule => 1
      case t : TerminalRule => 2
    },
    List(() => unaryRulePickler, () => binaryRulePickler, () => terminalRulePickler))
    	
  
  def binaryRulePickler : SPU[TreeRule] = 
    wrap((t : Triple[ParseTypes.SSPair,ParseTypes.SSPair,ParseTypes.SSPair]) => BinaryRule(t._1._1,t._1._2,t._2,t._3),
         (r : TreeRule) => {r match {case BinaryRule(lhs,split,rhs1,rhs2) => Triple((lhs,split),rhs1,rhs2)}},
         triple(ssPairPickler,ssPairPickler,ssPairPickler)) 
 
  
  def terminalRulePickler : SPU[TreeRule] = 
	wrap((t : Pair[ParseTypes.SSPair,ParseTypes.Terminal]) => TerminalRule(t._1._1,t._1._2,t._2),
		 (r : TreeRule) => r match {case TerminalRule(lhs,split,term) => Pair((lhs,split),term)},
         pair(ssPairPickler,terminalPickler))      	
  
  
  def unaryRulePickler : SPU[TreeRule] = 
    wrap((t : Triple[ParseTypes.Symbol,ParseTypes.Split,ParseTypes.SSPair]) => UnaryRule(t._1,t._2,t._3),
         (r : TreeRule) => r match {case UnaryRule(lhs,split,rhs) => Triple(lhs,split,rhs)},
         triple(symbolPickler,splitPickler,ssPairPickler))      	
  
  def terminalPickler : SPU[ParseTypes.Terminal] = nat //assuming Terminal is Int
  def symbolPickler : SPU[ParseTypes.Symbol] = nat //assuming that Symbol is Int
  def splitPickler : SPU[ParseTypes.Split] = 
    wrap((s : String) => ParseTypes.parseSplit(s),
    	 (p : ParseTypes.Split) => {p.toInt.toString},
    	 string)

  def ssPairPickler : SPU[ParseTypes.SSPair] = 
    wrap((p : Pair[ParseTypes.Symbol,ParseTypes.Split]) => Tuple2(p._1,p._2),
    	 (ss : ParseTypes.SSPair) => {Pair(ss._1,ss._2)},
    	 pair(symbolPickler,splitPickler)
    	)
  */
}

object PicklerTest {
  def main(args : Array[String]) : Unit = {
/**  
	  var pcfg = new FileBasedPCFG with PrintablePCFG
	  print("Reading ... ")
	  pcfg.read("/data/trainedGrammar.dump")
	  println("done")
	  
    
	  print("Pickling ... ")
	  Pickler.save(pcfg,"/data/pickledGrammar")
	  println("done")
	 
	  print("Unpickling ... ")
	  var pcfg2 = new PickleBasedPCFG("/data/pickledGrammar") with PrintablePCFG
	  println("done")
	  
	  //pcfg2.rules.foreach(r => println(pcfg2.ruleString(r._1) + " -- " + r._2))
	  if(pcfg == pcfg2)
		  println("EQUAL")
	  else
		  println("NOT EQUAL")
	*/	
  }
}
