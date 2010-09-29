package enbuske.programs

import util._
import parse._

/**
 * reads trees T1 from args(0)
 * reads trees T2 from args(1)
 * 
 * unks all terminals in T2 which dont appear in T1
 *
 * writes to args(2)
 */ 

object UnkTreebank {
  def main(args : Array[String]) : Unit = {
    val pcfg = new PCFG
	val data = pcfg.read(args(0))	
    val unker =  new UnkFromData(data,pcfg)
    val newdata = pcfg.read(args(1))
	val (unkbank,unkPCFG) = unker.unkData(newdata,pcfg)
	unkPCFG.write(args(2),unkbank)
  }
}


object UnkLC {
  def main(args : Array[String]) : Unit = {
    var pcfg = new PCFG

    val h1 = args.slice(0,args.length/2)
    val h2 : List[String] = args.toList.drop(args.length/2)
    println(h1)
    println(h2)
	val data = h1.map(x => pcfg.read(x)).toList
    val emp : List[ParseTree] = Nil
    val allD = (emp /: data)(_ ::: _)
    val unker =  new UnkLeastCommon(5,allD,pcfg)
    data zip h2 map (_ match {
      case (trees,outName) => {
        val (unkbank,unkPCFG) = unker.unkData(trees,pcfg)
	    unkPCFG.write(outName,unkbank)
        pcfg = unkPCFG
      }
    })
	
  }
}
