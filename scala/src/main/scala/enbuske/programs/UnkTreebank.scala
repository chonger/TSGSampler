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
    val unker =  new UnkFromPCFG(pcfg)
    val newdata = pcfg.read(args(1))
	val (unkbank,unkPCFG) = unker.unkData(newdata,pcfg)
	unkPCFG.write(args(2),unkbank)
  }
}
