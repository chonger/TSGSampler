package enbuske.programs

import parse._
import tsg._
import util.PTClassifier

object PTClass {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val data = pcfg.read(args(0))

    val ptc = new PTClassifier(pcfg)

    val triData = ptc.transform(data,pcfg)
    val revData = ptc.revert(triData,pcfg)

    revData zip data foreach( _ match {
      case (a,b) => if(a != b) {
        println(PCFGPrinter.treeToString(pcfg,a))
        println(PCFGPrinter.treeToString(pcfg,b))
        throw new Exception()
      }
    })

    data.slice(0,1).foreach(d => {
      println(PCFGPrinter.treeToString(pcfg,d))
    })
    triData.slice(0,1).foreach(d => {
      println(PCFGPrinter.treeToString(pcfg,d))
    })
    revData.slice(0,1).foreach(d => {
      println(PCFGPrinter.treeToString(pcfg,d))
    })


    pcfg.write(args(1),triData)
  }
}
