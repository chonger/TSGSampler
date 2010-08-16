package enbuske.programs

import parse._
import tsg._
import util.Trinarizer

object Trinarize {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val data = pcfg.read(args(0))
    val triData = Trinarizer.transform(data,pcfg)
    val revData = Trinarizer.revert(triData,pcfg)

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

    val bad = triData.filter(tree => {
      tree.nonterminals.length >= 255
    }).length
    println("gonna kill " + bad + " trees")

    pcfg.write(args(1),triData.filter(tree => {
      tree.nonterminals.length < 255
    }))
  }
}
