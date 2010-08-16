package enbuske.programs

import parse._
import tsg._
import util.StanfordNER

object StanNER {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val data = pcfg.read(args(0)).slice(0,1)
    
    val ner = new StanfordNER()

    val nerData = ner.transform(data,pcfg)


    data.slice(0,1).foreach(d => {
      println(PCFGPrinter.treeToString(pcfg,d))
    })
    nerData.slice(0,1).foreach(d => {
      println(PCFGPrinter.treeToString(pcfg,d))
    })


    pcfg.write(args(1),nerData)
  }
}
