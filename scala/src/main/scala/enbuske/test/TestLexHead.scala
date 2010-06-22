package enbuske.test

import enbuske.parse._
import enbuske.util.LexicalHeads

import java.io.{BufferedWriter,FileWriter,File}

object TestLexHead {
  def main(args : Array[String]) : Unit = {
    val pcfg = new DirectlyEstimatedPCFG()
    val data = pcfg.read(args(0))
    val (syms,terms) = pcfg.size
    println("PCFG created with " + syms + " symbols and " + terms + " terminals")
    pcfg.process(data)
    pcfg.setLock(true)

    val testT = data(1)

    val headT = new ParseTree(testT.root) with LexicalHeads

    println(PCFGPrinter.treeToString(pcfg,testT))

    headT.setHeads(pcfg)

    headT.nonterminals.map(n => {
      val headPT = headT.getHead(n)
      val hOff = headT.nonterminals.indexOf(headPT) - headT.nonterminals.indexOf(n)
      println(hOff)
      println(PCFGPrinter.nodeString(pcfg,n) + " ------- " + 
              PCFGPrinter.nodeString(pcfg,headPT))

    })
    

    1
  }
}
