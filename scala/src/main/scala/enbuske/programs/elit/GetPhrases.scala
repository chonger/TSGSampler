package enbuske.prorgams.elit

import parse._
import java.io.{BufferedWriter,FileWriter,File}
import scala.collection.mutable.HashMap

/**
 * an object that gets phrases out of trees
 *
 * in - filepath to some trees
 *
 * writes (arg2).np, (arg2).vp, and (arg2).adjp to current directory
 *
 * ELIT
 * 
 */ 

object GetPhrases {
  def main(args : Array[String]) : Unit = {
    val pcfg = new PCFG
	val data = pcfg.read(args(0))

    def getTerminalYield(nt : NonTerminalNode) : String = {
      val terminals = (new ParseTree(nt)).terminals.map(pcfg.getTerm(_))
      ("" /: terminals)(_ + " " + _)
    }


    val npMap = new HashMap[String,Int]()
    val vpMap = new HashMap[String,Int]()
    val adjMap = new HashMap[String,Int]()

    var ind = 0

    data.foreach(tree => {
      tree.nonterminals.foreach{nt => {
        ind += 1
        if(ind % 1000 == 0)
          println("Processed " + ind + " trees")
        val sym = pcfg.getSym(nt)

        def addYield(hm : HashMap[String,Int]) = {
          val termY = getTerminalYield(nt)
          val count = hm.getOrElse(termY,0)
          hm += termY -> (count + 1) 
        }

        sym match {
          case "NP" => {
            addYield(npMap)
          }
          case "VP" => {
            addYield(vpMap)
          }
          case "ADJP" => {
            addYield(adjMap)
          }
          case _ => {}
        }
      }
    }})

    def writeData(s : String, hm : HashMap[String,Int]) = {

      val fname = args(1) + "." + s
      println("writing " + s + " yields to " + fname)

      val bw = new BufferedWriter(
        new FileWriter(new File(fname)))
      val sorted = hm.elements.toList.sort((a,b) => {a._2 > b._2})
      sorted.foreach(el => {
        bw.write(el._2.toString + "\t" + el._1 + "\n")
      })
      bw.close()
    }

    writeData("np",npMap)
    writeData("vp",vpMap)
    writeData("adjp",adjMap)

  } 
}
