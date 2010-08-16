package enbuske.util

import parse._

class PTClassifier(pcfg : PCFG) {

  import scala.collection.mutable.HashMap

  var ptMap = new HashMap[ParseTypes.Symbol,HashMap[String,Int]]()

  val infiles = List("CDclusts.txt",
                     "NNPSclusts.txt",
                     "NNSclusts.txt",
                     "RBclusts.txt",
                     "VBGclusts.txt",
                     "VBPclusts.txt",
                     "VBclusts.txt",
                     "JJclusts.txt",
                     "NNPclusts.txt",
                     "NNclusts.txt",
                     "VBDclusts.txt",
                     "VBNclusts.txt",
                     "VBZclusts.txt")

  

  infiles.foreach(f => {
    
    val str = f.substring(0,f.indexOf('c'))
    val sym = pcfg.symbolIDs(str)
    val fname = "/home/chonger/Downloads/Results/" + f
    println(fname + " -- " + str)
    
    val lines = io.Source.fromFile(fname).getLines.toList

    println("got " + lines.length + " lines in " + f)
    
    var ind = 0
    var curMap = new HashMap[String,Int]()
    var state = 0
    
    lines.foreach(l => {
      
      val s = l.replaceAll("\\s*","")
      
      state match {
        case 0 => {
          state += 1 //burn one line
          ind += 1
        }
        case 1 => {
          if(s.length == 0)
            state = 0
          else {
            curMap += s -> ind
          }
        }
        
      }
    })
    
    ptMap += sym -> curMap
    
  })
  
  println("PTMAP SIZE = " + ptMap.size)
  println("SIZES")
  ptMap.foreach(m => {
    println(m._2.size)
  })
  
      


  def transform(data : List[ParseTree], pcfg : PCFG) : List[ParseTree] = {
    
    def recTr(n : NonTerminalNode) : NonTerminalNode = {
      n match {
        case pn : PreTerminalNode => {
          val term = pcfg.terminalStrings(pn.kid.terminal) 
          val str = pcfg.symbolStrings(pn.symbol)
          var newstr = str
          if(ptMap.isDefinedAt(pn.symbol)) {

            val map = ptMap(pn.symbol)
            if(map.isDefinedAt(term)) {
              val ind = map(term)

              newstr = str + "_" + ind
            }
          }
          new PreTerminalNode(pcfg.addSymbol(newstr),pn.kid)
        }
        case in : InternalNode => {
          new ProtoNode(in.symbol,in.children.map(c => recTr(c)))
        }
      }
    }

    data.map(tree => {
      new ParseTree(recTr(tree.root))
    })

  }
  
  def revert(data : List[ParseTree], pcfg : PCFG) : List[ParseTree] = {

    def recRev(n : NonTerminalNode) : NonTerminalNode = {
      n match {
        case pn : PreTerminalNode => {
          val str = pcfg.symbolStrings(pn.symbol)
          if(str.indexOf("_") > 0) {

            val newstr = str.replaceAll("_[0-9]*","")

            new PreTerminalNode(pcfg.addSymbol(newstr),pn.kid)
          } else {
            new PreTerminalNode(pn.symbol,pn.kid)
          }
        }
        case in : InternalNode => {
          new ProtoNode(in.symbol,in.children.map(c => recRev(c)))
        }
      }
    }

    data.map(tree => {
      new ParseTree(recRev(tree.root))
    })

  }


}
