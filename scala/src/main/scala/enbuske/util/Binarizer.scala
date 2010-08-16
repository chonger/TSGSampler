package enbuske.util

import parse._

/**
 *
  *  Performs head out trinarization
  *
  */ 

object Trinarizer {

    def transform(dataI : List[ParseTree], pcfg : PCFG) : List[ParseTree] = {
	  
      val data = dataI.map(t => new ParseTree(t.root) with LexicalHeads)
      data.foreach(_.setHeads(pcfg))

      def isBar(nt : NonTerminalNode) : Boolean = pcfg.symbolStrings(nt.symbol).indexOf('@') >= 0
      
      def makeBar(n : ParseTypes.Symbol, left : Boolean) : ParseTypes.Symbol = {
	    var s = pcfg.symbolStrings(n)
	    if(s.indexOf('@') < 0 && s.indexOf('+') < 0) {//not yet a bar
	      if(left)
            s = "@" + s
          else
            s = "+" + s
        }
	    pcfg.addSymbol(s)
      }

      var i = -1
	  data.map(tree => {
        def recTri(n : NonTerminalNode,lr : Option[Boolean]) : NonTerminalNode = {
          n match {
	        case pt : PreTerminalNode => pt
            case nt : InternalNode => {
    	      val kids = nt.children
    	      val sym = nt.symbol
    	      if(kids.length == 1) {
    		    
                ProtoNode(sym,kids.map(a => recTri(a,None)))
    	      
              } else {
                var headInd = 0 //leftmost by default
                lr match {
                  case Some(ls) => {
                    if(ls)
                      headInd = kids.length - 1
                  }
                  case None => {
                    var ind = 0
                    kids.foreach(k => {
                      if(tree.getHead(k) eq tree.getHead(nt))
                        headInd = ind
                      ind += 1
                    })
                  }
                }                

                val fst = kids.slice(0,headInd)
                val khead = recTri(kids(headInd),None)
                val lst = kids.drop(headInd + 1)
                
                var k1 : List[NonTerminalNode] = Nil
                if(fst.length > 0)
                  k1 = List(recTri(new ProtoNode(makeBar(sym,true),fst),Some(true)))
                var k2 : List[NonTerminalNode] = Nil
                if(lst.length > 0)
                  k2 = List(recTri(new ProtoNode(makeBar(sym,false),lst),Some(false)))
                                 
                val newKids = k1 ::: List(khead) ::: k2

                ProtoNode(sym,newKids)
              }
            }
          }
        }

		try {
		  i = i + 1
		  new ParseTree(recTri(tree.root,None))
		} catch {
		  case e : Exception => {
		    println("Failed to Trinarize tree " + i)
            throw e
		  }
		}
	  })
	}
 
  def revert(data : List[ParseTree], pcfg : PCFG) : List[ParseTree] = {

      def isBarSymbol(nt : NonTerminalNode) : Boolean = (pcfg.symbolStrings(nt.symbol).indexOf('@') >= 0 ||
                                                         pcfg.symbolStrings(nt.symbol).indexOf('+') >= 0)

      def rec2(n : NonTerminalNode) : List[NonTerminalNode] = {
        n match {
          case in : InternalNode => {
            in.children.flatMap(c => {
              if(isBarSymbol(c)) {
                rec2(c)
              } else {
                List(recUTri(c))
              }
            })
          }
        }
      }

      def recUTri(n : NonTerminalNode) : NonTerminalNode = {
        n match {
	        case pt : PreTerminalNode => pt
            case nt : InternalNode => {
    	      val kids = nt.children
    	      val sym = nt.symbol
    	      
              val newKids : List[NonTerminalNode] = kids.flatMap(k => {
                if(isBarSymbol(k)) {
                  k match {
                    case in : InternalNode => rec2(in)
                  }
                } else {
                  List(recUTri(k))
                }
              })

              ProtoNode(sym,newKids)
            }
        }
      }

	  var i = -1
	  //data.foreach(d => println(printTree(d.tree)))
	  data.map(d => {
		try {
		  i = i + 1
		  new ParseTree(recUTri(d.root)) //1d.tree.root = recUnTri(d.tree.root)
		} catch {
		  case e : Exception => {
		    println("Failed to Reverse Trinarize tree " + i)
            println(PCFGPrinter.treeToString(pcfg,d))
            throw e
		    
		    //  printStackTrace
		  }
		}
	  })
	}
  

 
  

}
