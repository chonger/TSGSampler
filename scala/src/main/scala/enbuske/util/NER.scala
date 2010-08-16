package enbuske.util 

import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.ie.AbstractSequenceClassifier
import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.ling.CoreLabel

import parse._

class StanfordNER {
  
  println("LOADING!")

  val serializedClassifier : String = "/home/chonger/Downloads/stanford-ner-2009-01-16/classifiers/ner-eng-ie.crf-3-all2008-distsim.ser.gz"
  val classifier : AbstractSequenceClassifier = CRFClassifier.getClassifierNoExceptions(serializedClassifier);

  def transform(data : List[ParseTree], pcfg : PCFG) : List[ParseTree] = {
    data.map(tree => {
      val sent = tree.terminals.map(t => {
        new CoreLabel(new Word(pcfg.terminalStrings(t.terminal)))
      })
      
      println("CLASS!")
      val out = classifier.classify(java.util.Arrays.asList(sent.toArray : _*))
      val tags = new collection.jcl.BufferWrapper[CoreLabel]() {
        def underlying = out
      }.toList
      println("DONE!")

      val tform = tags.map(t => {
        val label = t.ner()
        println(label)
        if (label == null) 
          t.word()
        else {
          label
        }
      })

      import scala.collection.mutable.HashMap
      val shmap = new HashMap[RefWrapper,String]()

      tree.nonterminals.filter(_.isInstanceOf[PreTerminalNode]) zip tform map(_ match {
        case (old,tag) => {
          shmap += new RefWrapper(old) -> tag
        }
      })
      
      def recT(n : NonTerminalNode) : NonTerminalNode = {
        n match { 
          case pt : PreTerminalNode => {
            val str = shmap(new RefWrapper(pt))
            PreTerminalNode(pt.symbol,TerminalNode(pcfg.addTerm(str)))
          }
          case in : InternalNode => {
            ProtoNode(in.symbol,in.children.map(a => recT(a)))
          }
        }
      }


      new ParseTree(recT(tree.root))
    })
  }


}
