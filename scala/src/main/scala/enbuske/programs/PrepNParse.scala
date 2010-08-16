package enbuske.programs

import java.io.{File,FileWriter,BufferedWriter}
import java.text.{BreakIterator}
import java.util.{Locale}

object PrepNParse {
  
  def prepText(infile : String, outfile : String) : Unit = {

    var lines = io.Source.fromFile(infile).getLines

    //remove newline stuff
    println("The carriage returns absent, Enbuske proceeds,")
    lines = lines.map(_.replaceAll("\r",""))
    println("  removes newlines from new lines,")
    lines = lines.map(_.replaceAll("\n",""))
    println("     and separates parenthetical characters ")
    println("           from their neighbors,")
    lines = lines.map(_.replaceAll("\\(","( "))
    lines = lines.map(_.replaceAll("\\)"," )"))
    println()
    println(" Enbuske decides the start of sentences,")
    val bDec = BreakIterator.getSentenceInstance(Locale.US)

    val sentsL : List[List[String]] = lines.map(line => {
      var ret : List[String] = Nil
      if(line != "") {
        bDec.setText(line)
        var end = bDec.next()
        var start = bDec.first()
        while(end != BreakIterator.DONE) {
          if(start != end) {
            ret ::= line.substring(start,end)
          }
          start = end
          end = bDec.next()
        }
      }
      ret
    }).toList

    val sents = sentsL.flatMap(a => a)
    println("  " + sents.toList.length + " lines is the mass of its memory.")

    var bw = new BufferedWriter(new FileWriter(new File(outfile)))

    sents.foreach(s => {
      bw.write(s + "\n")
    })
    
    bw.close()  
  
  }

  def parseText(infile : String, outfile : String) : Unit = {

    /**
    var argStr = Array("-tokenize","-nThreads","2","-gr","eng_sm6.gr", 
                      "-inputFile",infile,"-outputFile",outfile)

    edu.berkeley.nlp.PCFGLA.BerkeleyParser.main(argStr)
    */
    
    println("Calling on the Berkeley Parser")
    
    if (Runtime.getRuntime().
        exec(Array("java","-Xmx1500M","-jar","berkeleyParser.jar",
                   "-tokenize","-gr","eng_sm6.gr", 
                   "-inputFile",infile,"-outputFile",outfile + "-BP")).waitFor() != 0){
          throw new Exception("Execution failed")
        }
    
 
    var lines : List[String] = io.Source.fromFile(outfile + "-BP").getLines.toList
    
    var bw = new BufferedWriter(new FileWriter(new File(outfile)))

    val origSz = lines.length
        
    lines = lines.filter(f => {
      !f.equals("(())\n")
    })

    val nowSz = lines.length
    println("Unable to parse " + (origSz - nowSz) + " sentences.");
    
    lines = lines.map(s => {
      s.replaceAll("^\\(","(ROOT")
    })

    val pcfg = new parse.DirectlyEstimatedPCFG()
    val linesNtrees = lines.map(l => (l,pcfg.growTree(l)))
    
    linesNtrees.foreach( _ match {
      case (line,tree) => {
        if(tree.nonterminals.length < 255)
          bw.write(line)
      }
    })

    bw.close()  
  }

}

