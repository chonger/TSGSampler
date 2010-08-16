package enbuske.programs

import java.io.File

object Enbuske {

  def main(args : Array[String]) : Unit = {
    println()
    println("Enbuske - The tree that bends but never breaks")
    println()
    
    if(args.length < 2 || args.length > 3) {
      usage() 
    }

    var phase = 0
    if(args.length == 3) {
      phase = args(2) match {
        case "CLEAN" => 1
        case "PARSE" => 2
        case "LEARN" => 3
      }
    }

    //TODO? - check argument validity
    val base = args(0)
    val infile = args(1)
    
    if(phase == 0 || phase == 1) {
      println("<CLEAN---------------------------------------->")
      PrepNParse.prepText(base + infile,base + "split." + infile)
      if(phase == 1) {
        return
      }
    }

    if(phase == 0 || phase == 2) {
      println("<-------------PARSE--------------------------->")
      PrepNParse.parseText(base + "split." + infile,base + "parsed." + infile)
      if(phase == 2)
        return
    }
    
    if(phase == 0 || phase == 3) {
      println("<--------------------------LEARN-------------->")
      
      PackTSG.main(Array(base + "parsed." + infile,base + "spack." + infile))
      
      println()
      println("   This may take days...                    ")
      println()
      val runArr = Array("EnbuskeSampler/bin/tsgtest",
                         base + "spack." + infile,
                         base + "cpack." + infile,
                         base + "logl." + infile,
                         base + "schedule.txt")

      var p = Runtime.getRuntime().exec(runArr)
      var oStr = p.getInputStream()
      var printLen = 0
      var count = 1
      val maxLen = 42
      val letrs = List("E","N","B","U","S","K","E")
      val rando = util.Util.javaRandom

      def printOSTR() = {
        var av = oStr.available()
        if(av > 0) {
          var chrs = for{i <- 1 to av} yield {
            oStr.read().toChar
          }
          var s = ("" /: chrs)(_ + _)
          s = s.replaceAll("Log","#")
          s = s.replaceAll("[^#]","")
          s = ("" /: s.map(x => letrs(rando.nextInt(letrs.length))))(_ + _)
          while(printLen + s.length() > maxLen) {
            print(s.substring(0,maxLen - printLen))
            println(" " + count * maxLen)
            count += 1
            s = s.substring(maxLen - printLen)
            printLen = 0
          }
          printLen += s.length()
          print(s)
        }
      }

      var go = true
      while(go) {
        try {
          p.exitValue()
          printOSTR
          go = false
        } catch {
          case e : IllegalThreadStateException => {
            printOSTR
          }
        }
      }
      println()
      println()
/**
      if (Runtime.getRuntime().
          exec(runArr).waitFor() != 0){ 
            throw new Exception("Execution failed")
          }
*/   
 
      UnpackToText.main(Array(base + "parsed." + infile,
                              base + "cpack." + infile,
                              base + "tsg." + infile))


      println("<-----------------------------------------DONE>")


      println()
      println()
      println("Enjoy your file - " + (base + "tsg." + infile))
    }

  }


  def usage() = {

    println("<<<=============-----------USAGE-----------=============>>>")
    println(" |                                                       | ")
    println(" |       Enbuske wants two strings...                    | ")
    println(" |                                                       | ")
    println(" |      The first is a home for your computation,        | ")
    println(" |   a directory where Enbuske can write itself notes    | ")
    println(" |                                                       | ")
    println(" |    The second is a text file of plaintext             | ")
    println(" |          which should obey these rules without fail   | ")
    println(" |                                                       | ")
    println(" |  RULE 1 - A line break implies a sentence boundary    | ")
    println(" |                  but not the other way 'round.        | ")
    println(" |                                                       | ")
    println(" |  RULE 2 - The text is close enough to English to      | ")
    println(" |             be considered English.                    | ")
    println(" |                                                       | ")
    println(" | Example : ./enbuske /home/chonger/data/ treebank.txt  | ")
    println(" |                                                       |  ")
    println("<<<=============-------------o-------------=============>>>")
    println(" |                                                       | ")
    println(" |   Enbuske can run its phases in isolation,            | ")
    println(" |                   but each requires previous          |")
    println(" |    execution of its predecessors                      |")
    println(" | The phases in order are : CLEAN, PARSE, LEARN         |")
    println(" |                                                       |") 
    println(" | i.e. ./enbuske /home/chonger/data/ treebank.txt PARSE |")
    println(" |                                                       |  ")
    println("<<<=============-------------o-------------=============>>>")
    println(" |                                                       |  ")
    println(" |  The Sampling Schedule is determined by schedule.txt  |  ")
    println(" |     put one next to your data text file               | ")
    println(" | Copy the provided schedule.txt there for starters     | ")
    println(" |                                                       | ")
    println("<<<=============-------------o-------------=============>>>")
    println(" |                                                       |   ")
    println(" |       Read PROTIPS file to become a power user.       |")
    println(" |                                                       |  ")
    println("<<<=============----------KTHXBYE----------=============>>>")
    System.exit(-1)
  }

}
