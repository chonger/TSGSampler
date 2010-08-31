import sbt._

class Env(working : String) {
  private val workDir = new java.io.File(working)
  def run(cmd : String*) = {
    if (Runtime.getRuntime().
        exec(cmd.toArray[String], null, workDir).waitFor() != 0)
      throw new Exception("Execution failed")
  }
}

class EnbuskeProject(info: ProjectInfo) extends DefaultProject(info)
{
  val dataDir = "/home/chonger/data/"

  val optns = Nil

  //the main class of the exported runnable JAR
  override def mainClass = Some("enbuske.programs.Enbuske")

  lazy val pack = runTask(Some("enbuske.programs.PackTSG"),runClasspath,
                                List(dataDir + "fft.unk.txt",dataDir + "fft.pack")
                              ) dependsOn(compile)
  
  lazy val pack2 = runTask(Some("enbuske.programs.Pack2TSG"),runClasspath,
                                List(dataDir + "train.unk.txt",
                                     dataDir + "train.cpack",
                                     dataDir + "airplane.txt",
                                     dataDir + "air.pack")
                              ) dependsOn(compile)

  lazy val unpack = runTask(Some("enbuske.programs.UnpackToText"),runClasspath, 
                            List(dataDir + "fft.trim.txt",
                                 dataDir + "fft10.cpack",
                                 dataDir + "fft.seg")) dependsOn(compile)
  
  lazy val fft = {
    val trees = dataDir + "fft.unk.txt"
    val cpack = dataDir + "fft-N1-10000.cpack"
    val toTag = dataDir + "24.unk.txt"
    val outfile = dataDir + "24FFT.txt"
    val optns = List(trees,cpack,toTag,outfile)
    runTask(Some("enbuske.programs.FormFunctionTag"),runClasspath,optns) dependsOn(compile)
  }

  
  lazy val unkTB = runTask(Some("enbuske.programs.UnkTreebank"),
                           runClasspath,List(dataDir + "fft.unk.txt",
                                             dataDir + "23.txt",
                                             dataDir + "23.unk.txt")) dependsOn(compile)
  
  /**
   *
   * NOT AS USEFUL
   *
   *
   **/

  lazy val trim = runTask(Some("enbuske.programs.TrimTags"),
                           runClasspath,List(dataDir + "fft.unk.txt",
                                             dataDir + "fft.trim.txt")) dependsOn(compile)

  lazy val trinarize = runTask(Some("enbuske.programs.Trinarize"),
                               runClasspath,List(dataDir + "fft.unk.txt",
                                                 dataDir + "fft.tri.txt")) dependsOn(compile)

  lazy val ptclass = runTask(Some("enbuske.programs.PTClass"),
                               runClasspath,List(dataDir + "fft.unk.txt",
                                                 dataDir + "fft.ptc.txt")) dependsOn(compile)

  lazy val ner = runTask(Some("enbuske.programs.StanNER"),
                               runClasspath,List(dataDir + "fft.txt",
                                                 dataDir + "fft.ner.txt")) dependsOn(compile)

  lazy val testLexHeads = {
    val trees = dataDir + "24.unk.txt"
    val optns = List(trees)
    runTask(Some("enbuske.test.TestLexHead"),runClasspath,optns) dependsOn(compile)
  }

  lazy val toygen = runTask(Some("enbuske.programs.TSGGen"),runClasspath,
                            List(dataDir + "toygrammar2.txt",dataDir + "toygrammar.spec","1000","50")
                          ) dependsOn(compile)
  
  lazy val toypack = runTask(Some("enbuske.programs.PackTSG"),runClasspath,
                                List(dataDir + "toygrammar.gen",dataDir + "toyGen.pack")
                              ) dependsOn(compile)
  
  lazy val toypack2 = runTask(Some("enbuske.programs.Pack2TSG"),runClasspath,
                                List(dataDir + "toygrammar.gen",
                                     dataDir + "toyGen.cpack",
                                     dataDir + "toygrammar.spec",
                                     dataDir + "toyDouble.pack")
                              ) dependsOn(compile)
  
  lazy val toyunpack = runTask(Some("enbuske.programs.UnpackToText"),runClasspath, 
                               List(dataDir + "toygrammar.gen",
                                    dataDir + "toyGen.cpack",
                                    dataDir + "toy.seg")) dependsOn(compile)
  
  lazy val getP = runTask(Some("enbuske.programs.elit.GetPhrases"),runClasspath,
                          List(dataDir + "train.txt",dataDir + "yields")) dependsOn(compile)

}
