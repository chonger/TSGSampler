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
                                List(dataDir + "fft.trim.txt",dataDir + "fft.pack")
                              ) dependsOn(compile)
  
  lazy val unpack = runTask(Some("enbuske.programs.UnpackToText"),runClasspath, 
                            List(dataDir + "fft.trim.txt",
                                 dataDir + "fft.cpack",
                                 dataDir + "fft.seg")) dependsOn(compile)
  
  lazy val fft = {
    val trees = dataDir + "fft.trim.txt"
    val cpack = dataDir + "fft.cpack"
    val toTag = dataDir + "23.unk.txt"
    val outfile = dataDir + "23FFT.txt"
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

  lazy val testLexHeads = {
    val trees = dataDir + "24.unk.txt"
    val optns = List(trees)
    runTask(Some("enbuske.test.TestLexHead"),runClasspath,optns) dependsOn(compile)
  }

  lazy val toygen = runTask(Some("enbuske.programs.TSGGen"),runClasspath,
                            List(dataDir + "toygrammar.txt",dataDir + "toygrammar.gen","1000","50")
                          ) dependsOn(compile)
  
  lazy val toypack = runTask(Some("enbuske.programs.PackTSG"),runClasspath,
                             List(dataDir + "toygrammar.gen",dataDir + "toy.pack")
                           ) dependsOn(compile)
  
  lazy val toyunpack = runTask(Some("enbuske.programs.UnpackToText"),runClasspath, 
                               List(dataDir + "toygrammar.gen",
                                    dataDir + "toy.cpack",
                                    dataDir + "toy.seg")) dependsOn(compile)
  
  lazy val getP = runTask(Some("enbuske.programs.elit.GetPhrases"),runClasspath,
                          List(dataDir + "train.txt",dataDir + "yields")) dependsOn(compile)

}
