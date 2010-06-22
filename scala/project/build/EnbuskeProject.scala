import sbt._

class EnbuskeProject(info: ProjectInfo) extends DefaultProject(info)
{

  val dataDir = "/home/chonger/data/"

  val optns = Nil
  override def mainClass = Some("enbuske.programs.PackTSG")
  lazy val threadrun = runTask(mainClass,runClasspath,optns) dependsOn(compile)
  lazy val normalpack = runTask(Some("enbuske.programs.PackTSG"),runClasspath,
                               List(dataDir + "fft.unk.txt",dataDir + "fft.pack")
                             ) dependsOn(compile)

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

  lazy val unpack = runTask(Some("enbuske.programs.UnpackToText"),runClasspath, 
                            List(dataDir + "fft.unk.txt",
                                 dataDir + "fft.cpack",
                                 dataDir + "fft.segTEST")) dependsOn(compile)



  lazy val getP = runTask(Some("enbuske.programs.elit.GetPhrases"),runClasspath,
                          List(dataDir + "train.txt",dataDir + "yields")) dependsOn(compile)

  lazy val fft = {
    val trees = dataDir + "fft.unk.txt"
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
  


  def distPath = (
    ((outputPath ##) / defaultJarName) +++
    //mainDependencies.scalaJars +++
    mainResources
    //descendents(info.projectPath / "lib" ##, "*.jar")
  )

  override def manifestClassPath = {
    
    val ret = Some(
      distPath.getFiles
      .filter(_.getName.endsWith(".jar"))
    .map(_.getName).mkString(" ")
    )
    println(ret)
    ret

  }

  override def fork = Some(new ForkScalaRun{override def runJVMOptions = Seq("-Xmx1500M")})


  lazy val testLexHeads = {
    val trees = dataDir + "24.unk.txt"
    val optns = List(trees)
    runTask(Some("enbuske.test.TestLexHead"),runClasspath,optns) dependsOn(compile)
  }

}
