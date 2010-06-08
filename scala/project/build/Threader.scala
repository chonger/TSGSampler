import sbt._

class ThreaderProject(info: ProjectInfo) extends DefaultProject(info)
{

  val dataDir = "/home/chonger/data/"


  val optns = Nil
  override def mainClass = Some("parse.test.ThreadTSG")
  lazy val threadrun = runTask(mainClass,runClasspath,optns) dependsOn(compile)
  lazy val normalpack = runTask(Some("parse.test.TrainTSG"),runClasspath,
                               List(dataDir + "2.unk.txt",dataDir + "2.pack")
                             ) dependsOn(compile)

  lazy val toygen = runTask(Some("parse.generate.TSGGen"),runClasspath,
                               List(dataDir + "toygrammar.txt",dataDir + "toygrammar.gen","1000","50")
                             ) dependsOn(compile)

  lazy val toypack = runTask(Some("parse.test.TrainTSG"),runClasspath,
                               List(dataDir + "toygrammar.gen",dataDir + "toy.pack")
                             ) dependsOn(compile)
  lazy val toyunpack = runTask(Some("parse.test.Unpack"),runClasspath, 
                            List(dataDir + "toygrammar.gen",
                                 dataDir + "toy.cpack",
                                 dataDir + "toy.seg")) dependsOn(compile)

  lazy val unpack = runTask(Some("parse.test.Unpack"),runClasspath, 
                            List(dataDir + "2.unk.txt",
                                 dataDir + "2.cpack",
                                 dataDir + "2.seg")) dependsOn(compile)



  lazy val getP = runTask(Some("tools.GetPhrases"),runClasspath,List("/media/DATA/train.txt","data/yields")) dependsOn(compile)

  lazy val draw = runTask(Some("viz.FunctionDrawer"),runClasspath,List("data/yields.vp","data/out.png")) dependsOn(compile)
  lazy val sbDetect = runTask(Some("viz.SBDetector"),runClasspath,List("/media/DATA/train.txt","data/sbound")) dependsOn(compile)


  lazy val fft = {
    val trees = dataDir + "fft.unk.txt"
    val cpack = dataDir + "fft.trained.cpack"
    val toTag = dataDir + "24.unk.txt"
    val outfile = dataDir + "24FFT.txt"
    val optns = List(trees,cpack,toTag,outfile)
    runTask(Some("tools.FormFunctionTag"),runClasspath,optns) dependsOn(compile)
  }

  lazy val unkTB = runTask(Some("tools.UnkTreebank"),runClasspath,List(dataDir + "fft.unk.txt",dataDir + "24.txt",dataDir + "24.unk.txt")) dependsOn(compile)
  


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

}
