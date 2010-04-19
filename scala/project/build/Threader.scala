import sbt._

class ThreaderProject(info: ProjectInfo) extends DefaultProject(info)
{

  val dataDir = "/home/chonger/data/"

  val optns = Nil
  override def mainClass = Some("parse.test.ThreadTSG")
  lazy val threadrun = runTask(mainClass,runClasspath,optns) dependsOn(compile)
  lazy val normalrun = runTask(Some("parse.test.TrainTSG"),runClasspath,
                               List(dataDir + "train.unk.txt",dataDir + "train.pack")
                             ) dependsOn(compile)
  lazy val unpack = runTask(Some("parse.test.Unpack"),runClasspath, 
                            List(dataDir + "fft.unk.txt",
                                 dataDir + "fft.trained.cpack",
                                 dataDir + "fft.train.seg")) dependsOn(compile)
  lazy val getP = runTask(Some("tools.GetPhrases"),runClasspath,List("/media/DATA/train.txt","data/yields")) dependsOn(compile)

  lazy val draw = runTask(Some("viz.FunctionDrawer"),runClasspath,List("data/yields.vp","data/out.png")) dependsOn(compile)
  lazy val sbDetect = runTask(Some("viz.SBDetector"),runClasspath,List("/media/DATA/train.txt","data/sbound")) dependsOn(compile)


  lazy val fft = {
    val trees = dataDir + "fft.unk.txt"
    val cpack = dataDir + "fft.trained.cpack"
    val toTag = dataDir + "22.unk.txt"
    val outfile = dataDir + "22FFT.txt"
    val optns = List(trees,cpack,toTag,outfile)
    runTask(Some("tools.FormFunctionTag"),runClasspath,optns) dependsOn(compile)
  }

  lazy val unk = runTask(Some("tools.UnkTreebank"),runClasspath,List(dataDir + "22.txt",dataDir + "22.unk.txt")) dependsOn(compile)
  


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
