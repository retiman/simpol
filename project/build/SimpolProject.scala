import sbt._

class SimpolProject(info: ProjectInfo) extends DefaultProject(info) {
  override def artifactID          = "simpol"
  override def outputPath          = "build"
  override def mainScalaSourcePath = "src"
  override def testScalaSourcePath = "test"
  override def compileOptions      = Deprecation ::
                                     Verbose ::
                                     Optimize ::
                                     super.compileOptions.toList
  override def packageOptions      = MainClass("simpol.Main") ::
                                     super.packageOptions.toList

  val junit                        = "junit" % "junit" % "4.4" % "test"
  val specs                        = "specs" % "specs" % "1.4.1" % "test"
}
