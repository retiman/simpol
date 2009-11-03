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
}
