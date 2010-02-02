import sbt._

class SimpolProject(info: ProjectInfo) extends DefaultProject(info) {
  // For other configuration options, see
  // http://code.google.com/p/simple-build-tool/wiki/BuildConfiguration
  override def artifactID            = "simpol"
  override def dependencyPath        = "project" / "lib"
  override def managedDependencyPath = "project" / "lib_managed"
  override def mainScalaSourcePath   = "src"
  override def testScalaSourcePath   = "test"
  override def compileOptions        = Deprecation   ::
                                       Optimize ::
                                       super.compileOptions.toList
  override def packageOptions        = MainClass("simpol.Main") ::
                                       super.packageOptions.toList

  val scalaToolsRepository           = "Scala-tools Maven2 Repository" at
                                       "http://scala-tools.org/repo-releases"
  val lousycoderRepository           = "LousyCoder Maven Repository" at
                                       "http://maven.lousycoder.com"
  val junit = "junit"     % "junit" % "4.4"   % "test"
  val specs = "org.specs" % "specs" % "1.4.3" % "test"
}
