
enablePlugins(ScalaJSPlugin, WorkbenchPlugin)
scalaVersion := "2.11.7"
version := "0.3-SNAPSHOT"
name := "Sketcher"
resolvers += Resolver.jcenterRepo
resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-target:jvm-1.7",
  "-encoding", "UTF-8",
  "-language:_"
)
libraryDependencies ++= Seq(
  "com.github.yoeluk" %%% "paper-scala-js" % "0.5-SNAPSHOT",
  "com.lihaoyi" %%% "scalarx" % "0.2.8"
)
persistLauncher in Compile := false
persistLauncher in Test := false
skip in packageJSDependencies := false