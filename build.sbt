import sbt.Project.projectToRef
import play.sbt.PlayImport.PlayKeys._

lazy val clients = Seq(exampleClient)
lazy val scalaV = "2.11.8"

//resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

lazy val exampleMultitier = (crossProject.crossType(CrossType.Pure) in file("example-multitier")).
  settings(
    scalaVersion := scalaV,
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "de.tuda.stg" %%% "retier-core" % "0+",
      "de.tuda.stg" %%% "retier-architectures-basic" % "0+",
      "de.tuda.stg" %%% "retier-serializable-upickle" % "0+",
      "de.tuda.stg" %%% "retier-network-ws-akka-play" % "0+",
      "de.tuda.stg" %%% "retier-transmitter-rescala" % "0+",
      "com.lihaoyi" %%% "scalarx" % "0.2.8",
      "be.doeraene" %%%! "scalajs-jquery" % "0.9.0",
      "com.lihaoyi" %%%! "scalatags" % "0.6.0",
      "org.scala-js" %%%! "scalajs-dom" % "0.9.0",
      "com.typesafe.play" %% "play" % "2.5.4"
    )
  )

lazy val exampleMultitierJvm = exampleMultitier.jvm
lazy val exampleMultitierJs = exampleMultitier.js

lazy val exampleServer = (project in file("example-server")).settings(
  scalaVersion := scalaV,
  routesImport += "config.Routes._",
  routesGenerator := StaticRoutesGenerator,
  scalaJSProjects := clients,
  pipelineStages := Seq(scalaJSProd, gzip),
  resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
  libraryDependencies ++= Seq(
    filters,
    jdbc,
    evolutions,
    "com.michaelpollmeier" %% "gremlin-scala" % "3.1.0-incubating",
    "org.neo4j" % "neo4j-tinkerpop-api-impl" % "0.1-2.2",
    "com.typesafe.play" %% "anorm" % "2.5.0",
    "com.vmunier" %% "play-scalajs-scripts" % "0.3.0",
    "com.typesafe.slick" %% "slick" % "3.1.1",
    "com.typesafe.play" %% "play-slick" % "2.0.2",
    "com.lihaoyi" %% "upickle" % "0.4.1",
    "org.webjars" %% "webjars-play" % "2.4.0",
    "org.webjars" % "bootstrap" % "3.3.5",
    "org.webjars" % "jquery" % "2.1.4",
    "org.webjars" % "font-awesome" % "4.4.0",
    "com.lihaoyi" %% "utest" % "0.3.0" % "test"
  )
 ).enablePlugins(PlayScala).
  aggregate(clients.map(projectToRef): _*).
  dependsOn(exampleSharedJvm, exampleMultitierJvm)

lazy val exampleClient = (project in file("example-client")).settings(
  scalaVersion := scalaV,
  persistLauncher := true,
  persistLauncher in Test := false,
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.0",
    "com.lihaoyi" %%% "scalatags" % "0.6.0",
    "com.lihaoyi" %%% "scalarx" % "0.2.8",
    "be.doeraene" %%% "scalajs-jquery" % "0.9.0",
    "com.lihaoyi" %%% "upickle" % "0.4.1",
    "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
  )
).enablePlugins(ScalaJSPlugin, ScalaJSPlay).
  dependsOn(exampleSharedJs, exampleMultitierJs)

val exampleSharedJvmSettings = List(
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "upickle" % "0.4.1",
    "com.lihaoyi" %% "utest" % "0.3.0" % "test"
  )
)

val exampleSharedForIDE = (project in file("example-shared")).settings(
  (scalaVersion := scalaV) +:
  (testFrameworks += new TestFramework("utest.runner.Framework")) +:
  exampleSharedJvmSettings :_*)

val exampleShared = (crossProject.crossType(CrossType.Pure) in file("example-shared")).
  settings(
    scalaVersion := scalaV,
    testFrameworks += new TestFramework("utest.runner.Framework")
  ).
  jvmSettings(exampleSharedJvmSettings: _*).
  jsSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % "0.4.1",
      "com.lihaoyi" %%% "utest" % "0.3.0" % "test"
    )
  )

lazy val exampleSharedJvm = exampleShared.jvm
lazy val exampleSharedJs = exampleShared.js

// loads the jvm project at sbt startup
onLoad in Global := (Command.process("project exampleServer", _: State)) compose (onLoad in Global).value

// for Eclipse users
EclipseKeys.skipParents in ThisBuild := false
// Compile the project before generating Eclipse files, so that generated .scala or .class files for views and routes are present
EclipseKeys.preTasks := Seq(compile in (exampleServer, Compile))