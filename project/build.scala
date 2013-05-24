import sbt._
import Keys._
import de.johoop.jacoco4sbt._
import JacocoPlugin._
import com.typesafe.sbt.SbtStartScript

object DaffodilBuild extends Build {

  var s = Defaults.defaultSettings
  lazy val nopub = Seq(publish := {}, publishLocal := {})

  // daffodil projects
  lazy val root    = Project(id = "daffodil", base = file("."), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .configs(CliTest)
                             .aggregate(propgen, lib, libUnit, core, coreUnit, tdml, tdmlUnit, cli, cliUnit, test)

  lazy val propgen = Project(id = "daffodil-propgen", base = file("daffodil-propgen"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)

  lazy val lib     = Project(id = "daffodil-lib", base = file("daffodil-lib"), settings = s ++ propgenSettings)
                             .configs(DebugTest)
                             .configs(NewTest)
                             
  lazy val libUnit     = Project(id = "daffodil-lib-unittest", base = file("daffodil-lib-unittest"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(lib)

  lazy val core    = Project(id = "daffodil-core", base = file("daffodil-core"), settings = s ++ startScriptSettings)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(lib)
                             
  lazy val coreUnit    = Project(id = "daffodil-core-unittest", base = file("daffodil-core-unittest"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(tdml)          
                             
  lazy val tdml    = Project(id = "daffodil-tdml", base = file("daffodil-tdml"), settings = s)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(core)
                             
  lazy val tdmlUnit    = Project(id = "daffodil-tdml-unittest", base = file("daffodil-tdml-unittest"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(tdml)  
                             
  lazy val cli    = Project(id = "daffodil-cli", base = file("daffodil-cli"), settings = s ++ startScriptSettings)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(tdml)
                             
  lazy val cliUnit    = Project(id = "daffodil-cli-unittest", base = file("daffodil-cli-unittest"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(cli)                               

  lazy val test    = Project(id = "daffodil-test", base = file("daffodil-test"), settings = s ++ stageTaskSettings ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .configs(CliTest)
                             .dependsOn(cli)

  lazy val perf    = Project(id = "daffodil-perf", base = file("daffodil-perf"), settings = s ++ nopub)
                             .configs(DebugTest)
                             .configs(NewTest)
                             .dependsOn(tdml)

  //set up 'sbt stage' as a dependency
  //TODO: find a way to clean this up and reduce repetition
  lazy val testTask = Keys.test in CliTest
  lazy val testOnlyTask = Keys.testOnly in CliTest
  lazy val testQuickTask = Keys.testQuick in CliTest
  
  lazy val testTaskNew = Keys.test in NewTest
  lazy val testOnlyTaskNew = Keys.testOnly in NewTest
  lazy val testQuickTaskNew = Keys.testQuick in NewTest
  
  lazy val testTaskDebug = Keys.test in DebugTest
  lazy val testOnlyTaskDebug = Keys.testOnly in DebugTest
  lazy val testQuickTaskDebug = Keys.testQuick in DebugTest

  lazy val stageTask = SbtStartScript.stage in Compile in core

  lazy val stageTaskSettings = Seq(
    //cli test tasks
    (testTask <<= testTask.dependsOn(stageTask)),
    (testOnlyTask <<= testOnlyTask.dependsOn(stageTask)),
    (testQuickTask <<= testQuickTask.dependsOn(stageTask)),
    //new test tasks
    (testTaskNew <<= testTaskNew.dependsOn(stageTask)),
    (testOnlyTaskNew <<= testOnlyTaskNew.dependsOn(stageTask)),
    (testQuickTaskNew <<= testQuickTaskNew.dependsOn(stageTask)),
    //debug test tasks
    (testTaskDebug <<= testTaskDebug.dependsOn(stageTask)),
    (testOnlyTaskDebug <<= testOnlyTaskDebug.dependsOn(stageTask)),
    (testQuickTaskDebug <<= testQuickTaskDebug.dependsOn(stageTask)),
    //cli, new, and debug tasks
    (debugTask <<= debugTask.dependsOn(stageTask)),
    (newTask <<= newTask.dependsOn(stageTask)),
    (cliTask <<= cliTask.dependsOn(stageTask))
  )

  val propertyGenerator = TaskKey[Seq[File]]("gen-props", "Generate properties scala source")
  lazy val propgenSettings = Seq(
    sourceGenerators in Compile <+= (propertyGenerator in Compile),
    propertyGenerator in Compile <<=
      (sourceManaged in Compile, dependencyClasspath in Runtime in propgen) map {
        (outdir, cp) => runPropertyGenerator(outdir, cp.files)
      }
  )

  def runPropertyGenerator(outdir: File, cp: Seq[File]): Seq[File] = {
    val mainClass = "edu.illinois.ncsa.daffodil.propGen.PropertyGenerator"
    val out = new java.io.ByteArrayOutputStream()
    val ret = new Fork.ForkScala(mainClass).fork(None, Nil, cp, Seq(outdir.toString), None, false, CustomOutput(out)).exitValue()
    if (ret != 0) {
      sys.error("Failed to generate code")
    }
    val in = new java.io.InputStreamReader(new java.io.ByteArrayInputStream(out.toByteArray))
    val bin = new java.io.BufferedReader(in)
    val iterator = Iterator.continually(bin.readLine()).takeWhile(_ != null)
    val files = iterator.map(f => new File(f)).toList
    files
  }

  // modify the managed source directories so that any generated code can be more easily included in IDE's
  s ++= Seq(sourceManaged <<= baseDirectory(_ / "src_managed"))

  // creates 'sbt debug:*' tasks, using src/test/scala-debug as the source directory
  lazy val DebugTest = config("debug") extend(Runtime)
  lazy val debugSettings: Seq[Setting[_]] = inConfig(DebugTest)(Defaults.testSettings ++ Seq(
    sourceDirectory <<= baseDirectory(_ / "src" / "test"),
    scalaSource <<= sourceDirectory(_ / "scala-debug"),
    javaSource <<= sourceDirectory(_ / "java-debug"),
    exportJars := false,
    publishArtifact := false
  ))
  s ++= Seq(debugSettings : _*)

  // creates 'sbt debug' task, which is essentially an alias for 'sbt debug:test'
  lazy val debugTask = TaskKey[Unit]("debug", "Executes all debug tests")
  lazy val debugTaskSettings = debugTask <<= (executeTests in DebugTest, streams in DebugTest, resolvedScoped in DebugTest, state in DebugTest) map {
    (results, s, scoped, state) => {
      val display = Project.showContextKey(state)
      Tests.showResults(s.log, results, "No tests to run for " + display(scoped))
    }
  }
  s ++= Seq(debugTaskSettings)

  // creates 'sbt new:*' tasks, using src/test/scala-new as the source directory
  lazy val NewTest = config("new") extend(Runtime)
  lazy val newSettings: Seq[Setting[_]] = inConfig(NewTest)(Defaults.testSettings ++ Seq(
    sourceDirectory <<= baseDirectory(_ / "src" / "test"),
    scalaSource <<= sourceDirectory(_ / "scala-new"),
    javaSource <<= sourceDirectory(_ / "java-new"),
    exportJars := false,
    publishArtifact := false

  ))
  s ++= Seq(newSettings : _*)

  // creates 'sbt new' task, which is essentially an alias for 'sbt new:test'
  lazy val newTask = TaskKey[Unit]("new", "Executes all new tests")
  lazy val newTaskSettings = newTask <<= (executeTests in NewTest, streams in NewTest, resolvedScoped in NewTest, state in NewTest) map {
    (results, s, scoped, state) => {
      val display = Project.showContextKey(state)
      Tests.showResults(s.log, results, "No tests to run for " + display(scoped))
    }
  }
  s ++= Seq(newTaskSettings)

  // add scala-new as a source test directory for the 'sbt test' commands
  lazy val buildNewWithTestSettings = unmanagedSourceDirectories in Test <++= baseDirectory { base =>
    Seq(base / "src/test/scala-new")
  }
  s ++= Seq(buildNewWithTestSettings)
  
  // creates 'sbt cli:*' tasks, using src/test/scala-cli as the source directory
  lazy val CliTest = config("cli") extend(Runtime)
  lazy val cliSettings: Seq[Setting[_]] = inConfig(CliTest)(Defaults.testSettings ++ Seq(
    sourceDirectory <<= baseDirectory(_ / "src" / "test"),
    scalaSource <<= sourceDirectory(_ / "scala-cli"),
    javaSource <<= sourceDirectory(_ / "java-cli"),
    exportJars := false,
    publishArtifact := false

  ))
  s ++= Seq(cliSettings : _*)

  // creates 'sbt cli' task, which is essentially an alias for 'sbt cli:test'
  lazy val cliTask = TaskKey[Unit]("cli", "Executes all CLI tests")
  lazy val cliTaskSettings = cliTask <<= (executeTests in CliTest, streams in CliTest, resolvedScoped in CliTest, state in CliTest) map {
    (results, s, scoped, state) => {
      val display = Project.showContextKey(state)
      Tests.showResults(s.log, results, "No tests to run for " + display(scoped))
    }
  }
  s ++= Seq(cliTaskSettings)

  // jacoco configuration
  s ++= Seq(jacoco.settings : _*)

  // start-script configuration
  lazy val startScriptSettings = Seq(SbtStartScript.startScriptForJarSettings : _*) ++
                                 Seq(mainClass in Compile := Some("edu.illinois.ncsa.daffodil.Main"))

  // get the version from the latest tag
  s ++= Seq(version := {
    val r = java.lang.Runtime.getRuntime()
    val p = r.exec("git describe HEAD")
    p.waitFor()
    val ret = p.exitValue()
    if (ret != 0) {
      sys.error("Failed to get daffodil version")
    }
    val b = new java.io.BufferedReader(new java.io.InputStreamReader(p.getInputStream))
    val version = b.readLine()
    val parts = version.split("-")
    val res =
      if (parts.length == 1) {
        parts(0)
      } else {
        parts(0) + "-SNAPSHOT"
      }
    res
  })

  def gitShortHash(): String = {
    val r = java.lang.Runtime.getRuntime()
    val p = r.exec("git rev-parse --short HEAD")
    p.waitFor()
    val ret = p.exitValue()
    if (ret != 0) {
      sys.error("Failed to get git hash")
    }
    val b = new java.io.BufferedReader(new java.io.InputStreamReader(p.getInputStream))
    val line = b.readLine()
    line
  }


  // update the manifest version to include the git hash
  lazy val manifestVersion = packageOptions in (Compile, packageBin) <++= version map { v => {
    val parts = v.split("-")
    val version =
      if (parts.length == 1) {
        "%s-%s".format(parts(0), gitShortHash)
      } else {
        "%s-%s [SNAPSHOT]".format(parts(0), gitShortHash)
      }
    Seq(
      Package.ManifestAttributes(java.util.jar.Attributes.Name.IMPLEMENTATION_VERSION -> version),
      Package.ManifestAttributes(java.util.jar.Attributes.Name.SPECIFICATION_VERSION -> version)
    )
  }}
  s ++= manifestVersion
}
