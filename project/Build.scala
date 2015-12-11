
import sbt._
import Keys._
import Process._

object DemoBuild extends Build {

  // We can make an SBT plugin out of these. However, for the time being this is good enough.
  val generatorMode = SettingKey[Boolean]("generator-mode",
    "Is the compiler used for generating the deep embedding")

  def embed = Command.command("embed") { state =>
    val cleaned = Project.runTask(Keys.clean in Compile, state)
    cleaned match {
      case Some((state, _)) =>
        Project.evaluateTask(Keys.compile in Compile,
          (Project extract state).append(Seq(generatorMode := true), state))
        Project.evaluateTask(Keys.clean in Compile, state)
        state
      case None =>
        state
    }
  }

  def generatorSettings: Seq[Setting[_]] = Seq(
    libraryDependencies += "ch.epfl.lamp" % "yy-generator_2.11" % "0.1-SNAPSHOT",
    generatorMode := false,
    scalacOptions ++= {
      if(generatorMode.value) {
        val cpath = update.value.matching(configurationFilter()).classpath
        val plugin = cpath.files.find(_.getName contains "yy-generator").get.absString
        val yy_core = cpath.files.find(_.getName contains "yy-core").get.absString
        val yy = cpath.files.find(_.getName contains "yin-yang").get.absString
        Seq(
          s"-Xplugin:$plugin:$yy_core:$yy",
          "-Ystop-after:backend-generator"
        )
      } else
        Seq()
    },
    commands += embed
  )


}


