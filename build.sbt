version := "0.1"

organization := "ch.epfl"

name  := "query"

scalaVersion := "2.11.1"

libraryDependencies += "ch.epfl.lamp" % "yy-core_2.11" % "0.1-SNAPSHOT"

libraryDependencies += "ch.epfl.lamp" % "yin-yang_2.11" % "0.1-SNAPSHOT"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test")

libraryDependencies += "EPFL" % "lms_2.11" % "0.3-SNAPSHOT"

libraryDependencies += "ch.epfl.lamp" % "yy-generator_2.11" % "0.1-SNAPSHOT"

scalariformSettings

generatorSettings
