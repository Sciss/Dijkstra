name               := "Dijkstra"
organization       := "de.sciss"
version            := "0.1.0-SNAPSHOT"
scalaVersion       := "2.12.4"
crossScalaVersions := Seq("2.12.4", "2.11.12")
licenses           := Seq("Apache License 2.0" -> url("https://raw.githubusercontent.com/Sciss/Dijkstra/master/LICENSE.md"))

libraryDependencies += "org.specs2" %% "specs2-core" % "4.0.3" % "test"

scalacOptions in Test ++= Seq("-Yrangepos")

