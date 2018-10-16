name               := "Dijkstra"
organization       := "de.sciss"
version            := "0.1.1"
scalaVersion       := "2.12.7"
crossScalaVersions := Seq("2.12.7", "2.11.12")
licenses           := Seq("Apache License 2.0" -> url("https://git.iem.at/sciss/Dijkstra/blob/master/LICENSE.md"))
homepage           := Some(url(s"https://git.iem.at/sciss/${name.value}"))
description        := "Dijkstra's algorithm for calculating shortest (least costly) route in a graph"
scalacOptions     ++= Seq("-Xlint", "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Yrangepos")

libraryDependencies += "org.specs2" %% "specs2-core" % "4.3.5" % Test

// ---- publishing ----

publishMavenStyle := true

publishTo :=
  Some(if (isSnapshot.value)
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := { val n = name.value
<scm>
  <url>git@git.iem.at:sciss/{n}.git</url>
  <connection>scm:git:git@git.iem.at:sciss/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>gseaton</id>
    <name>Greg Seaton</name>
    <url>https://github.com/gseaton</url>
  </developer>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
  <developer>
    <id>bepcyc</id>
    <name>Viacheslav Rodionov</name>
    <url>https://github.com/bepcyc</url>
  </developer>
</developers>
}

