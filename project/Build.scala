import sbt._
import Keys._
import sbtbuildinfo.Plugin._
import com.typesafe.sbteclipse.plugin.EclipsePlugin._

object Build extends sbt.Build {

  lazy val root =
    Project("giter8-lib", file("."))
      .settings(
        organization := "net.databinder.giter8",
        description := "giter8 library",
        version := "0.6.0-SNAPSHOT",
        scalaVersion := "2.10.1",
        libraryDependencies ++= Seq(
          "org.clapper" % "scalasti_2.10" % "1.0.0",
          "org.eclipse.jgit" % "org.eclipse.jgit" % "2.3.1.201302201838-r",
          "commons-io" % "commons-io" % "2.4"),
        licenses := Seq("LGPL v3" -> url("http://www.gnu.org/licenses/lgpl.txt")))
      .settings(buildInfoSettings: _*)
      .settings(
        EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed,
        EclipseKeys.withSource := true,
        unmanagedSourceDirectories in Compile <<= Seq(scalaSource in Compile).join,
        unmanagedSourceDirectories in Test <<= Seq(scalaSource in Test).join,
        sourceGenerators in Compile <+= buildInfo,
        buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
        buildInfoPackage := "giter8")
}
