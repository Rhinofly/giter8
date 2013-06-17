package giter8.files

import java.io.File
import org.apache.commons.io.FileUtils
import org.apache.commons.io.Charsets.UTF_8
import giter8.StringRenderer

sealed trait Action {
  def execute(): Unit
}

trait RenderAction extends Action {
  val out: File
  val template: String
  val properties: Map[String, String]
  val append: Boolean

  private lazy val renderedTemplate = StringRenderer.render(template, properties)
  
  def execute(): Unit = 
    FileUtils.writeStringToFile(out, renderedTemplate, UTF_8, append)
}

case class RenderAndAppend(
    out: File, 
    template: String, 
    properties: Map[String, String]) extends RenderAction {
  val append = true
}

case class Render(
    out: File, 
    template: String, 
    properties: Map[String, String]) extends RenderAction {
  val append = false
}

case class Copy(in: File, out: File) extends Action {
  def execute(): Unit =
    FileUtils.copyFile(in, out)
}

case class Ignore(out: File) extends Action {
  def execute(): Unit =
    println(s"Skipping existing file: $out")
}