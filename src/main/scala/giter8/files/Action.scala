package giter8.files

import java.io.File
import org.apache.commons.io.FileUtils
import org.apache.commons.io.Charsets.UTF_8
import giter8.render.StringRenderer

sealed trait Action {
  def execute(): Unit
}

trait RenderAction extends Action {
  val out: File
  val template: String
  val properties: Map[String, String]
  val append: Boolean
  val renderer:StringRenderer
  
  private lazy val renderedTemplate = renderer.render(template, properties)
  
  def execute(): Unit = 
    FileUtils.writeStringToFile(out, renderedTemplate, UTF_8, append)
}

case class RenderAndAppend(
    out: File, 
    properties: Map[String, String],
    template: String,
    renderer:StringRenderer) extends RenderAction {
  val append = true
}

case class Render(
    out: File, 
    properties: Map[String, String],
    template: String,
    renderer:StringRenderer) extends RenderAction {
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