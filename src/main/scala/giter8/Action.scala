package giter8

import java.io.File
import org.apache.commons.io.FileUtils
import org.apache.commons.io.Charsets.UTF_8

sealed trait Action {
  def execute(): Unit
}

trait RenderAction extends Action {
  val out: File
  val text: String
  val parameters: Map[String, String]
  val append: Boolean

  def execute(): Unit = {
    val applied = StringRenderer.render(text, parameters)
    FileUtils.writeStringToFile(out, applied, UTF_8, append)
  }
}

case class RenderAndAppend(
    out: File, 
    text: String, 
    parameters: Map[String, String]) extends RenderAction {
  val append = true
}

case class Render(
    out: File, 
    text: String, 
    parameters: Map[String, String]) extends RenderAction {
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