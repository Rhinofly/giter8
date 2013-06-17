package giter8

import java.io.File
import org.apache.commons.io.FileUtils

sealed trait Action {
  def execute(): Unit
}

trait RenderAction extends Action {
  val out: File
  val text: String
  val parameters: Map[String, String]
  val append: Boolean

  def execute(): Unit =
    G8.write(out, text, parameters, append)
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