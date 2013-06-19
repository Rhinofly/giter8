package giter8.files

import java.io.File

import giter8.render.StringRenderer

class ActionFactory(
  in: File, out: File,
  properties: Map[String, String],
  textContent: Option[String],
  renderer: StringRenderer) {

  private def textContentException(action:String) = 
    throw new Exception(s"Can not create a $action action without text content")
  
  def ignore: Ignore = Ignore(out)

  def renderAndAppend =
    textContent
      .map(RenderAndAppend(out, properties, _, renderer))
      .getOrElse(textContentException("RenderAndAppend"))

  def render =
    textContent
      .map(Render(out, properties, _, renderer))
      .getOrElse(textContentException("Render"))

  def copy = Copy(in, out)

}