package giter8

import java.io.File

case class FileInformation(in: File, out: File, isVerbatim: Boolean, textContent: Option[String])

object FileInformation {
  
  def get(in:File, out:File, parameters:Map[String, String]) = {
    val text = G8Helpers.readUtf8File(in)
    val isVerbatim = G8.verbatim(out, parameters)
    
    FileInformation(in, out, isVerbatim, text)
  }
  
}
