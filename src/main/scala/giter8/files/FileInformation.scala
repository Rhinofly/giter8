package giter8.files

import java.io.File
import scala.util.control.Exception.catching
import java.nio.charset.MalformedInputException
import org.apache.commons.io.FileUtils
import org.apache.commons.io.Charsets.UTF_8
import scala.util.matching.Regex
import giter8.KnownPropertyNames
import java.nio.charset.MalformedInputException
import scala.Array.canBuildFrom

case class FileInformation(
    input: File, 
    target: File, 
    isVerbatim: Boolean, 
    textContent: Option[String])

object FileInformation {

  def get(in: File, out: File, parameters: Map[String, String]) = {
    FileInformation(in, out, isVerbatim(out, parameters), readUtf8TextContent(in))
  }

  private def isVerbatim(file: File, parameters: Map[String, String]): Boolean =
    (parameters get KnownPropertyNames.VERBATIM)
      .map(matchesVerbatimPattern(file))
      .getOrElse(false)

  private def matchesVerbatimPattern(file: File)(verbatimPattern: String): Boolean = {
    val patterns = verbatimPattern.split(' ')
    patterns
      .map(toRegex)
      .exists(regexMatchesWith(file))
  }

  private def toRegex(pattern: String) = {
    val regexPattern =
      pattern flatMap {
        case '*' => """.*"""
        case '?' => """."""
        case '.' => """\."""
        case x => x.toString
      }
    
    ("^" + regexPattern + "$").r
  }

  private def regexMatchesWith(file:File)(regex:Regex):Boolean = 
    regex.pattern.matcher(file.getName).matches
  
  private def readUtf8TextContent(file: File) =
    catching(classOf[MalformedInputException]).opt {
      FileUtils.readFileToString(file, UTF_8)
    }
}
