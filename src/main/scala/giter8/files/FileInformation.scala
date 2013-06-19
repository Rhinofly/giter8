package giter8.files

import java.io.File
import java.nio.charset.MalformedInputException

import scala.Array.canBuildFrom
import scala.util.control.Exception.catching
import scala.util.matching.Regex

import org.apache.commons.io.Charsets.UTF_8
import org.apache.commons.io.FileUtils

import giter8.properties.KnownPropertyNames
import giter8.render.StringRenderer

case class FileInformation(
    input: File, 
    target: File, 
    parameters: Map[String, String],
    renderer:StringRenderer) {
  
  val isVerbatim: Boolean = 
    (parameters get KnownPropertyNames.VERBATIM)
      .map(matchesVerbatimPattern)
      .getOrElse(false)
      
  val text:Option[String] = 
    readUtf8TextContent
      
  val actionFactory = new ActionFactory(input, target, parameters, text, renderer)
  
  private def matchesVerbatimPattern(verbatimPattern: String): Boolean = {
    val patterns = verbatimPattern.split(' ')
    patterns
      .map(toRegex)
      .exists(regexMatchesWithInput)
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

  private def regexMatchesWithInput(regex:Regex):Boolean = 
    regex.pattern.matcher(input.getName).matches
  
  private def readUtf8TextContent =
    catching(classOf[MalformedInputException]).opt {
      FileUtils.readFileToString(input, UTF_8)
    }
}
    
    

