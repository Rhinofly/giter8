package giter8.files

import java.io.File
import giter8.render.StringRenderer
import giter8.properties.KnownPropertyNames

class PathExpander(properties: Map[String, String], renderer:StringRenderer) {

  def expand(relative: String, toPath: File): File =
    new File(toPath, renderPath(relative))

  private val renderPath = formatize _ andThen render _
  
  private def render(string:String) = 
    renderer.render(string, normalizedPackage)
  
  private def formatize(s: String) =
    s.replaceAll("""\$(\w+)__(\w+)\$""", """\$$1;format="$2"\$""")

  private lazy val normalizedPackage =
    (properties get KnownPropertyNames.PACKAGE)
      .map(packageToDirectory)
      .map(updatePackageValueInMap(properties))
      .getOrElse(properties)

  private def packageToDirectory(p: String) =
    p.replaceAll("""\.""", escapedFileSeparator)

  private val escapedFileSeparator =
    System.getProperty("file.separator") match {
      case "\\" => "\\\\"
      case sep => sep
    }

  private def updatePackageValueInMap(map: Map[String, String])(newValue: String) =
    map updated (KnownPropertyNames.PACKAGE, newValue)
}
