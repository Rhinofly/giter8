package giter8

import java.io.File

class PathExpander(parameters: Map[String, String]) {

  def expand(relative: String, toPath: File): File =
    new File(toPath, StringRenderer.render(formatize(relative), normalizePackage(parameters)))

  private def formatize(s: String) =
    s.replaceAll("""\$(\w+)__(\w+)\$""", """\$$1;format="$2"\$""")

  private def normalizePackage(parameters: Map[String, String]) =
    (parameters get KnownPropertyNames.PACKAGE)
      .map(packageToDirectory)
      .map(updatePackageValueInMap(parameters))
      .getOrElse(parameters)

  private def packageToDirectory(p: String) =
    p.replaceAll("""\.""", escapedFileSeparator)

  private lazy val escapedFileSeparator =
    System.getProperty("file.separator") match {
      case "\\" => "\\\\"
      case sep => sep
    }

  private def updatePackageValueInMap(map: Map[String, String])(newValue: String) =
    map updated (KnownPropertyNames.PACKAGE, newValue)
}
