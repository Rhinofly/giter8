package giter8

import java.io.File
import java.nio.charset.MalformedInputException

import scala.Array.canBuildFrom
import scala.util.control.Exception.allCatch
import scala.util.control.Exception.catching

import org.apache.commons.io.Charsets.UTF_8
import org.apache.commons.io.FileUtils
import org.clapper.scalasti.StringTemplate

import Regex.Param

object G8 {
  import scala.util.control.Exception.allCatch
  import org.clapper.scalasti.StringTemplate

  private val renderer = new StringRenderer

  def apply(fromMapping: Seq[(File, String)], toPath: File, parameters: Map[String, String]): Seq[File] =
    fromMapping filter { !_._1.isDirectory } flatMap {
      case (in, relative) =>
        apply(in, expandPath(relative, toPath, parameters), parameters)
    }

  def apply(in: File, out: File, parameters: Map[String, String]) = {
    try {
      if (verbatim(in, parameters)) FileUtils.copyFile(in, out)
      else {
        write(out, FileUtils.readFileToString(in, "UTF-8"), parameters)
      }
    } catch {
      case e: Exception =>
        println("Falling back to file copy for %s: %s" format (in.toString, e.getMessage))
        FileUtils.copyFile(in, out)
    }
    allCatch opt {
      if (in.canExecute) out.setExecutable(true)
    }
    Seq(out)
  }

  def write(out: File, template: String, parameters: Map[String, String], append: Boolean = false) {
    val applied = new StringTemplate(template)
      .setAttributes(parameters)
      .registerRenderer(renderer)
      .toString
    FileUtils.writeStringToFile(out, applied, UTF_8, append)
  }

  def verbatim(file: File, parameters: Map[String, String]): Boolean =
    parameters.get("verbatim") map { s => globMatch(file, s.split(' ').toSeq) } getOrElse { false }
  private def globMatch(file: File, patterns: Seq[String]): Boolean =
    patterns exists { globRegex(_).findFirstIn(file.getName).isDefined }
  private def globRegex(pattern: String) = "^%s$".format(pattern flatMap {
    case '*' => """.*"""
    case '?' => """."""
    case '.' => """\."""
    case x => x.toString
  }).r

  def expandPath(relative: String, toPath: File, parameters: Map[String, String]): File = {
    val fileParams = Map(parameters.toSeq map {
      case (k, v) if k == "package" => (k, v.replaceAll("""\.""", System.getProperty("file.separator") match {
        case "\\" => "\\\\"
        case sep => sep
      }))
      case x => x
    }: _*)

    new File(toPath, new StringTemplate(formatize(relative)).setAttributes(fileParams).registerRenderer(renderer).toString)
  }
  private def formatize(s: String) = s.replaceAll("""\$(\w+)__(\w+)\$""", """\$$1;format="$2"\$""")

  def decapitalize(s: String) = if (s.isEmpty) s else s(0).toLower + s.substring(1)
  def startCase(s: String) = s.toLowerCase.split(" ").map(_.capitalize).mkString(" ")
  def wordOnly(s: String) = s.replaceAll("""\W""", "")
  def upperCamel(s: String) = wordOnly(startCase(s))
  def lowerCamel(s: String) = decapitalize(upperCamel(s))

  def snakeCase(s: String) = s.replaceAll("""\s+""", "_")
  def packageDir(s: String) = s.replace(".", System.getProperty("file.separator"))
  def addRandomId(s: String) = s + "-" + new java.math.BigInteger(256, new java.security.SecureRandom).toString(32)

}

object G8Helpers {
  import scala.util.control.Exception.catching

  def getOutputRoot(parameters: Map[String, String], outputFolder: java.io.File): java.io.File = {

    val outputPath = parameters
      .get("name")
      .map(StringHelpers.normalize)
      .getOrElse(".")

    new File(outputFolder, outputPath)
  }

  private def argumentsToProperties(arguments: Seq[String]): Map[String, String] =
    arguments.map {
      case Param(key, value) => key -> value
    }.toMap

  case class ReplacePropertyValues(baseProperties: Map[String, String], replacements: Map[String, String]) {

    lazy val unchangedProperties =
      baseProperties.filterNot {
        case (key, _) => replacements contains key
      }

    lazy val replacedProperties =
      replacements.filterKeys(baseProperties.contains)

    lazy val invalidProperties =
      replacements.filterNot {
        case (key, _) => baseProperties contains key
      }
  }

  def getParameters(arguments: Seq[String], defaultProperties: Map[String, String]): Map[String, String] = {
    val replacements = argumentsToProperties(arguments)
    val result = ReplacePropertyValues(defaultProperties, replacements)

    result.invalidProperties.keys.foreach { key =>
      println(s"Ignoring unrecognized parameter: $key")
    }

    result.replacedProperties ++ interact(result.unchangedProperties)
  }

  def interact(params: Map[String, String]) = {
    val (desc, others) = params partition { case (k, _) => k == "description" }

    desc.values.foreach { d =>
      @scala.annotation.tailrec
      def liner(cursor: Int, rem: Iterable[String]) {
        if (!rem.isEmpty) {
          val next = cursor + 1 + rem.head.length
          if (next > 70) {
            println()
            liner(0, rem)
          } else {
            print(rem.head + " ")
            liner(next, rem.tail)
          }
        }
      }
      println()
      liner(0, d.split(" "))
      println("\n")
    }

    val fixed = Set("verbatim")
    others map {
      case (k, v) =>
        if (fixed.contains(k))
          (k, v)
        else {
          val in = Console.readLine("%s [%s]: ", k, v).trim
          (k, if (in.isEmpty) v else in)
        }
    }
  }

  private def relativize(in: File, from: File) = from.toURI().relativize(in.toURI).getPath

  type In = File
  type Out = File

  def toInputAndOutput(parameters: Map[String, String], templatesRoot: File, outputRoot: File)(template: File): (In, Out) = {
    val name = relativize(template, templatesRoot)
    val out = G8.expandPath(name, outputRoot, parameters)
    template -> out
  }

  def write(templatesRoot: File,
    templates: Iterable[File],
    parameters: Map[String, String],
    outputRoot: File) = {

    val renderer = new StringRenderer

    templates
      .map(toInputAndOutput(parameters, templatesRoot, outputRoot))
      .foreach {
        case (in, out) =>

          val text = readUtf8File(in)
          val verbatim = G8.verbatim(out, parameters)

          val action =
            if (out.exists) Skip
            else NoAction

          out.getParentFile.mkdirs()

          (action, text, verbatim) match {
            case (Skip, _, _) =>
              println("Skipping existing file: %s" format out.toString)
            case (_, Some(text), false) =>
              G8.write(out, text, parameters, append = false)
            case (_, _, _) =>
              FileUtils.copyFile(in, out)
          }

          if (action != Skip && in.canExecute) {
            out.setExecutable(true)
          }
      }

    Right("Template applied in %s" format (outputRoot.toString))
  }

  def readUtf8File(file: File) =
    catching(classOf[MalformedInputException]).opt {
      FileUtils.readFileToString(file, UTF_8)
    }

  sealed trait Action
  case object Append extends Action
  case object Override extends Action
  case object Skip extends Action
  case object NoAction extends Action

  private def getAction(fileName: String, isText: Boolean, verbatim: Boolean) = {
    val utf8 = "UTF8 text"
    val fileType = if (isText) utf8 else s"non-$utf8"
    val markedAsVerbatim = if (verbatim) " and is marked as verbatim" else ""
    println(s"$fileName already exists ($fileType)$markedAsVerbatim")

    val allowAppend = isText && !verbatim

    val append = if (allowAppend) " append," else ""
    val appendOption = if (allowAppend) "/a" else ""
    print(s"do you want to$append override or skip existing file? [O$appendOption/s] ")

    Console.readLine match {
      case "a" if allowAppend => Append
      case "o" | "" => Override
      case _ => Skip
    }
  }

  def writeInterative(templatesRoot: File,
    templates: Iterable[File],
    parameters: Map[String, String],
    outputRoot: File) = {

    val renderer = new StringRenderer

    templates
      .map(toInputAndOutput(parameters, templatesRoot, outputRoot))
      .foreach {
        case (in, out) =>
          val text = readUtf8File(in)
          val verbatim = G8.verbatim(out, parameters)

          val action =
            if (out.exists) {
              getAction(out.getCanonicalPath, text.isDefined, verbatim)
            } else NoAction

          out.getParentFile.mkdirs()

          (action, text, verbatim) match {
            case (Skip, _, _) =>
              println("Skipping existing file: %s" format out.toString)
            case (Append, Some(text), _) =>
              G8.write(out, text, parameters, append = true)
            case (_, Some(text), false) =>
              G8.write(out, text, parameters, append = false)
            case (_, _, _) =>
              FileUtils.copyFile(in, out)
          }

          if (action != Skip && in.canExecute) {
            out.setExecutable(true)
          }
      }

    Right("Template applied in %s" format (outputRoot.toString))
  }

  def copyScaffolds(sf: File, output: File) {

    val scaffolds = if (sf.exists) Some(FileHelper.getAllFilesRecursively(sf)) else None

    for (
      fs <- scaffolds;
      f <- fs if !f.isDirectory
    ) {
      // Copy scaffolding recipes
      val realProjectRoot = FileHelper.getVisibleFilesRecursively(output)
        .filter(_.isDirectory)
        .filter(_.getName == "project")
        .map(_.getParentFile)
        .headOption
        .getOrElse(output)

      val hidden = new File(realProjectRoot, ".g8")
      val name = relativize(f, sf)
      val out = new File(hidden, name)
      FileUtils.copyFile(f, out)
    }
  }

  def copyScaffolds(scaffoldsRoot: Option[File], outputRoot: File): Any = {
    for (
      root <- scaffoldsRoot
    ) copyScaffolds(root, outputRoot)
  }
}

class StringRenderer extends org.clapper.scalasti.AttributeRenderer[String] {
  import G8._
  def toString(value: String): String = value

  override def toString(value: String, formatName: String): String = {
    val formats = formatName.split(",").map(_.trim)
    formats.foldLeft(value)(format)
  }

  def format(value: String, formatName: String): String =
    formatName match {
      case "upper" | "uppercase" => value.toUpperCase
      case "lower" | "lowercase" => value.toLowerCase
      case "cap" | "capitalize" => value.capitalize
      case "decap" | "decapitalize" => decapitalize(value)
      case "start" | "start-case" => startCase(value)
      case "word" | "word-only" => wordOnly(value)
      case "Camel" | "upper-camel" => upperCamel(value)
      case "camel" | "lower-camel" => lowerCamel(value)
      case "hyphen" | "hyphenate" => StringHelpers.hyphenate(value)
      case "norm" | "normalize" => StringHelpers.normalize(value)
      case "snake" | "snake-case" => snakeCase(value)
      case "packaged" | "package-dir" => packageDir(value)
      case "random" | "generate-random" => addRandomId(value)
      case _ => value
    }
}
