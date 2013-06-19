package giter8

import java.io.File
import scala.util.Try
import org.apache.commons.io.FileUtils
import giter8.files.Action
import giter8.files.Copy
import giter8.files.FileInformation
import giter8.files.FileUtilities
import giter8.files.Ignore
import giter8.files.PathExpander
import giter8.files.Render
import giter8.interaction.Interaction
import giter8.properties.KnownPropertyNames
import giter8.properties.Properties
import giter8.render.StringRenderer
import giter8.render.StringHelpers

class Template(
  directory: File,
  propertyOverrides: Map[String, String],
  interaction: Interaction,
  rootPath: Option[String] = Some("src/main/g8"),
  scaffoldsPath: Option[String] = Some("src/main/scaffolds")) {

  val root = rootPath.map(new File(directory, _)).getOrElse(directory)
  val files = FileUtilities getAllFilesRecursively root
  val templates =
    files.filter { file =>
      file != propertiesFile && !file.isDirectory
    }
  val propertiesFile = new File(root, "default.properties")
  val properties = Properties(propertiesFile, propertyOverrides, interaction.unchangedParameterHandler)
  val scaffoldsRoot = scaffoldsPath.map(new File(directory, _))

  val renderer = new StringRenderer
  val pathExpander = new PathExpander(properties.all, renderer)

  def process(outputFolder: File) = Try {

    properties.description foreach printDescription

    val outputRoot = getOutputRoot(outputFolder)

    processTemplates(outputRoot)
    copyScaffolds(outputRoot)

    s"Template applied in $outputRoot"
  }

  def processTemplates(outputRoot: File) =
    templates
      .map(toFileInformation(outputRoot))
      .foreach { fileInformation =>

        val action = determineAction(fileInformation)

        val input = fileInformation.input
        val target = fileInformation.target

        target.getParentFile.mkdirs()

        action.execute()

        if (!action.isInstanceOf[Ignore] && input.canExecute) {
          target.setExecutable(true)
        }
      }

  def toFileInformation(outputRoot: File)(template: File) = {
    val name = FileUtilities.getRelativePath(template, root)
    val out = pathExpander.expand(name, outputRoot)

    FileInformation(template, out, properties.all, renderer)
  }

  def determineAction(fileInformation: FileInformation) = {

    val text = fileInformation.text
    val input = fileInformation.input
    val target = fileInformation.target

    val action: Action =
      if (target.exists)
        interaction.existingFileActionProvider
          .map(_.determineAction(fileInformation))
          .getOrElse(Ignore(target))
      else if (text.isDefined && !fileInformation.isVerbatim)
        Render(target, properties.all, text.get, renderer)
      else
        Copy(input, target)

    action
  }

  def printDescription(description: String): Unit = {
    @scala.annotation.tailrec
    def printWords(cursor: Int, words: Iterable[String]) {
      if (!words.isEmpty) {
        val nextPosition = cursor + 1 + words.head.length
        if (nextPosition > 70) {
          println()
          printWords(0, words)
        } else {
          print(words.head + " ")
          printWords(nextPosition, words.tail)
        }
      }
    }
    println()
    printWords(0, description.split(" "))
    println("\n")
  }

  def getOutputRoot(outputFolder: File) = {
    val outputPath = properties.all
      .get(KnownPropertyNames.NAME)
      .map(StringHelpers.normalize)
      .getOrElse(".")

    new File(outputFolder, outputPath)
  }

  def copyScaffolds(outputRoot: File): Unit = {
    for (
      root <- scaffoldsRoot
    ) copyScaffolds(root, outputRoot)
  }

  def copyScaffolds(root: File, outputRoot: File) {

    val scaffolds = if (root.exists) Some(FileUtilities.getAllFilesRecursively(root)) else None

    for (
      fs <- scaffolds;
      f <- fs if !f.isDirectory
    ) {
      // Copy scaffolding recipes
      val realProjectRoot = FileUtilities.getVisibleFilesRecursively(outputRoot)
        .filter(_.isDirectory)
        .filter(_.getName == "project")
        .map(_.getParentFile)
        .headOption
        .getOrElse(outputRoot)

      val hidden = new File(realProjectRoot, ".g8")
      val name = FileUtilities.getRelativePath(f, root)
      val out = new File(hidden, name)
      FileUtils.copyFile(f, out)
    }
  }
}
