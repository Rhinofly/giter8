package giter8

import java.io.File
import java.io.FileInputStream
import scala.collection.JavaConversions.enumerationAsScalaIterator
import java.util.{ Properties => JavaProperties }
import scala.util.Try
import giter8.files.Render
import giter8.files.Ignore
import giter8.files.Copy
import giter8.files.Action
import giter8.interaction.ExistingFileActionProvider
import giter8.files.FileUtilities
import giter8.files.FileInformation
import giter8.files.PathExpander
import giter8.properties.Properties
import giter8.properties.KnownPropertyNames
import giter8.interaction.Interaction
import org.apache.commons.io.FileUtils

class Template(
  directory: File,
  parameterProperties: Map[String, String],
  interaction: Interaction,
  rootPath: Option[String] = Some("src/main/g8"),
  scaffoldsPath: Option[String] = Some("src/main/scaffolds")) {

  lazy val root = rootPath.map(new File(directory, _)).getOrElse(directory)
  lazy val defaultProperties = readProperties(propertiesFile)

  lazy val templates = files.filter { file =>
    file != propertiesFile && !file.isDirectory
  }

  lazy val scaffoldsRoot = scaffoldsPath.map(new File(directory, _))

  lazy val properties = Properties.determineProperties(parameterProperties, defaultProperties, interaction.unchangedParameterHandler)

  def getOutputRoot(outputFolder: File) = {
    val outputPath = properties
      .get(KnownPropertyNames.NAME)
      .map(StringHelpers.normalize)
      .getOrElse(".")
      
      new File(outputFolder, outputPath)
  }

    def process(outputFolder: File) = Try {

      val outputRoot = getOutputRoot(outputFolder)

    processTemplates(outputRoot)

    copyScaffolds(outputRoot)

    s"Template applied in $outputRoot"
  }

  def copyScaffolds(outputRoot:File): Unit = {
    for (
      root <- scaffoldsRoot
    ) copyScaffolds(root, outputRoot)
  }

  private def copyScaffolds(root: File, outputRoot:File) {

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

  def processTemplates(outputRoot:File) = {
    val pathExpander = new PathExpander(properties)

    templates
      .map(toInputAndOutput(pathExpander, root, outputRoot))
      .foreach {
        case (in, out) =>

          val action = determineAction(in, out, properties, interaction.existingFileActionProvider)

          out.getParentFile.mkdirs()

          action.execute()

          if (!action.isInstanceOf[Ignore] && in.canExecute) {
            out.setExecutable(true)
          }
      }
  }

  private def toInputAndOutput(pathExpander: PathExpander, templatesRoot: File, outputRoot: File)(template: File): (File, File) = {
    val name = FileUtilities.getRelativePath(template, templatesRoot)
    val out = pathExpander.expand(name, outputRoot)
    template -> out
  }

  private def determineAction(in: File, out: File, parameters: Map[String, String], actionProvider: Option[ExistingFileActionProvider]) = {

    val fileInformation = FileInformation.get(in, out, parameters)
    val textContent = fileInformation.textContent

    val action: Action =
      if (out.exists)
        actionProvider
          .map(_.determineAction(fileInformation, parameters))
          .getOrElse(Ignore(out))
      else if (textContent.isDefined && !fileInformation.isVerbatim)
        Render(out, textContent.get, parameters)
      else
        Copy(in, out)

    action
  }

  private lazy val files = FileUtilities getAllFilesRecursively root
  private lazy val propertiesFile = new File(root, "default.properties")

  private def readProperties(propertiesFile: File): Map[String, String] =
    if (propertiesFile.exists) {

      val loadProperties = (fileToProperties _) andThen (propertiesToMap _)
      loadProperties(propertiesFile)

    } else Map.empty

  private def fileToProperties(propertiesFile: File) = {
    val properties = new JavaProperties
    val fileInputStream = new FileInputStream(propertiesFile)
    properties.load(fileInputStream)
    fileInputStream.close()
    properties
  }

  private def propertiesToMap(properties: JavaProperties) =
    (Map.empty[String, String] /: properties.propertyNames) { (m, k) =>
      m + (k.toString -> properties.getProperty(k.toString))
    }
}

object Template {

}