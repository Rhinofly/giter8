package giter8

import java.io.File
import java.io.FileInputStream
import scala.collection.JavaConversions.enumerationAsScalaIterator
import java.util.{Properties => JavaProperties}
import scala.util.Try

object Template {

  def fetchInfo(base: File, templatePath: Option[String], scaffoldsPath: Option[String]): TemplateInfo = {

    val templatesRoot = templatePath.map(new File(base, _)).getOrElse(base)
    val files = FileHelper.getAllFilesRecursively(templatesRoot)
    val propertiesFile = new File(templatesRoot, "default.properties")

    val defaultProperties = readProperties(propertiesFile)

    val templates = files.filter { file =>
      file != propertiesFile && !file.isDirectory
    }

    val scaffoldsRoot = scaffoldsPath.map(new File(base, _))

    TemplateInfo(defaultProperties, templates, templatesRoot, scaffoldsRoot)
  }

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

  def processTemplates(templatesRoot: File,
    templates: Iterable[File],
    parameters: Map[String, String],
    outputRoot: File,
    existingFileActionProvider: Option[ExistingFileActionProvider]) = Try {

    val pathExpander = new PathExpander(parameters)
    
    templates
      .map(toInputAndOutput(pathExpander, templatesRoot, outputRoot))
      .foreach {
        case (in, out) =>

          val action = determineAction(in, out, parameters, existingFileActionProvider)

          out.getParentFile.mkdirs()

          action.execute()

          if (!action.isInstanceOf[Ignore] && in.canExecute) {
            out.setExecutable(true)
          }
      }

    s"Template applied in $outputRoot"
  }

  private def toInputAndOutput(pathExpander: PathExpander, templatesRoot: File, outputRoot: File)(template: File): (File, File) = {
    val name = FileHelper.relativize(template, templatesRoot)
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
}