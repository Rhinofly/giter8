package giter8

import java.io.File
import java.io.FileInputStream
import scala.collection.JavaConversions.enumerationAsScalaIterator
import java.util.Properties

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
    val properties = new Properties
    val fileInputStream = new FileInputStream(propertiesFile)
    properties.load(fileInputStream)
    fileInputStream.close()
    properties
  }

  private def propertiesToMap(properties: Properties) =
    (Map.empty[String, String] /: properties.propertyNames) { (m, k) =>
      m + (k.toString -> properties.getProperty(k.toString))
    }
}