package giter8.properties

import java.io.File
import java.io.FileInputStream
import java.util.{Properties => JavaProperties}

import scala.annotation.migration
import scala.collection.JavaConverters.enumerationAsScalaIteratorConverter

import giter8.interaction.UnchangedParameterHandler

case class Properties(
  defaultProperties: Map[String, String],
  propertyOverrides: Map[String, String],
  unchangedPropertyHandler: Option[UnchangedParameterHandler]) {

  val description = defaultProperties get KnownPropertyNames.DESCRIPTION

  val fixedProperties =
    defaultProperties.filterKeys(KnownPropertyNames.fixed.contains)

  val replacedProperties =
    fixedProperties ++
      propertyOverrides.filterKeys(defaultProperties.contains)

  val invalidProperties =
    propertyOverrides.filterNot {
      case (key, _) => defaultProperties contains key
    }

  val unchangedProperties =
    defaultProperties.filterNot {
      case (key, _) =>
        (propertyOverrides contains key) && (fixedProperties contains key)
    }

  lazy val transformedUnchangedProperties =
    unchangedPropertyHandler.map(_.handle(unchangedProperties)).getOrElse(unchangedProperties)

  lazy val all = {

    invalidProperties.keys.foreach { key =>
      println(s"Ignoring unrecognized parameter: $key")
    }

    replacedProperties ++ transformedUnchangedProperties
  }
}

object Properties {

  def apply(propertiesFile: File, propertyOverrides: Map[String, String], unchangedPropertyHandler: Option[UnchangedParameterHandler]):Properties =
    Properties(readProperties(propertiesFile), propertyOverrides, unchangedPropertyHandler)

  def readProperties(propertiesFile: File): Map[String, String] =
    if (propertiesFile.exists) {

      val loadProperties = (fileToProperties _) andThen (propertiesToMap _)
      loadProperties(propertiesFile)

    } else Map.empty

  def fileToProperties(propertiesFile: File) = {
    val properties = new JavaProperties
    val fileInputStream = new FileInputStream(propertiesFile)
    properties.load(fileInputStream)
    fileInputStream.close()
    properties
  }

  def propertiesToMap(properties: JavaProperties) =
    (Map.empty[String, String] /: properties.propertyNames.asScala) { (m, k) =>
      m + (k.toString -> properties.getProperty(k.toString))
    }
}
