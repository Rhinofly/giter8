package giter8

import org.clapper.scalasti.StringTemplate

object StringRenderer extends org.clapper.scalasti.AttributeRenderer[String] {
  def toString(value: String): String = value

  override def toString(value: String, formatName: String): String = {
    val formats = formatName.split(",").map(_.trim)
    formats.foldLeft(value)(format)
  }

  def render(template: String, parameters: Map[String, String]) =
    new StringTemplate(template)
      .setAttributes(parameters)
      .registerRenderer(this)
      .toString
  
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

  def decapitalize(s: String) = if (s.isEmpty) s else s(0).toLower + s.substring(1)
  def startCase(s: String) = s.toLowerCase.split(" ").map(_.capitalize).mkString(" ")
  def wordOnly(s: String) = s.replaceAll("""\W""", "")
  def upperCamel(s: String) = wordOnly(startCase(s))
  def lowerCamel(s: String) = decapitalize(upperCamel(s))

  def snakeCase(s: String) = s.replaceAll("""\s+""", "_")
  def packageDir(s: String) = s.replace(".", System.getProperty("file.separator"))
  def addRandomId(s: String) = s + "-" + new java.math.BigInteger(256, new java.security.SecureRandom).toString(32)

}