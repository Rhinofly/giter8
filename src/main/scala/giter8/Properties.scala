package giter8

import Regexes.Param

object Properties {
  def determineProperties(arguments: Seq[String], defaultProperties: Map[String, String], unchangedPropertyHandler: UnchangedParameterHandler): Map[String, String] = {

    printDescription(defaultProperties)

    val replacements = argumentsToProperties(arguments)
    val result = ReplacePropertyValues(defaultProperties, replacements)

    result.invalidProperties.keys.foreach { key =>
      println(s"Ignoring unrecognized parameter: $key")
    }

    result.replacedProperties ++
      unchangedPropertyHandler.handle(result.unchangedProperties)
  }

  private def printDescription(params: Map[String, String]): Unit = {
    val description = params get KnownPropertyNames.DESCRIPTION
    description foreach printDescription
  }

  private def printDescription(description: String): Unit = {
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

  private def argumentsToProperties(arguments: Seq[String]): Map[String, String] =
    arguments.map {
      case Param(key, value) => key -> value
    }.toMap

  private case class ReplacePropertyValues(baseProperties: Map[String, String], replacements: Map[String, String]) {

    private val fixedProperties =
      baseProperties.filterKeys(KnownPropertyNames.fixed.contains)

    lazy val unchangedProperties =
      baseProperties.filterNot {
        case (key, _) =>
          (replacements contains key) && (fixedProperties contains key)
      }

    lazy val replacedProperties =
      fixedProperties ++
        replacements.filterKeys(baseProperties.contains)

    lazy val invalidProperties =
      replacements.filterNot {
        case (key, _) => baseProperties contains key
      }
  }
}