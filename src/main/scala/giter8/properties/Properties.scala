package giter8.properties

import giter8.Regexes.Param
import giter8.interaction.UnchangedParameterHandler

object Properties {
  def determineProperties(replacements: Map[String, String], defaultProperties: Map[String, String], unchangedPropertyHandler: Option[UnchangedParameterHandler]): Map[String, String] = {

    printDescription(defaultProperties)

    val result = ReplacePropertyValues(defaultProperties, replacements)

    result.invalidProperties.keys.foreach { key =>
      println(s"Ignoring unrecognized parameter: $key")
    }

    result.replacedProperties ++
      unchangedPropertyHandler.map(_.handle(result.unchangedProperties)).getOrElse(result.unchangedProperties)
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