package giter8.interaction.user

import giter8.interaction.UnchangedParameterHandler

object UserUnchangedPropertyHandler extends UnchangedParameterHandler {

  def handle(properties: Map[String, String]): Map[String, String] =
    requestParametersFromUser(properties)

  private def requestParametersFromUser(properties: Map[String, String]) =
    properties map {
      case (k, v) =>
        val in = Console.readLine("%s [%s]: ", k, v).trim
        (k, if (in.isEmpty) v else in)
    }
}