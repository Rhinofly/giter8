package giter8.interaction.user

import giter8.interaction.UnchangedParameterHandler
import giter8.interaction.UnchangedParameterHandler

object UserUnchangedPropertyHandler extends UnchangedParameterHandler {

  def handle(params: Map[String, String]): Map[String, String] =
    requestParametersFromUser(params)

  def requestParametersFromUser(params: Map[String, String]) =
    params map {
      case (k, v) =>
        val in = Console.readLine("%s [%s]: ", k, v).trim
        (k, if (in.isEmpty) v else in)
    }
}