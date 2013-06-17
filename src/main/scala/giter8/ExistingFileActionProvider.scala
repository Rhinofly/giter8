package giter8

import java.io.File

trait ExistingFileActionProvider {

  def determineAction(fileInformation: FileInformation, parameters: Map[String, String]): Action
}