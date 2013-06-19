package giter8.interaction

import giter8.files.ActionFactory
import giter8.files.Action
import giter8.files.FileInformation

trait ExistingFileActionProvider {

  def determineAction(fileInformation: FileInformation): Action
  
}