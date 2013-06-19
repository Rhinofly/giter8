package giter8.interaction.user

import giter8.files.Action
import giter8.files.ActionFactory
import giter8.files.FileInformation
import giter8.interaction.ExistingFileActionProvider

object UserExistingFileActionProvider extends ExistingFileActionProvider {

  def determineAction(fileInformation: FileInformation): Action = {

    val isVerbatim = fileInformation.isVerbatim
    val isText = fileInformation.isText
    val fileName = fileInformation.target.getCanonicalPath
    
    val userResponse = requestActionFromUser(isVerbatim, isText, fileName)

    convertResponseToAction(isVerbatim, isText, userResponse, fileInformation.actionFactory)
  }

  private def requestActionFromUser(isText:Boolean, isVerbatim:Boolean, fileName:String): UserResponse = {

    val allowAppend = isText && !isVerbatim

    println(titleLine(fileName, isText, isVerbatim))
    print(optionLine(allowAppend))

    userResponse(allowAppend)
  }

  private def titleLine(fileName: String, isText: Boolean, isVerbatim: Boolean) = {
    val utf8 = "UTF8 text"
    val fileType = if (isText) utf8 else s"non-$utf8"
    val markedAsVerbatim = if (isVerbatim) " and is marked as verbatim" else ""

    s"$fileName already exists ($fileType)$markedAsVerbatim"
  }

  private def optionLine(allowAppend: Boolean) = {

    val append = if (allowAppend) " append," else ""
    val appendOption = if (allowAppend) "/a" else ""

    s"do you want to$append override or skip existing file? [O$appendOption/s] "
  }

  private def userResponse(allowAppend: Boolean) =
    Console.readLine match {
      case "a" if allowAppend => Append
      case "o" | "" => Override
      case _ => Skip
    }

  private def convertResponseToAction(
      isVerbatim:Boolean,
      isText:Boolean,
    userResponse: UserResponse,
    actionFactory: ActionFactory) = {

    userResponse match {
      case Skip =>
        actionFactory.ignore
      case Override if (isText && !isVerbatim) =>
        actionFactory.render
      case Override =>
        actionFactory.copy
      case Append if (isText) =>
        actionFactory.renderAndAppend
    }
  }

}