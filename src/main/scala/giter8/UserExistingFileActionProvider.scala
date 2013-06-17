package giter8

object UserExistingFileActionProvider extends ExistingFileActionProvider {

  def determineAction(fileInformation: FileInformation, parameters: Map[String, String]): Action = {

    val userResponse = requestActionFromUser(fileInformation)

    convertResponseToAction(fileInformation, userResponse, parameters)
  }

  private def requestActionFromUser(fileInformation: FileInformation): UserResponse = {

    val FileInformation(_, out, isVerbatim, textContent) = fileInformation

    val isText = textContent.isDefined
    val allowAppend = isText && !isVerbatim

    println(titleLine(out.getCanonicalPath, isText, isVerbatim))
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

  private def convertResponseToAction(fileInformation: FileInformation, userResponse: UserResponse, parameters: Map[String, String]) = {
    val FileInformation(in, out, isVerbatim, textContent) = fileInformation

    (userResponse, textContent) match {
      case (Skip, _) =>
        Ignore(out)
      case (Override, Some(text)) if (!isVerbatim) =>
        Render(out, text, parameters)
      case (Override, _) =>
        Copy(in, out)
      case (Append, Some(text)) =>
        RenderAndAppend(out, text, parameters)
    }
  }

}