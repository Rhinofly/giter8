package giter8

import java.io.File
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.errors.JGitInternalException
import Regexes.Branch
import Regexes.Git
import Regexes.Local
import Regexes.Param
import Regexes.Repo
import giter8.git.GitUtilities

class Giter8 {

  private val tempDirectory =
    new File(FileUtils.getTempDirectory, "giter8-" + System.nanoTime)

  def partitionParams(args: Array[String]) =
    args.partition { s =>
      Param.pattern.matcher(s).matches
    }

  def run(args: Array[String]): Int = {

    val (params, repositoryPattern) = partitionParams(args)

    val result = performTasks(params, repositoryPattern)

    removeTempDirectory()

    result match {
      case Success(message) =>
        printMessage(message); 0
      case Failure(error) => printError(error); 1
    }
  }

  def performTasks(repo: String, branch: Option[String], arguments: Seq[String]): Try[String] = {

    val template = GitUtilities.clone(repo, branch, tempDirectory)

    template.flatMap { template =>
      val templateInfo = Template.fetchInfo(template, Some("src/main/g8"), Some("src/main/scaffolds"))
      val TemplateInfo(defaultProperties, templates, templatesRoot, scaffoldsRoot) = templateInfo
      
      val properties = Properties.determineProperties(arguments, defaultProperties, UserUnchangedPropertyHandler)

      val outputRoot = getOutputRoot(properties, outputFolder = new File("."))

      val result = Template.processTemplates(templatesRoot, templates, properties, outputRoot, Some(UserExistingFileActionProvider))

      if (result.isSuccess) Scaffolds.copy(scaffoldsRoot, outputRoot)

      result
    }
  }

  def getOutputRoot(parameters: Map[String, String], outputFolder: File): File = {

    val outputPath = parameters
      .get(KnownPropertyNames.NAME)
      .map(StringHelpers.normalize)
      .getOrElse(".")

    new File(outputFolder, outputPath)
  }
  
  
  def performTasks(user: String, project: String, branch: Option[String], params: Seq[String]): Try[String] =

    performTasks(s"git://github.com/$user/$project.g8.git", branch, params)
      .recoverWith {
        // assume it was an access failure, try with ssh
        case _: JGitInternalException =>
          // after cleaning the clone directory
          removeTempDirectory()
          performTasks(s"git@github.com:$user/$project.g8.git", branch, params)
      }

  def removeTempDirectory() =
    if (tempDirectory.exists) FileUtils forceDelete tempDirectory

  def printError(error: Throwable) =
    System.err.println("\n%s\n" format error.getMessage)

  def printMessage(message: String) =
    println("\n%s\n" format message)

  def usage = """giter8 %s
                |Usage: g8 [TEMPLATE] [OPTION]...
                |Apply specified template.
                |
                |OPTIONS
                |    -b, --branch
                |        Resolves a template within a given branch
                |    --paramname=paramvalue
                |        Set given parameter value and bypass interaction.
                |
                |
                |Apply template and interactively fulfill parameters.
                |    g8 n8han/giter8
                |
                |Or
                |    g8 git://github.com/n8han/giter8.git
                |
                |Apply template from a remote branch
                |    g8 n8han/giter8 -b some-branch
                |
                |Apply template from a local repo
                |    g8 file://path/to/the/repo
                |
                |Apply given name parameter and use defaults for all others.
                |    g8 n8han/giter8 --name=template-test
                |
                |""".stripMargin format (BuildInfo.version)

  private def performTasks(params: Array[String], repositoryPattern: Array[String]): Try[String] =
    repositoryPattern match {
      case Array(Local(repo)) =>
        performTasks(repo, None, params)
      case Array(Local(repo), Branch(_), branch) =>
        performTasks(repo, Some(branch), params)
      case Array(Repo(user, proj)) =>
        performTasks(user, proj, None, params)
      case Array(Repo(user, proj), Branch(_), branch) =>
        performTasks(user, proj, Some(branch), params)
      case Array(Git(remote)) =>
        performTasks(remote, None, params)
      case Array(Git(remote), Branch(_), branch) =>
        performTasks(remote, Some(branch), params)
      case _ => Failure(new Exception(usage))
    }

}

object Giter8 extends Giter8 {

  val home =
    Option(System getProperty "G8_HOME")
      .map(new File(_))
      .getOrElse(new File(System getProperty "user.home", ".g8"))

  /** Main-class runner just for testing from sbt*/
  def main(args: Array[String]) {
    System.exit(run(args))
  }
}
