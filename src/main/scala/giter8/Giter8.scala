package giter8

import java.io.File
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.apache.commons.io.FileUtils
import Regexes.Param
import giter8.git.GitUtilities
import giter8.git.Repository
import giter8.interaction.Interaction
import giter8.interaction.UserExistingFileActionProvider
import giter8.interaction.UserUnchangedPropertyHandler

class Giter8 {

  def run(arguments: Array[String]): Int = {

    val (parameters, repository) = splitParametersAndRepository(arguments)

    val outputFolder = new File(".")
    val interaction = Interaction(UserUnchangedPropertyHandler, UserExistingFileActionProvider)

    val result =
      repository map { repository =>
        run(repository, parameters, outputFolder, interaction)
      } getOrElse Failure(new Exception(usage))

    result match {
      case Success(message) =>
        printMessage(message); 0
      case Failure(error) => printError(error); 1
    }
  }

  private def splitParametersAndRepository(args: Array[String]): (Array[String], Option[Repository]) = {

    val (parameters, repositoryPattern) = args.partition { s =>
      Param.pattern.matcher(s).matches
    }
    parameters -> Repository.get(repositoryPattern)
  }

  def run(repository: Repository, parameters: Seq[String], outputFolder: File, interaction: Interaction): Try[String] = {
    run(repository, parametersToProperties(parameters), outputFolder, interaction)
  }

  private def parametersToProperties(parameters: Seq[String]): Map[String, String] =
    parameters.map {
      case Param(key, value) => key -> value
    }.toMap

  def run(repository: Repository, parameterProperties: Map[String, String], outputFolder: File, interaction: Interaction): Try[String] = {

    val tempDirectory =
      new File(FileUtils.getTempDirectory, "giter8-" + System.nanoTime)

    val clone = GitUtilities.clone(repository, tempDirectory)

    val result = clone.flatMap { clone =>
      new Template(clone, parameterProperties, interaction)
        .process(outputFolder)
    }

    if (tempDirectory.exists) FileUtils forceDelete tempDirectory

    result
  }

  private def printError(error: Throwable) =
    System.err.println("\n%s\n" format error.getMessage)

  private def printMessage(message: String) =
    println("\n%s\n" format message)

  private def usage = """giter8 %s
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

}

object Giter8 extends Giter8 {

  def main(args: Array[String]) {
    System.exit(run(args))
  }
}
