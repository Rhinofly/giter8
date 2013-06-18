package giter8.git

import org.eclipse.jgit.api.Git
import java.io.File
import scala.collection.JavaConverters._
import scala.util.Try
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.errors.JGitInternalException
import scala.util.Failure

object GitUtilities {

  def clone(repository: Repository, directory: File): Try[File] = {

    val Repository(repositories, branch) = repository

    clone(repositories, branch, directory)
  }

  private def clone(repositories: Seq[String], branch: Option[String], directory: File, tried: Seq[String] = Seq.empty): Try[File] = {
    
    repositories.headOption map { repo =>

      clone(repo, branch, directory)
        .recoverWith {
          // try the remaining repositories
          case e: JGitInternalException =>
            // after cleaning the clone directory
            if (directory.exists) FileUtils forceDelete directory
            
            clone(repositories.tail, branch, directory, tried :+ repo)
        }
    } getOrElse createFailure(tried)
  }

  private def clone(repo: String, branch: Option[String], directory: File): Try[File] = Try {

    val fullBranchName = branch.map("refs/heads/" + _)

    val git = executeClone(repo, fullBranchName, directory)

    val optionalBranchFound =
      fullBranchName map containsBranch(git)

    git.getRepository.close()

    optionalBranchFound match {
      case Some(true) | None => directory
      case Some(false) => throw new Exception(s"Branch not found: ${branch.get}")
    }
  }

  private def containsBranch(git: Git)(branch: String) = {
    val branchList = git.branchList.call().asScala
    val branchNames = branchList map (_.getName)
    branchNames contains branch
  }

  private def executeClone(repo: String, fullBranchName: Option[String], directory: File) = {

    val cloneCommand =
      Git.cloneRepository()
        .setURI(repo)
        .setDirectory(directory)

    fullBranchName foreach cloneCommand.setBranch

    cloneCommand.call()
  }
  
  private def createFailure(tried:Seq[String]) = 
    Failure(new Exception("No more repositories found, tried: " + tried.mkString(",")))
  
}