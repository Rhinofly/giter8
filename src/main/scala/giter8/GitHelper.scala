package giter8

import org.eclipse.jgit.api.Git
import java.io.File
import scala.collection.JavaConverters._
import scala.util.Success
import scala.util.Failure

object GitHelper {
  def clone(repo: String, branch: Option[String], directory: File) = {

    val fullBranchName = branch.map("refs/heads/" + _)

    val git = executeClone(repo, fullBranchName, directory)

    val optionalBranchFound = 
      fullBranchName map containsBranch(git)

    git.getRepository.close()

    optionalBranchFound match {
      case Some(true) | None => Success(directory)
      case Some(false) => Failure(new Exception(s"Branch not found: ${branch.get}"))
    }
  }

  def containsBranch(git: Git)(branch: String) = {
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
}