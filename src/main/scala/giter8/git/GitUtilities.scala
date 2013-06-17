package giter8.git

import org.eclipse.jgit.api.Git
import java.io.File
import scala.collection.JavaConverters._
import scala.util.Try

object GitUtilities {
  
  def clone(repo: String, branch: Option[String], directory: File):Try[File] = Try {

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
}