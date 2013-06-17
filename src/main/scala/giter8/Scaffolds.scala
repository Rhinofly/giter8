package giter8

import org.apache.commons.io.FileUtils
import java.io.File
import giter8.files.FileUtilities

object Scaffolds {
  private def copy(sf: File, output: File) {

    val scaffolds = if (sf.exists) Some(FileUtilities.getAllFilesRecursively(sf)) else None

    for (
      fs <- scaffolds;
      f <- fs if !f.isDirectory
    ) {
      // Copy scaffolding recipes
      val realProjectRoot = FileUtilities.getVisibleFilesRecursively(output)
        .filter(_.isDirectory)
        .filter(_.getName == "project")
        .map(_.getParentFile)
        .headOption
        .getOrElse(output)

      val hidden = new File(realProjectRoot, ".g8")
      val name = FileUtilities.getRelativePath(f, sf)
      val out = new File(hidden, name)
      FileUtils.copyFile(f, out)
    }
  }

  def copy(scaffoldsRoot: Option[File], outputRoot: File): Any = {
    for (
      root <- scaffoldsRoot
    ) copy(root, outputRoot)
  }
}