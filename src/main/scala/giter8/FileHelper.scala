package giter8

import java.io.File

object FileHelper {
  private def getFilesRecursively(filter: File => Boolean)(f: File): Stream[File] =
    f #:: getChildFiles(f, filter)

  private def getChildFiles(f: File, filter: File => Boolean): Stream[File] =
    if (f.isDirectory) {
      val filesInDirectory = f.listFiles.toStream
      filesInDirectory
        .filter(filter)
        .flatMap(getFilesRecursively(filter))
    } else Stream.empty

  def getAllFilesRecursively = getFilesRecursively(_ => true) _
  def getVisibleFilesRecursively = getFilesRecursively(!_.isHidden) _
}