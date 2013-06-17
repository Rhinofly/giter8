package giter8.files

import java.io.File
import scala.collection.immutable.Stream.consWrapper

object FileUtilities {

  def getAllFilesRecursively = getFilesRecursively(_ => true) _
  def getVisibleFilesRecursively = getFilesRecursively(!_.isHidden) _
  
  private def getFilesRecursively(filter: File => Boolean)(f: File): Stream[File] =
    f #:: getChildFiles(f, filter)

  private def getChildFiles(f: File, filter: File => Boolean): Stream[File] =
    if (f.isDirectory) {
      val filesInDirectory = f.listFiles.toStream
      filesInDirectory
        .filter(filter)
        .flatMap(getFilesRecursively(filter))
    } else Stream.empty
  
  def getRelativePath(file: File, base: File) = 
    (base.toURI relativize file.toURI).getPath
  
}