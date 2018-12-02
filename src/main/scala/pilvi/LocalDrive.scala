package pilvi

import java.io.{File => LocalFile}

object LocalDrive {

  def listFiles(filePath: String): List[LocalFile] =
    new LocalFile(filePath).listFiles().toList
}