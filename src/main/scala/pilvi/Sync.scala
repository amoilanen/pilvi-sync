package pilvi

import scala.concurrent.duration._
import java.io.{File => LocalFile}
import scala.concurrent.{Await, ExecutionContext, Future}

import FileParts._

object Sync extends App {

  implicit val ec = ExecutionContext.global
  
  val timeout: Duration = 5 seconds

  val drive: GoogleDrive = GoogleDrive.getDrive

  def upload(localDirectoryPath: String, remoteDirectoryPath: String): Unit = {
    println(s"Uploading ${localDirectoryPath} to ${remoteDirectoryPath}")
    val localFilesAndDirectories: List[LocalFile] = LocalDrive.listFiles(localDirectoryPath)

    val localFiles = localFilesAndDirectories.filter(!_.isDirectory)
    val localDirectories = localFilesAndDirectories.filter(_.isDirectory)
    //println("Local files = ")
    //println(localFiles)
    //println("Local dirs = ")
    //println(localDirectories)

    drive.ensureExists(FilePath(remoteDirectoryPath))

    localDirectories.map(localDirectory => {
      val directoryName = localDirectory.getName
      upload(s"${localDirectoryPath}/${directoryName}", s"${remoteDirectoryPath}/${directoryName}")
    })

    val fileUploads: Seq[Future[Option[FileId]]] = localFiles.map(file => {
      val relativeFileName = file.getName
      if (!Await.result(drive.exists(FilePath(s"${remoteDirectoryPath}/${relativeFileName}")), timeout)) {
        println(s"${relativeFileName} uploading again")
        drive.uploadFile(FilePath(remoteDirectoryPath), file).map(Some(_))
      } else {
        println(s"${relativeFileName} already exists")
        Future { None }
      }
    })

    val fileIds = Await.ready(Future.sequence(fileUploads), timeout)
    println(fileIds)
  }

  val files = Await.result(drive.listFiles(FilePath("/")), timeout)

  files.foreach(file => println(file.getName))

  val localSyncFolderPath = "src/main/resources/test"
  val remoteFolderPath = "pilvi-sync"

  upload(localSyncFolderPath, remoteFolderPath)
}
