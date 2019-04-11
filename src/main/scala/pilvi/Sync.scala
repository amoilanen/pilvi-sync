package pilvi

import scala.concurrent.duration._
import java.io.{File => LocalFile}
import scala.concurrent.{Await, ExecutionContext, Future}

import FileParts._

object Sync extends App {

  implicit val ec = ExecutionContext.global
  
  val timeout: Duration = 5 seconds

  val drive: GoogleDrive = GoogleDrive.getDrive

  def upload(localDirectoryPath: String, remoteDirectoryPath: String): Future[Unit] = {
    println(s"Uploading ${localDirectoryPath} to ${remoteDirectoryPath}")
    val localFilesAndDirectories: List[LocalFile] = LocalDrive.listFiles(localDirectoryPath)

    val localFiles = localFilesAndDirectories.filter(!_.isDirectory)
    val localDirectories = localFilesAndDirectories.filter(_.isDirectory)
    //println("Local files = ")
    //println(localFiles)
    //println("Local dirs = ")
    //println(localDirectories)

    drive.ensureExists(FilePath(remoteDirectoryPath)).flatMap { _ =>
      Future.sequence(localDirectories.map(localDirectory => {
        val directoryName = localDirectory.getName
        upload(s"${localDirectoryPath}/${directoryName}", s"${remoteDirectoryPath}/${directoryName}")
      }))
    }.flatMap { _ =>
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
      Future.sequence(fileUploads)
    }.map {_ => ()}
  }

  val localSyncFolderPath = "src/main/resources/test"
  val remoteFolderPath = "pilvi-sync"

  Await.result(upload(localSyncFolderPath, remoteFolderPath), 10 minutes)

  //TODO: Add logger and log the upload progress
  val files = Await.result(drive.listFiles(FilePath("/pilvi-sync")), timeout)
  files.foreach(file => println(file.getName))
}