package pilvi

import scala.concurrent.duration._
import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File, FileList}
import java.io.{File => LocalFile}

import scala.collection.immutable
import scala.concurrent.{Await, ExecutionContext, Future}

object Sync extends App {

  implicit val ec = ExecutionContext.global

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

    drive.ensureExists(remoteDirectoryPath)

    localDirectories.map(localDirectory => {
      val directoryName = localDirectory.getName
      upload(s"${localDirectoryPath}/${directoryName}", s"${remoteDirectoryPath}/${directoryName}")
    })

    val fileUploads: Seq[Future[Option[FileId]]] = localFiles.map(file => {
      val relativeFileName = file.getName
      if (!drive.exists(s"${remoteDirectoryPath}/${relativeFileName}")) {
        println(s"${relativeFileName} uploading again")
        drive.uploadFile(remoteDirectoryPath, file).map(Some(_))
      } else {
        println(s"${relativeFileName} already exists")
        Future { None }
      }
    })

    val fileIds = Await.ready(Future.sequence(fileUploads), 5 seconds)
    println(fileIds)
  }

  //drive.listFiles("/").foreach(file => println(file.getName))

  val localSyncFolderPath = "src/main/resources/test"
  val remoteFolderPath = "pilvi-sync"

  upload(localSyncFolderPath, remoteFolderPath)
}
