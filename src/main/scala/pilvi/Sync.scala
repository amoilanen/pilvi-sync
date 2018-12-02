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

  if (!drive.exists("pilvi-sync")) {
    val fileMetadata = new File()
    fileMetadata.setName("pilvi-sync")
    fileMetadata.setMimeType("application/vnd.google-apps.folder")
    val file = drive.d.files.create(fileMetadata).setFields("id").execute
    println("Created sync folder with ID: " + file.getId)
  } else {
    println("Sync folder already exists")
  }
  drive.listFiles("/").foreach(file => println(file.getName))

  val localFiles: List[LocalFile] = LocalDrive.listFiles("src/main/resources/test")

  println(localFiles)
  val fileUploads: Seq[Future[FileId]] = localFiles.map(file =>
    drive.uploadFile("pilvi-sync", file)
  )

  val fileIds = Await.ready(Future.sequence(fileUploads), 5 seconds)
  println(fileIds)
}
