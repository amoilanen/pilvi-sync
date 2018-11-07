package pilvi

import com.google.api.services.drive.Drive
import com.google.api.services.drive.model.{File, FileList}

object Sync extends App {

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
}
