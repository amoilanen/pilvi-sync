package pilvi

import java.io.{File => LocalFile}
import java.io.InputStreamReader
import java.util.Collections

import scala.collection.JavaConverters._
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.http.FileContent
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.drive.model.{File, FileList}
import com.google.api.services.drive.{Drive, DriveScopes}
import pilvi.Sync.drive

import scala.concurrent.{ExecutionContext, Future}

object FileParts {
  case class FileId(value: String)
  case class FileName(value: String)
  case class FilePath(value: String) {
    val pathParts: List[FileName] = value.split('/').toList.map(FileName(_))
  }
}

object StringFilePartsConversions {
  import FileParts._

  implicit def toFileId(fileId: String): FileId = FileId(fileId)
  implicit def toFileName(fileName: String): FileName = FileName(fileName)
  implicit def toFilePath(filePath: String): FilePath = FilePath(filePath)
}

//TODO: Make the methods of GoogleDrive asynchronous: return Future
//TODO: Use consistent naming: folder or directory (probably folder as Google drive uses it)
case class GoogleDrive(d: Drive)(implicit ec: ExecutionContext) {

  import FileParts._

  def getFileIdInParentFolder(parentFolderId: FileId, fileName: FileName): Option[FileId] = {
    val searchResult: FileList = d.files.list().set("q", s"'${parentFolderId.value}' in parents and name='${fileName.value}' and trashed = false").execute()
    val foundFiles = searchResult.getFiles.asScala.toList
    foundFiles.headOption.map(_.getId).map(FileId(_))
  }

  def getFileId(filePath: FilePath): Option[FileId] = {
    val pathParts: List[FileName] = filePath.pathParts

    val fileId: Option[FileId] = pathParts.foldLeft(Option(FileId("root")))({
      case (Some(FileId(parentFolderId)), pathPart) => getFileIdInParentFolder(FileId(parentFolderId), pathPart)
      case _ => None
    })

    fileId
  }

  def listFiles(filePath: FilePath): List[File] = {
    val fileId: Option[FileId] = getFileId(filePath)

    val foundFiles = fileId match {
      case Some(id) => d.files.list().set("q", s"'${id.value}' in parents and trashed = false").execute().getFiles().asScala.toList
      case None => List()
    }
    foundFiles
  }

  def uploadFile(parentDirectoryPath: FilePath, file: LocalFile): Future[FileId] = {
    val fileMetadata = new File()
    fileMetadata.setName(file.getName)
    fileMetadata.setParents(getFileId(parentDirectoryPath).map(_.value).toList.asJava)

    val mediaContent = new FileContent("text/plain", file)
    val createdFile: Future[File] = Future {
      d.files().create(fileMetadata, mediaContent)
        .setFields("id")
        .execute()
    }
    createdFile.map(file => FileId(file.getId))
  }

  def exists(filePath: FilePath): Boolean =
    getFileId(filePath).isDefined

  def ensureFolderExistsInParentFolder(parentFolderId: FileId, folderName: FileName): FileId = {
    val folderId: Option[FileId] = getFileIdInParentFolder(parentFolderId, folderName)

    if (!folderId.isDefined) {
      val fileMetadata = new File()
      fileMetadata.setName(folderName.value)
      fileMetadata.setParents(List(parentFolderId.value).asJava)
      fileMetadata.setMimeType("application/vnd.google-apps.folder")
      val file = drive.d.files.create(fileMetadata).setFields("id").execute
      println(s"Created folder ${folderName} with ID: " + file.getId)
      return FileId(file.getId)
    } else {
      println(s"Folder ${folderName} already exists")
      folderId.get
    }
  }

  //TODO: Make asynchronous
  def ensureExists(folderPath: FilePath): FileId = {
    val folderId: FileId = folderPath.pathParts.foldLeft(FileId("root"))({
      case (parentFolderId, pathPart) => ensureFolderExistsInParentFolder(parentFolderId, pathPart)
    })
    folderId
  }
}

object GoogleDrive {

  private val VerificationCodeReceiverPort = 8888
  private val ApplicationName = "pilvi-sync"
  private val TokensDirectoryPath = "tokens"

  private val Scopes = Collections.singletonList(DriveScopes.DRIVE)

  private val CredentialsFilePath = "/credentials.json"

  private def getCredentials(transport: NetHttpTransport, jsonFactory: JacksonFactory): Credential = {
    val in = GoogleDrive.getClass.getResourceAsStream(CredentialsFilePath)
    val clientSecrets = GoogleClientSecrets.load(jsonFactory, new InputStreamReader(in))

    val flow = new GoogleAuthorizationCodeFlow.Builder(transport, jsonFactory, clientSecrets, Scopes)
      .setDataStoreFactory(new FileDataStoreFactory(new LocalFile(TokensDirectoryPath))).setAccessType("offline").build
    val receiver = new LocalServerReceiver.Builder().setPort(VerificationCodeReceiverPort).build
    new AuthorizationCodeInstalledApp(flow, receiver).authorize("user")
  }

  def getDrive(implicit ec: ExecutionContext): GoogleDrive = {
    val jsonFactory: JacksonFactory = JacksonFactory.getDefaultInstance
    val transport = GoogleNetHttpTransport.newTrustedTransport
    val credential = getCredentials(transport, jsonFactory)
    val drive = new Drive.Builder(transport, jsonFactory, credential)
      .setApplicationName(ApplicationName).build
    GoogleDrive(drive)
  }
}
