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

import scala.collection.immutable
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

case class GoogleDrive(d: Drive)(implicit ec: ExecutionContext) {

  import FileParts._

  def getFileIdInParentFolder(parentFolderId: FileId, fileName: FileName): Future[Option[FileId]] = Future {
    val searchResult: FileList = d.files.list().set("q", s"'${parentFolderId.value}' in parents and name='${fileName.value}' and trashed = false").execute()
    val foundFiles = searchResult.getFiles.asScala.toList
    foundFiles.headOption.map(_.getId).map(FileId(_))
  }

  def getFileId(filePath: FilePath): Future[Option[FileId]] = {
    val pathParts: List[FileName] = filePath.pathParts

    val fileId = pathParts.foldLeft(Future.successful(Option(FileId("root")))) { case (parentFolderId, pathPart) =>
      parentFolderId.flatMap {
        case Some(FileId(folderId)) => getFileIdInParentFolder(FileId(folderId), pathPart)
        case None => Future.successful(None)
      }
    }
    fileId
  }

  def listFiles(filePath: FilePath): Future[List[File]] = {
    val fileId: Future[Option[FileId]] = getFileId(filePath)

    val foundFiles: Future[List[File]] = fileId.map {
      case Some(id) => d.files.list().set("q", s"'${id.value}' in parents and trashed = false").execute().getFiles().asScala.toList
      case None => List()
    }
    foundFiles
  }

  def uploadFile(parentFolderPath: FilePath, file: LocalFile): Future[FileId] =
    getFileId(parentFolderPath).flatMap { parentFileId =>
      val fileMetadata = new File()
      fileMetadata.setName(file.getName)
      fileMetadata.setParents(parentFileId.map(_.value).toList.asJava)

      val mediaContent = new FileContent("text/plain", file)
      val createdFile: Future[File] = Future {
        d.files().create(fileMetadata, mediaContent)
          .setFields("id")
          .execute()
      }
      createdFile.map(file => FileId(file.getId))
    }

  def exists(filePath: FilePath): Future[Boolean] =
    getFileId(filePath).map(_.isDefined)

  def ensureFolderExistsInParentFolder(parentFolderId: FileId, folderName: FileName): Future[FileId] = {
    getFileIdInParentFolder(parentFolderId, folderName).flatMap { folderId =>
      if (!folderId.isDefined) {
        val fileMetadata = new File()
        fileMetadata.setName(folderName.value)
        fileMetadata.setParents(List(parentFolderId.value).asJava)
        fileMetadata.setMimeType("application/vnd.google-apps.folder")
        Future {
          val file = drive.d.files.create(fileMetadata).setFields("id").execute
          FileId(file.getId)
        }
      } else {
        Future.successful(folderId.get)
      }
    }
  }

  def ensureExists(folderPath: FilePath): Future[FileId] = {
    val folderId: Future[FileId] = folderPath.pathParts.foldLeft(Future.successful(FileId("root")))({ case (parentFolderId, pathPart) =>
      parentFolderId.flatMap { folderId =>
        ensureFolderExistsInParentFolder(folderId, pathPart)
      }
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
