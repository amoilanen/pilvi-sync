package pilvi

import java.io.{File => JFile}
import java.io.{IOException, InputStreamReader}
import java.util.Collections

import scala.collection.JavaConverters._
import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.drive.model.{File, FileList}
import com.google.api.services.drive.{Drive, DriveScopes}

case class GoogleDrive(d: Drive) {

  def getFileId(parentFolderId: String, fileName: String): Option[String] = {
    val searchResult: FileList = d.files.list().set("q", s"'${parentFolderId}' in parents and name='${fileName}' and trashed = false").execute()
    val foundFiles = searchResult.getFiles.asScala.toList
    foundFiles.headOption.map(_.getId)
  }

  def getFileId(filePath: String): Option[String] = {
    val pathParts = filePath.split('/')

    val fileId: Option[String] = pathParts.foldLeft(Option("root"))({
      case (Some(parentFolderId), pathPart) => getFileId(parentFolderId, pathPart)
      case (None, pathPart) => None
    })

    fileId
  }

  def listFiles(filePath: String): List[File] = {
    val fileId: Option[String] = getFileId(filePath)
    println(fileId)
    val foundFiles = fileId match {
      case Some(id) => d.files.list().set("q", s"'${id}' in parents and trashed = false").execute().getFiles().asScala.toList
      case None => List()
    }
    foundFiles
  }

  def exists(filePath: String): Boolean =
    getFileId(filePath).isDefined
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
      .setDataStoreFactory(new FileDataStoreFactory(new JFile(TokensDirectoryPath))).setAccessType("offline").build
    val receiver = new LocalServerReceiver.Builder().setPort(VerificationCodeReceiverPort).build
    new AuthorizationCodeInstalledApp(flow, receiver).authorize("user")
  }

  def getDrive: GoogleDrive = {
    val jsonFactory: JacksonFactory = JacksonFactory.getDefaultInstance
    val transport = GoogleNetHttpTransport.newTrustedTransport
    val credential = getCredentials(transport, jsonFactory)
    val drive = new Drive.Builder(transport, jsonFactory, credential)
      .setApplicationName(ApplicationName).build
    GoogleDrive(drive)
  }
}
