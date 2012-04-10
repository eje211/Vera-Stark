package edu.cmu.etc.verastark.lib

import net.liftweb.http.JsonResponse
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{ InMemoryResponse, StreamingResponse }
import net.liftweb.http.S
import net.liftweb.http.FileParamHolder
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonDSL._
import net.liftweb.common.{Box, Full, Empty}
import net.liftweb.http.BadResponse
import net.liftweb.util.StringHelpers

import java.io.{InputStream, StringReader, File, FileOutputStream}

import edu.cmu.etc.verastark.model.Artifact

// import mongo.MongoStorage

object UploadManager extends RestHelper {
  serve {
    case "artifact" :: "new" :: Nil Post req => {
      println("Upload")
      def saveImage(fph: FileParamHolder) = {
        var art = Artifact.create// Not efficient but we need the ID now.
        art.save
        val uploadDir = new File(System.getProperty("user.dir") + "/main/webapp/upload/")
        val uploadingFile = new File(uploadDir, art.id.toString + fph.name)
        var output = new FileOutputStream(uploadingFile)
        try {output.write(fph.file)}
        catch {case e => println(e)}
        finally {output.close; output = null}
        art.title(fph.name).save
        ("name" -> fph.name) ~ ("type" -> fph.mimeType) ~ ("size" -> fph.length)
      }

      val ojv: Box[JValue] = req.uploadedFiles.map(fph => saveImage(fph)).headOption
      val ajv = ("name" -> "n/a") ~ ("type" -> "n/a") ~ ("size" -> 0L)
      val ret = ojv openOr ajv
     
      val jr = JsonResponse(ret).toResponse.asInstanceOf[InMemoryResponse]
      InMemoryResponse(jr.data, ("Content-Length", jr.data.length.toString) ::
        ("Content-Type", "text/plain") :: Nil, Nil, 200)
      }

    case "serving" :: imageName :: Nil Get req => {
      /*
      MongoStorage.mongoGridFS.findOne(imageName) match {
        case Some(image) =>
          val imageStream = image.inputStream
          StreamingResponse(imageStream, () => imageStream.close(), image.length, ("Content-Type", image.contentType) :: Nil, Nil, 200)
        case _ => new BadResponse
      }
      */
      InMemoryResponse(Array[Byte](), ("Content-Type", "image/png") :: Nil, Nil, 200)
    }
  }
}
