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
import net.liftweb.util.Helpers._
import net.liftweb.mapper.By

import java.io._
import javax.activation.MimetypesFileTypeMap;

import edu.cmu.etc.verastark.lib.ModerateEnum._
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
    case "upload" :: imageName :: _ Get req => {
      val art = Artifact.find(By(Artifact.filename, imageName))
      val f = new File("/var/images/" + imageName)
      f.exists && (art.map(_.published == Published) openOr false) match {
        case true  => {
          val fis = new FileInputStream(f)
          StreamingResponse(fis, () => fis.close, f.length,
            List("Content-Type" -> (art.map(_.filetype.is) openOr "image/png")), Nil, 200)
        }
        case false => InMemoryResponse(Array(), ("Content-Type", "text/plain") :: Nil, Nil, 404)
      }
    }
    case "thumbnails" :: imageName :: _ Get req => {
      val art = Artifact.find(By(Artifact.filename, imageName))
      val f = new File("/var/images/thumbs/" + imageName)
      f.exists && (art.map(_.published.is == Published) openOr false) match {
        case true  => {
          val fis = new FileInputStream(f)
          StreamingResponse(fis, () => fis.close, f.length,
            List("Content-Type" -> (art.map(_.filetype.is) openOr "image/png")), Nil, 200)
        }
        case false => InMemoryResponse(Array(), ("Content-Type", "text/plain") :: Nil, Nil, 404)
      }
    }
  }
}
