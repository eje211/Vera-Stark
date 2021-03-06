package edu.cmu.etc.verastark.snippet

import scala.xml.{NodeSeq, Text, Unparsed, XML}
import scala.collection.immutable.ListMap
import net.liftweb._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import java.text.SimpleDateFormat
import code.lib._
import sitemap._
import Helpers._
import textile._
import mapper.By
import js.jquery.JqJsCmds.FadeOut

import java.io._
import java.awt.Dimension
import java.awt.Rectangle
import java.awt.image.BufferedImage
import java.awt.image.RasterFormatException
import javax.imageio.ImageIO

import edu.cmu.etc.verastark.model._
import edu.cmu.etc.verastark.lib.{Gravatar, RenderUser, TimeAgoInWords}
import edu.cmu.etc.verastark.lib.ModerateEnum._

class ArtifactParam                   extends VeraObject
case object ArtifactList              extends ArtifactParam
case class  ArtifactPage(a: Artifact) extends ArtifactParam
case object ArtifactNotFound          extends ArtifactParam

object ArtifactPageMenu {
  def parse(value: String) =
    try {
      Artifact.find(By(Artifact.id, value.toInt)).map( a => ArtifactPage(a)) or Full(ArtifactNotFound)
    } catch {
      case nfe: NumberFormatException => {
        if (value != "index")
          S.notice("We're sorry. We couldn't find the artifact you specified.")
        Full(ArtifactList)
      }
    }

  def encode(ap: ArtifactParam) = ap match {
    case ArtifactPage(a) => a.id.toString
    case ArtifactList    => "index"
  }

  val menu = Menu.param[ArtifactParam]("Artifact", "Artifact", parse _, encode _) / "artifact" 
  lazy val loc = menu.toLoc

  def render = "*" #> loc.currentValue.map(encode(_))
}

class ArtifactEditForm(ap:ArtifactPage) {
  def render = {
    var title   = ""
    var artist  = ""
    var content = ""
    var date    = ""
    var apdate  = new Date
    def processArtifact = {
      ap.a.title(title).artist(artist).content(content).date(date).changed(now).app_date(apdate).save
      S.redirectTo("/artifact/" + ap.a.id.toString)
    }
    def deleteArtifact = {
      ap.a.deleted(!ap.a.deleted.is).save
      S.notice("Artifact \"" + ap.a.title.is + "\" is now marked for " + (if (ap.a.deleted.is) "" else "un") +  "deletion.")
    }

    ".title"          #> SHtml.onSubmit(title = _)   &
    ".title [value]"  #> ap.a.title.is               &
    ".artist"         #> SHtml.onSubmit(artist = _)  &	
    ".artist [value]" #> ap.a.artist.is              &
    ".description"    #> SHtml.onSubmit(content = _) &
    ".description *"  #> ap.a.content.is             &
    ".date"           #> SHtml.onSubmit(date = _)    &
    ".date [value]"   #> ap.a.date.is                &
    ".apdate"         #> SHtml.onSubmit((s: String) => 
      apdate = new SimpleDateFormat("y/M/dd").parse(s)) &
    ".apdate [value]" #> ap.a.app_date.toString      &
    "type=submit"     #> SHtml.submit("Submit", processArtifact _) &
    "type=button"     #> SHtml.submit(if(ap.a.deleted.is) "Undelete" else "Delete", deleteArtifact _)
  }
}

class ArtifactContainerSnippet(ap: ArtifactParam) {
  def render = ap match {
    case ArtifactList     => renderList
    case ArtifactPage(p)  => renderPage
    case ArtifactNotFound => renderNotFound
  }

  def renderList =
    "#artifact_list ^^"      #> "*"           &
    "#artifact_list [class]" #> "static_page" &
    "#artifact_list [id]"    #> "content"

  def renderPage =
    "#artifact_item ^^"      #> "*"               &
    "#artifact_item [id]"    #> "content"         &
    "#artifact_item [class]" #> "without_sidebar"

  def renderNotFound =
    "#artifact_none ^^"      #> "*"           &
    "#artifact_none [class]" #> "static_page" &
    "#artifact_none [id]"    #> "content"

}

class ArtifactSnippet(ap: ArtifactPage) {
  def render =
    ".artist *"              #> ap.a.artist.is                        &
    ".title *"               #> ap.a.title.is                         &
    ".date *"                #> ap.a.date.is                          &
    "#authorimg"             #> Gravatar.gravatar(ap.a.owner, 60)     &
    "#profilelink [href]"    #> RenderUser(ap.a.owner)                &
    ".description *"         #> TextileParser.toHtml(ap.a.content.is) &
    ".authorName *"          #> (ap.a.owner.map(_.niceName) openOr "Unknown") &
    ".authorName [href]"     #> RenderUser(ap.a.owner)                &
    ".artifact-comments *"   #> Comment.findAll(By(Comment.art_id, ap.a.id)).map(c => {
      def ownOrSuper =
        User.currentUser.map(u => u.superUser.is || u.editor.is ||
        (c.owner.map(_.id.is == u.id.is) openOr false)) openOr false
      ".comment [id]"        #> "comment-id-%s".format(c.id.is)               &
      ".comment-icon [href]" #> RenderUser(c.owner)                           &
      "img"                  #> Gravatar.gravatar(c.owner, 50)                &
      ".comment-content"     #> c.content                                     &
      ".comment-user [href]" #> RenderUser(c.owner)                           &
      ".comment-user *"      #> Text(c.owner.map(_.niceName) openOr "Annonymous") &
      ".comment_age"         #> TimeAgoInWords(c.date.is)                     &
      ".report [class+]"     #> (if(User.loggedIn_?) "" else "clearable") andThen
      ClearClearable                                                          &
      ".report [onclick]"    #> SHtml.ajaxInvoke(() => {
        if (User.loggedIn_?) c.owner.map(u => {
          u.report(u.report + 1).save
          User.currentUser.map(u => u.whistle(u.whistle + 1).save)
          S.notice("User %s was reported".format(u.niceName))
        })
        ()
      })                                                                      &
      ".delete_text [class+]" #> (if (ownOrSuper) "" else "clearable")  andThen
      ClearClearable &
      ".comment_delete [onclick]" #> SHtml.ajaxInvoke(() => {
        if (ownOrSuper) {
          c.delete_!
          FadeOut( "comment-id-%s".format(c.id.is), new TimeSpan(0), new TimeSpan(500))} else ()
        }
      )
    }) &
    "#comment-field [class+]" #> (if (User.currentUser isEmpty) "clearable" else "") andThen ClearClearable &
    "#comment-login [class+]" #> (if (User.currentUser isEmpty) "" else "clearable") andThen ClearClearable
}

class ArtifactImage(ap: ArtifactPage) {
  def render =
    if(ap.a.filetype.is.startsWith("external/"))
      "img" #> {Unparsed(ap.a.url.is)}
    else
      "img [src]" #> ap.a.url.is
}

class NewArtifact {
  def render = {
    var fileHandler: Box[FileParamHolder] = Empty
    var videoUrl:Box[String] = Empty
    def saveArtifact = {
      getVideo
      if (videoUrl isEmpty) saveImage
    }
    def getVideo = {
      val providers = ListMap(
        "vimeo" -> """<iframe src="http://player.vimeo.com/video/%s" width="500" height="281" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>""",
        "youtube" -> """<iframe width="560" height="315" src="http://www.youtube.com/embed/%s" frameborder="0" allowfullscreen></iframe>"""
      )
      def testProvider(p: scala.util.matching.Regex, url: String): Box[String] =
        try {
          val p(_, _, id, _) = url
          if (id == "") Empty else Full(id)
        } catch {case e: MatchError => (Empty)}
      var id: Box[List[String]] = Empty
      List(
        List("vimeo", """(http://)?(www\.)?vimeo\.com/(\d+)()?"""),
        List("youtube", """(http://)?(www\.)?youtube\.com/watch\?v=([^&]+)(&.*)?""")
      ) foreach {(p) => {
        val res = testProvider(p(1).r, videoUrl openOr "")
        id = if (res isDefined) Full(List(p(0), res open_!)) else id
      }}
      videoUrl = id.map(i => providers(i(0)).format(i(1)))
      if (videoUrl isDefined) {
        var art = Artifact.create
        art.url(videoUrl open_!).title("New Video").
          filename(id.map(_(1)) openOr "").filetype("external/"+(id.map(_(0)) openOr "")).deleted(false).genuine(true).created(now).
          ownerid(User.currentUser.map(_.id) open_!).app_date(new Date(71035201)).save
        S.redirectTo("/artifact/" + art.id + "#edit")
      }
    }
    def saveImage = fileHandler match {
      // First of all, see if there was a video link specified
      case Full(file: FileParamHolder) if (file.mimeType.startsWith("image/") || file.length <= 1024*1024*2) => {
        var art = Artifact.create
        // Not super efficient to save now, but we need the artifact's ID for the rest.
        art.save
        val filename = "0" * (5 - art.id.toString.length) + art.id + "." + file.fileName
        art.url("/upload/" + filename).title(file.fileName.slice(0, file.fileName.lastIndexOf(".")).replace('_', ' ')).
          filename(filename).filetype(file.mimeType).deleted(false).genuine(true).created(now).
          ownerid(User.currentUser.map(_.id) open_!).app_date(new Date(71035201)).save // Default date set to Apr 2 1972, near Vear's disapearance
        var f = new File("/var/images/" + filename)
        if (!f.exists) {
          f.createNewFile
          var output = new FileOutputStream(f)
          try {output.write(file.file)}
          catch {case e => println(e)}
          finally {output.close; output = null}
          S.redirectTo("/artifact/" + art.id + "#edit")
        }
        S.notice("We're sorry. There's been a problem processing your artifact. Please try again. If that still doesn't work, contant us.")
      }
      case Full(file: FileParamHolder) if (file.mimeType.startsWith("image/") && file.length > 1024*1024*2) =>
        S.notice("The image you tried to upload is over 2 megabytes." +
          "Please try to make the image smaller by using stronger compression or reducing the image's size.")
      case Full(file: FileParamHolder) if (!file.mimeType.startsWith("image/")) =>
        S.notice("The image you tried to upload doesn't seem to be an image. Please check it is an image and try again.")
      case _ => S.notice("We're sorry. There's been a problem processing your artifact. Please try again. If that still doesn't work, contant us.")
    }

    "type=file"              #> SHtml.fileUpload(fph => fileHandler = Full(fph))      &
    "type=text"              #> SHtml.onSubmit(s => videoUrl = Full(s)) &
    "type=submit"            #> SHtml.submit("Send artifact", saveArtifact _)
  }

  private def makeThumbnail(in: File, fileName: String, fileType: String): Unit = {
    val img = ImageIO.read(in)
    val height = if (img.getHeight <= 133) img.getHeight else 133
    val width  = if (img.getWidth <= 200) img.getWidth else 200
    val thumb = img.getSubimage(
      if (width  <= 133) 0 else (width - 200) / 2,
      if (height <= 200) 0 else (height - 133) / 3,
      width, height)
    var outputFile = new File("/var/images/thumbs/%s".format(fileName))
    ImageIO.write(thumb, fileType, outputFile)
  }
}

class ContributeFilter {
  def render =
    "#no_user [class+]"      #> (if (User.currentUser isEmpty) "" else "clearable") &
    "#new_artifact [class+]" #> (if (User.currentUser isEmpty) "clearable" else "") andThen
    "*"                      #> ClearClearable
}

class ArtifactList {
  def render =
    "ul *" #> Artifact.findAllByPreparedStatement({superconn =>
      superconn.connection.prepareStatement(
        "SELECT * FROM artifact a " /* +
        "JOIN users current ON current.id = " + (User.currentUserId openOr "1") + " " +
        "JOIN users aowner ON a.ownerid = aowner.id " +
        "WHERE a.published = TRUE OR " +
        "aowner.id = current.id OR " +
        "current.superuser = TRUE OR " +
        "current.editor = TRUE GROUP BY a.id" */
    )}).flatMap(
      a => <li><a href={"/artifact/" + a.id}>{a.title}</a></li>
    ) & ClearClearable
}

class CommentField(ap: ArtifactPage) {
  def render = {
    var content   = ""
    var art_id    = 0
    var published = Published
    def processComment = 
      if (content.length > 0 && content != "Leave a comment...") {
        Comment.create.content(content).ownerid(User.currentUser.map(_.id) open_!).date(now).art_id(ap.a.id).published(Published).save
        S.redirectTo("/artifact/" + ap.a.id.toString + "#talk")
      }

    "textarea"    #> SHtml.onSubmit(content = _) &
    "type=submit" #> SHtml.submit("Write Comment", processComment _)
  }
}

class BrowseCollection {
  def render = 
    ClearClearable &
    ".thumb" #> Artifact.findAll().map(a =>
      "* ^*" #> "" &
      "a [href]" #> "/artifact/%s#talk".format(a.id.is) &
      "a [title]" #> a.title.is &
      "img [src]" #> (if(a.filetype.startsWith("external/")) a.filetype.is.split('/')(1) match {
          case "youtube" => "http://img.youtube.com/vi/%s/2.jpg".format(a.filename)
          case "vimeo" =>
            (XML.load(new java.net.URL("http://vimeo.com/api/v2/video/%s.xml".format(a.filename)).openStream) \\ "thumbnail_medium").text 
          case _ => ""
        } 
        else {
          val f = new File("/var/images/thumbs/%s".format(a.filename))
          if (!f.exists) createThumb(a, f)
          "/thumbnails/%s".format(a.filename)
          }
        )
    )
  def createThumb(a: Artifact, f: File) = {
    val img = ImageIO.read(new File("/var/images/%s".format(a.filename)))
    val width  = if (img.getWidth <= 200) img.getWidth else 200
    val height = if (img.getHeight <= 133) img.getHeight else 133
    val x = if (width  < 200) 0 else clamp((img.getWidth / 3) - 100, 0, img.getWidth -width)
    val y = if (height < 133) 0 else clamp((img.getHeight / 3) - 66, 0, img.getHeight - height)
    val thumb = img.getSubimage(x, y, width, height)
    ImageIO.write(thumb, a.filetype.split("/")(1), f)
  }
  def clamp(v: Int, min: Int, max: Int) = if (v < min) min else if (v > max) max else v
}
