package edu.cmu.etc.verastark.snippet

import scala.xml.{NodeSeq, Text}
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

import edu.cmu.etc.verastark.model._
import edu.cmu.etc.verastark.lib.{Gravatar, RenderUser}
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
      ".comment_age"         #> Text("Today")                                 &
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
    "img [src]" #> ap.a.url.is
}

class NewArtifact {
  def render = {
    var fileHandler: Box[FileParamHolder] = Empty
    def saveImage = fileHandler match {
      case Full(file: FileParamHolder) if (file.mimeType.startsWith("image/")) => {
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
        S.notice("We're sorry. There's been a problem processing your artifact upload. Please try again. If that still doesn't work, contant us.")
      }
      case _ => S.notice("We're sorry. There's been a problem processing your artifact. Please try again. If that still doesn't work, contant us.")
    }

    "type=file"              #> SHtml.fileUpload(fph => fileHandler = Full(fph))      &
    "type=submit"            #> SHtml.submit("Send artifact", saveImage _)
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
      if (content.length > 0) {
        Comment.create.content(content).ownerid(User.currentUser.map(_.id) open_!).date(now).art_id(ap.a.id).published(Published).save
        S.redirectTo("/artifact/" + ap.a.id.toString + "#talk")
      }

    "textarea"    #> SHtml.onSubmit(content = _) &
    "type=submit" #> SHtml.submit("Write Comment", processComment _)
  }
}
