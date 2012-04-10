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

import java.io._

import edu.cmu.etc.verastark.model._

class ArtifactParam
// case class  ArtifactID(id: Int)       extends ArtifactParam
case object ArtifactNew               extends ArtifactParam
case object ArtifactList              extends ArtifactParam
case class  ArtifactPage(a: Artifact) extends ArtifactParam
case object ArtifactNotFound          extends ArtifactParam

/*
object ArtifactidsList {
  def render(in:NodeSeq):NodeSeq =
    ArtifactTools.getPageTitles.flatMap { text => bind("page", in, "title" -> <a href={"/journal/" + text}>{text}</a>)}
}
*/

object ArtifactPageMenu {
  def parse(value: String) =
    try {
      // Repackage the box if it's full. Otherwise, send the placeholder object.
      //ArtifactTools.getArtifactById(value.toInt).map( a => ArtifactPage(a)) or Full(ArtifactNotFound)
      Artifact.find(By(Artifact.id, value.toInt)).map( a => ArtifactPage(a)) or Full(ArtifactNotFound)
      // ArtifactTools.getArtifactById(1).map( a => ArtifactPage(a)) or Full(ArtifactNotFound)
    } catch {
      case nfe: NumberFormatException =>
        if (value == "new") Full(ArtifactNew)
        else Full(ArtifactList)
    }

  def encode(ap: ArtifactParam) = ap match {
    case ArtifactPage(a) => a.id.toString
    case ArtifactNew     => "new"
    case ArtifactList    => "index"
  }

  val menu = Menu.param[ArtifactParam]("Artifact", "Artifact", parse _, encode _) / "artifact" 
  lazy val loc = menu.toLoc

  def render = "*" #> loc.currentValue.map(encode(_))
}

/*
object ArtifactHelper {
  def current_page(id: Int) =
    // Create a new page if there isn't one already
    ArtifactTools.getartifactById(id) match {
      case head :: tail => head
      case Nil => new Artifact
    }
}
*/

class ArtifactEditForm(ap:ArtifactPage) {
  def render = {
    var title   = ""
    var artist  = ""
    var content = ""
    var date    = ""
    def processArtifact = {
      ap.a.title(title).artist(artist).content(content).date(date).changed(now).save
      S.redirectTo("/artifact/" + ap.a.id.toString)
    }

    ".title"          #> SHtml.onSubmit(title = _)   &
    ".title [value]"  #> ap.a.title.is               &
    ".artist"         #> SHtml.onSubmit(artist = _)  &	
    ".artist [value]" #> ap.a.artist.is              &
    ".description"    #> SHtml.onSubmit(content = _) &
    ".description *"  #> ap.a.content.is             &
    ".date"           #> SHtml.onSubmit(date = _)    &
    ".date [value]"   #> ap.a.date.is                &
    "type=submit"     #> SHtml.submit("Submit", processArtifact _)
  }
}

/*
class ArtifactContainer(ai: Artifactid) {
  def render = ai.id match {
    case "index" => "#ArtifactidsList ^^" #> "*"
    case _       => (if (S.param("edit") isDefined)
                      "#ArtifactEditForm ^^" #> "*"
                      else "#Artifactid ^^" #> "*"
                    )
  }
}
*/

class ArtifactContainerSnippet(ap: ArtifactParam) {
  def render = ap match {
    case ArtifactList    => renderList
    case ArtifactNew     => renderNew
    case ArtifactPage(p) => renderPage
  }

  def renderList =
    "#artifact_list ^^"      #> "*"           &
    "#artifact_list [class]" #> "static_page" &
    "#artifact_list [id]"    #> "content"

  def renderPage =
    "#artifact_item ^^"      #> "*"               &
    "#artifact_item [id]"    #> "content"         &
    "#artifact_item [class]" #> "without_sidebar"

  def renderNew =
    "#artifact_new ^^"       #> "*" &
    "#artifact_new [id]"     #> "content"

}

class ArtifactSnippet(ap: ArtifactPage) {
  def render =
    ".artist *"              #> ap.a.artist.is                        &
    ".title *"               #> ap.a.title.is                         &
    ".date"                  #> ap.a.date.is                          &
    ".description *"         #> TextileParser.toHtml(ap.a.content.is) &
    ".authorName *"          #> (ap.a.owner.map(u => u.firstName + " " + u.lastName) openOr "Unknown") &
    ".artifact-comments"     #> Comment.findAll(By(Comment.art_id, ap.a.id)).flatMap(c =>
      <article class="comment">
        <a href="/profile/bradbuchanan"><img src="/img/avatar_herb.png" /></a>
        <a href="/profile/bradbuchanan">{c.owner.map(s => s.firstName + " " + s.lastName) openOr "Annonymous"}</a>
        {c.content.is}
        <p class="comment_age">Today</p>
      </article> ) & 
    "#comment-field [class+]" #> (if (User.currentUserId isEmpty) "clearable" else "") andThen
    "#comment-field"          #> ClearClearable
}

class ArtifactImage(ap: ArtifactPage) {
  def render =
    "img [src]" #> ap.a.url.is
}

class NewArtifact {
  def render = {
    var fileHandler: Box[FileParamHolder] = Empty
    def saveImage = fileHandler match {
      case Full(file: FileParamHolder) => {
        var art = Artifact.create// Not efficient but we need the ID now.
        art.save
        art.url("/upload/" + art.id.toString + ".png").title("New Artifact").save
        var f = new File(System.getProperty("user.dir") + "/src/main/webapp/upload/" + art.id.toString + ".png")
        if (!f.exists) f.createNewFile
        var output = new FileOutputStream(f)
        try {output.write(file.file)}
        catch {case e => println(e)}
        finally {output.close; output = null}
        S.redirectTo("/artifact/" + art.id.toString)
      }
      case _ => ()
    }

    "type=file"   #> SHtml.fileUpload(fph => fileHandler = Full(fph)) &
    "type=submit" #> SHtml.submit("Send artifact", saveImage _)
  }
}

class ArtifactList {
  def render =
    "ul *" #> Artifact.findAllByPreparedStatement({superconn =>
      superconn.connection.prepareStatement(
        "SELECT * FROM artifact a " /* +
        "JOIN users current ON current.id = " + (User.currentUserId.map(_.toString) openOr "1") + " " +
        "JOIN users aowner ON a.ownerid = aowner.id " +
        "WHERE a.published = TRUE OR " +
        "aowner.id = current.id OR " +
        "current.superuser = TRUE OR " +
        "current.editor = TRUE GROUP BY a.id" */
    )}).flatMap(
      a => <li><a href={"/artifact/" + a.id}>{a.title}</a></li>
    ) & ClearClearable
}

/*
object ArtifactScreen extends LiftScreen {
  object page extends ScreenVar(Artifact.create)

  addFields(() => page.is)

  val shouldSave = field("Save", false)

  def finish() {}
}
*/

class CommentField(ap: ArtifactPage) {
  def render = {
    var content   = ""
    var art_id    = 0
    var published = true
    def processComment = 
      if (content.length > 0) {
        Comment.create.content(content).ownerid(User.currentUserId.map(_.toInt) openOr 1).date(now).art_id(ap.a.id).published(true).save
        S.redirectTo("/artifact/" + ap.a.id.toString)
      }

    "textarea"    #> SHtml.onSubmit(content = _) &
    "type=submit" #> SHtml.submit("Write Comment", processComment _)
  }
}
