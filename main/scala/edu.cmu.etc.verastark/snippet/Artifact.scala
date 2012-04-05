package edu.cmu.etc.verastark.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import sitemap._
import Helpers._

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
      // ArtifactTools.getArtifactById(value.toInt).map( a => ArtifactPage(a)) or Full(ArtifactNotFound)
      ArtifactTools.getArtifactById(1).map( a => ArtifactPage(a)) or Full(ArtifactNotFound)
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
  // object title extends RequestVar("")
  // object content extends RequestVar("")
  def edit(xhtml:NodeSeq):NodeSeq = {
    def processArtifact() = ap.a.save
  
    bind("artprop", xhtml,
      "title"   -> ap.a.title.toForm,
      // "title"    -> SHtml.text(ap.a.title.is, ap.a.title(_), "class" -> "text title"),
      // "artist"   -> SHtml.text(ap.a.artist.is, ap.a.artist(_), "class" -> "text artist") //,
       "artist"  -> ap.a.artist.toForm,
      "content" -> ap.a.content.toForm,
      "date"    -> ap.a.date.toForm,
      "submit"  -> SHtml.submit("Submit", processArtifact)
    )
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

class ArtifactSnippet(ap: ArtifactPage) {
  def render = {
    "#artifact_image [src]" #> ap.a.url.is     &
    ".artist *"             #> ap.a.artist.is  &
    ".title *"              #> ap.a.title.is   &
    ".date"                 #> ap.a.date.is    &
    ".description *"        #> ap.a.content.is &
    ".authorName *"         #> ArtifactTools.art_owner(ap.a.owner.is.toInt).map(u => u.firstName + " " + u.lastName)
    // "p *"  #> current_page.content
  }
}

/*
object ArtifactScreen extends LiftScreen {
  object page extends ScreenVar(Artifact.create)

  addFields(() => page.is)

  val shouldSave = field("Save", false)

  def finish() {}
}
*/
