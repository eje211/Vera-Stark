package edu.cmu.etc.verastark.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb._
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._
import code.lib._
import sitemap._
import Helpers._
import mapper.{By, OrderBy, Ascending}
import textile._
import js.jquery.JqJsCmds.FadeOut

import java.lang.{Integer, NumberFormatException}
import java.util.Date
import java.text.SimpleDateFormat

import edu.cmu.etc.verastark.model._
import edu.cmu.etc.verastark.lib.{Gravatar, RenderUser}
import edu.cmu.etc.verastark.lib.ModerateEnum._

class AutobiographyParam                           extends VeraObject
case class  AutobiographyPage(a: Autobiography) extends AutobiographyParam
case object AutobiographyIndex                     extends AutobiographyParam
case object AutobiographyNew                       extends AutobiographyParam
case object AutobiographyNotFound                  extends AutobiographyParam

object AutobiographyPageMenu {
  def parse(id: String) = id match {
    case "" | "index" => Full(AutobiographyIndex)
    case "new"        => Full(AutobiographyNew)
    case _            => {
      try {
        var page = Autobiography.find(By(Autobiography.id, Integer.parseInt(id)))
        page match {
          case Full(page) => Full(AutobiographyPage(page))
          case _          => Full(AutobiographyIndex)
        }
      }
      catch { // Default behavior: go to index
        case e: NumberFormatException => Full(AutobiographyIndex)
      }
    }
  }
  def encode(ap: AutobiographyParam) = ap match {
    case AutobiographyPage(id) => id.toString
    case AutobiographyNew      => "new"
    case AutobiographyNotFound => "index"
    case _                     => "index"
  }

  val menu = Menu.param[AutobiographyParam]("Autobiography", "Autobiography", parse _, encode _) / "autobiography"
  lazy val loc = menu.toLoc

  def render = "*" #> "Autobiography" // loc.currentValue.map(_.pageName)
}

class AutobiographyContainer(ap: AutobiographyParam) {
  def render = ap match {
    case AutobiographyIndex   => renderIndex
    case AutobiographyNew     => renderNew
    case AutobiographyPage(a) => renderPage
  }

  def renderIndex =
    "#autobiography_index ^^"          #> "*"                &
    "#autobiography_index [class+]"    #> "static_page"      &
    "#autobiography_index [id]"        #> "content"
  def renderNew   =
    "#autobiography_new ^^"            #> "*"                &
    "#autobiography_new [id]"          #> "content"          &
    "#autobiography_new_form [class+]" #> (if(User.currentUser isEmpty) "clearable" else "") &
    "#not_logged_in [class+]"          #> (if(User.currentUser isEmpty) "" else "clearable") andThen
    ClearClearable
  def renderPage  =
    "#autobiography_page ^^"           #> "*"                &
    "#autobiography_page [class+]"     #> "without_sidebars" &
    "#autobiography_page [id]"         #> "content"
}

class AutobiographyPageSnippet(ap:AutobiographyPage) {
  def render =
    //"h2 *" #> a.title.is &
    "#autobiography_text *"  #> TextileParser.toHtml(ap.a.content.is)
}

class AutobiographyList {
  def render =
    "ul *" #> Autobiography.findAll(By(Autobiography.published, Published), OrderBy(Autobiography.app_date, Ascending)
    ).flatMap(
      a => <li><a href={"/autobiography/" + a.id}>{a.title}</a></li>
    ) & ClearClearable
}

class AutobiographyNewForm {
  var title  = ""
  var text   = ""
  var date   = ""
  var apdate: Date = _
  def processAutobiography = {
    Autobiography.create.title(title).content(text).date(date).created(now).
    app_date(apdate).ownerid(User.currentUserId.map(_.toInt) openOr 0).
    changed(now).published(Published).deleted(false).genuine(false).save
    S.redirectTo("/autobiography/index")
  }
    
  def render =
    "name=autobiography_title" #> SHtml.onSubmit(title  = _) &
    "name=text_body"           #> SHtml.onSubmit(text   = _) &
    "name=text_date"           #> SHtml.onSubmit(date   = _) &
    "name=text_app_date"       #> SHtml.onSubmit((s: String) => 
      apdate = new SimpleDateFormat("y/M/dd").parse(s))      &
    "type=submit"              #> SHtml.submit("Submit the page", processAutobiography _)
             
}

class AutobiographyTalkSnippet(ap: AutobiographyPage) {
  def render = 
    ".title *"                   #> ap.a.title.is                         &
    ".time_period *"             #> ap.a.date.is                          &
    ".description [class+]"      #> (if(ap.a.description.is.length == 0) "clearable" else "") &
    "#profilelink [href]"        #> RenderUser(ap.a.owner)                &
    "#authorimg"                 #> Gravatar.gravatar(ap.a.owner, 60)     &
    ".description *"             #> TextileParser.toHtml(ap.a.description.is) &
    ".author *"                  #> (ap.a.owner.map(u => u.firstName + " " + u.lastName) openOr "Unknown") &
    "#autobiography-annotations *" #> Annotation.findAll(By(Annotation.bio_id, ap.a.id)).map(a => {
      def ownOrSuper =
        User.currentUser.map(u => u.superUser.is || u.editor.is ||
        (a.owner.map(_.id.is == u.id.is) openOr false)) openOr false
      ".comment [id]"        #> "comment-id-%s".format(a.id.is)               &
      ".comment-icon [href]" #> RenderUser(a.owner)                           &
      "img"                  #> Gravatar.gravatar(a.owner, 50)                &
      ".comment-content"     #> a.content                                     &
      ".comment-user [href]" #> RenderUser(a.owner)                           &
      ".comment-user *"      #> Text(a.owner.map(_.niceName) openOr "Annonymous") &
      ".comment_age"         #> Text("Today")                                 &
      ".delete_text [class+]" #> (if (ownOrSuper) "" else "clearable")  andThen
      ClearClearable &
      ".comment_delete [onclick]" #> SHtml.ajaxInvoke(() => {
        if (ownOrSuper) {
          a.delete_!
          FadeOut( "comment-id-%s".format(a.id.is), new TimeSpan(0), new TimeSpan(500))} else ()
        }
      )
    }) &
    "#annotation-login [class+]" #> (if (User.currentUserId isEmpty) "" else "clearable") andThen ClearClearable &
    "#annotation-form [class+]"  #> (if (User.currentUserId isEmpty) "clearable" else "") andThen ClearClearable
}

class AnnotationField(ap: AutobiographyPage) {
  def render = {
    var content   = ""
    var art_id    = 0
    var published = Published
    def processAnnotation = 
      if (content.length > 0) {
        Annotation.create.content(content).ownerid(User.currentUserId.map(_.toInt) open_!).date(now).bio_id(ap.a.id).published(Published).save
        S.redirectTo("/autobiography/" + ap.a.id + "#talk")
      }

    "textarea"    #> SHtml.onSubmit(content = _) &
    "type=submit" #> SHtml.submit("Annotate this page", processAnnotation _)
  }
}

class AutobiographyEditForm(ap:AutobiographyPage) {
  def render = {
    var title       = ""
    var artist      = ""
    var content     = ""
    var description = ""
    var date        = ""
    var apdate      = new Date
    def processAutobiography = {
      ap.a.title(title).content(content).description(description).date(date).changed(now).app_date(apdate).save
      S.redirectTo("/autobiography/" + ap.a.id.toString)
    }
    def deleteAutobiography = {
      ap.a.deleted(!ap.a.deleted.is).save
      S.notice("The page of Vera's autobiogrpahy called \"" + ap.a.title.is + "\" is now marked for " + (if (ap.a.deleted.is) "" else "un") +  "deletion.")
    }

    ".title"          #> SHtml.onSubmit(title = _)       &
    ".title [value]"  #> ap.a.title.is                   &
    ".description"    #> SHtml.onSubmit(description = _) &
    ".description *"  #> ap.a.description.is             &
    ".content"        #> SHtml.onSubmit(content = _)     &
    ".content *"      #> ap.a.content.is                 &
    ".date"           #> SHtml.onSubmit(date = _)        &
    ".date [value]"   #> ap.a.date.is                    &
    ".apdate"         #> SHtml.onSubmit((s: String) => 
      apdate = new SimpleDateFormat("y/M/dd").parse(s))  &
    ".apdate [value]" #> ap.a.app_date.toString          &
    "type=submit"     #> SHtml.submit("Submit", processAutobiography _) &
    "type=button"     #> SHtml.submit(if(ap.a.deleted.is) "Undelete" else "Delete", deleteAutobiography _)
  }
}

class AutobiographySidebars(ap: AutobiographyParam) {
  def render = ap match {
    case a: AutobiographyPage => "*" #> ClearClearable
    case _ => ".sidebar [class+]" #> "clearable" andThen ClearClearable
  }
}
