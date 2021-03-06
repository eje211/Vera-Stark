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
import edu.cmu.etc.verastark.lib.{Gravatar, RenderUser, TimeAgoInWords}
import edu.cmu.etc.verastark.lib.ModerateEnum._

class NotebookParam                           extends VeraObject
case class  NotebookPage(a: Notebook)         extends NotebookParam
case object NotebookIndex                     extends NotebookParam
case object NotebookNew                       extends NotebookParam
case object NotebookNotFound                  extends NotebookParam

object NotebookPageMenu {
  def parse(id: String) = id match {
    case "" | "index" => Full(NotebookIndex)
    case "new"        => Full(NotebookNew)
    case _            => {
      try {
        var page = Notebook.find(By(Notebook.id, Integer.parseInt(id)))
        page match {
          case Full(page) => Full(NotebookPage(page))
          case _          => Full(NotebookIndex)
        }
      }
      catch { // Default behavior: go to index
        case e: NumberFormatException => Full(NotebookIndex)
      }
    }
  }
  def encode(ap: NotebookParam) = ap match {
    case NotebookPage(id) => id.toString
    case NotebookNew      => "new"
    case NotebookNotFound => "index"
    case _                     => "index"
  }

  val menu = Menu.param[NotebookParam]("Scrapbook", "Scrapbook", parse _, encode _) / "scrapbook"
  lazy val loc = menu.toLoc

  def render = "*" #> "Scrapbook" // loc.currentValue.map(_.pageName)
}

class NotebookContainer(ap: NotebookParam) {
  def render = ap match {
    case NotebookIndex   => renderIndex
    case NotebookNew     => renderNew
    case NotebookPage(a) => renderPage
  }

  def renderIndex =
    "#notebook_index ^^"          #> "*"                &
    "#notebook_index [class+]"    #> "static_page"      &
    "#notebook_index [id]"        #> "content"
  def renderNew   =
    "#notebook_new ^^"            #> "*"                &
    "#notebook_new [id]"          #> "content"          &
    "#notebook_new_form [class+]" #> (if(User.currentUser isEmpty) "clearable" else "") &
    "#not_logged_in [class+]"          #> (if(User.currentUser isEmpty) "" else "clearable") andThen
    ClearClearable
  def renderPage  =
    "#notebook_page ^^"           #> "*"                &
    "#notebook_page [class+]"     #> "without_sidebars" &
    "#notebook_page [id]"         #> "content"
}

class NotebookPageSnippet(ap:NotebookPage) {
  def render =
    //"h2 *" #> a.title.is &
    "#notebook_text *"  #> TextileParser.toHtml(ap.a.content.is)
}

class NotebookList {
  def render =
    "ul *" #> Notebook.findAll(By(Notebook.published, Published), OrderBy(Notebook.app_date, Ascending)
    ).flatMap(
      a => <li><a href={"/notebook/" + a.id}>{a.title}</a></li>
    ) & ClearClearable
}

class NotebookNewForm {
  var title  = ""
  var text   = ""
  var date   = ""
  var apdate: Date = _
  var validated = true
  def problemfield(name: Tuple2[String, String]) = if (name._2.length == 0) {
    S.notice("The field \"%s\" must be filled!".format(name._1))
    validated = false
  }
  def processNotebook = {
    List(("Tile", title), ("Description", text), ("Date", date)) foreach {problemfield _}
    if (validated)
    Notebook.create.title(title).content(text).date(date).created(now).
    app_date(apdate).ownerid(User.currentUserId.map(_.toInt) openOr 0).
    changed(now).published(Published).deleted(false).genuine(false).save
    S.redirectTo("/notebook/index")
  }
    
  def render =
    "name=notebook_title" #> SHtml.onSubmit(title  = _) &
    "name=text_body"           #> SHtml.onSubmit(text   = _) &
    "name=text_date"           #> SHtml.onSubmit(date   = _) &
    "name=text_app_date"       #> SHtml.onSubmit((s: String) => 
      apdate = new SimpleDateFormat("y/M/dd").parse(s))      &
    "type=submit"              #> SHtml.submit("Submit the page", processNotebook _)
             
}

class NotebookTalkSnippet(ap: NotebookPage) {
  def render = 
    ".title *"                   #> ap.a.title.is                         &
    ".time_period *"             #> ap.a.date.is                          &
    ".description [class+]"      #> (if(ap.a.description.is.length == 0) "clearable" else "") &
    "#profilelink [href]"        #> RenderUser(ap.a.owner)                &
    "#authorimg"                 #> Gravatar.gravatar(ap.a.owner, 60)     &
    ".description *"             #> TextileParser.toHtml(ap.a.description.is) &
    ".author *"                  #> (ap.a.owner.map(u => u.firstName + " " + u.lastName) openOr "Unknown") &
    "#notebook-margin-notes" #> MarginNote.findAll(By(MarginNote.bio_id, ap.a.id)).map(c => {
      def ownOrSuper =
        User.currentUser.map(u => u.superUser.is || u.editor.is ||
        (c.owner.map(_.id.is == u.id.is) openOr false)) openOr false
      ".comment [id]"        #> "comment-id-%s".format(c.id.is)               &
      ".comment-icon [href]" #> RenderUser(c.owner)                           &
      "img"                  #> Gravatar.gravatar(c.owner, 50)                &
      ".comment-content"     #> c.content                                     &
      ".comment-user [href]" #> RenderUser(c.owner)                           &
      ".comment-user *"      #> Text(c.owner.map(_.niceName) openOr "Annonymous") &
      ".comment_age"         #> TimeAgoInWords(c.date)                        &
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
      })
    }) &
    "#margin-note-login [class+]" #> (if (User.currentUserId isEmpty) "" else "clearable") andThen ClearClearable &
    "#margin-note-form [class+]"  #> (if (User.currentUserId isEmpty) "clearable" else "") andThen ClearClearable
}

class MarginNoteField(ap: NotebookPage) {
  def render = {
    var content   = ""
    var art_id    = 0
    var published = Published
    def processMarginNote = 
      if (content.length > 0 && content != "Leave a comment...") {
        MarginNote.create.content(content).ownerid(User.currentUserId.map(_.toInt) open_!).date(now).bio_id(ap.a.id).published(Published).save
        S.redirectTo("/notebook/" + ap.a.id + "#talk")
      }

    "textarea"    #> SHtml.onSubmit(content = _) &
    "type=submit" #> SHtml.submit("Write a note in the margin", processMarginNote _)
  }
}

class NotebookEditForm(ap:NotebookPage) {
  def render = {
    var title       = ""
    var artist      = ""
    var content     = ""
    var description = ""
    var date        = ""
    var apdate      = new Date
    def processNotebook = {
      ap.a.title(title).content(content).description(description).date(date).changed(now).app_date(apdate).save
      S.redirectTo("/notebook/" + ap.a.id.toString)
    }
    def deleteNotebook = {
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
    "type=submit"     #> SHtml.submit("Submit", processNotebook _) &
    "type=button"     #> SHtml.submit(if(ap.a.deleted.is) "Undelete" else "Delete", deleteNotebook _)
  }
}

class NotebookSidebars(ap: NotebookParam) {
  def render = ap match {
    case a: NotebookPage => "*" #> ClearClearable
    case _ => ".sidebar [class+]" #> "clearable" andThen ClearClearable
  }
}
