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

import java.lang.{Integer, NumberFormatException}
import java.util.Date
import java.text.SimpleDateFormat

import edu.cmu.etc.verastark.model._

class AutobiographyParam                           extends VeraObject
case class  AutobiographyPage(page: Autobiography) extends AutobiographyParam
case object AutobiographyIndex                     extends AutobiographyParam
case object AutobiographyNew                       extends AutobiographyParam
case object AutobiographyNotFound                  extends AutobiographyParam

object AutobiographyPageMenu {
  def parse(id: String) = id match {
    case "" | "index" => Full(AutobiographyIndex)
    case "new"        => Full(AutobiographyNew)
    case _            => {
      try {
        val iid = Integer.parseInt(id)
        var page = Autobiography.find(By(Autobiography.id, iid))
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
    "#autobiography_page [+class]"     #> "without_sidebars" &
    "#autobiography_page [id]"         #> "content"
}

object AutobiographyPagesList {
  def render(in:NodeSeq):NodeSeq =
    AutobiographyTools.getPageTitles.flatMap { text => bind("page", in, "title" -> <a href={"/journal/" + text}>{text}</a>)}
}

class AutobiographyForm(a:Autobiography) {
  var title   = ""
  var content = ""
  def render = {
    def processJournalPage() = a.title(title).content(content).
      ownerid(User.currentUserId.map(_.toInt) openOr 1).changed(now).save
  
    ".title"         #> SHtml.onSubmit(title = _)          &
    ".title [value]" #> a.title.is                         &
    ".content"       #> SHtml.onSubmit(content = _)        &
    ".content *"     #> a.content.is                       &
    "type=submit"    #> SHtml.submit("Write in Vera's Journal", processJournalPage)
  }
}

class AutobiographyPageSnippet(a:Autobiography) {
  def render =
    "h2 *" #> a.title.is &
    "p *"  #> TextileParser.toHtml(a.content.is)
}

class AutobiographyList {
  def render =
    "ul *" #> Autobiography.findAll(By(Autobiography.published, true), OrderBy(Autobiography.app_date, Ascending)
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
    Autobiography.create.title(title).content(text).date(date).
    app_date(apdate).ownerid(User.currentUserId.map(_.toInt) openOr 0).
    changed(now).published(true).save
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
