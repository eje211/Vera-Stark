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

case class AutobiographyPage(pageName: String)

object AutobiographyPagesList {
  def render(in:NodeSeq):NodeSeq =
    AutobiographyTools.getPageTitles.flatMap { text => bind("page", in, "title" -> <a href={"/journal/" + text}>{text}</a>)}
}

object AutobiographyPageMenu {
  def parse(name: String) = {if (name == "") Full(AutobiographyPage("list")) else Full(AutobiographyPage(name))}
  def encode(ap: AutobiographyPage) = ap.pageName

  val menu = Menu.param[AutobiographyPage]("Autobiography", "Autobiography", parse _, encode _) / "journal"
  lazy val loc = menu.toLoc

  def render = "*" #> loc.currentValue.map(_.pageName)
}

object AutobiographyHelper {
  def current_page(title: String) =
    // Create a new page if there isn't one already
    AutobiographyTools.getPageByTitle(title) match {
      case head :: tail => head
      case Nil => new Autobiography
    }
}

class AutobiographyForm(ap:AutobiographyPage) {
  object title extends RequestVar("")
  object content extends RequestVar("")
  var current_page = AutobiographyHelper.current_page(ap.pageName)
  def edit(xhtml:NodeSeq):NodeSeq = {
    def processJournalPage() = current_page.save
  
    bind("abpage", xhtml,
      "title"   -> current_page.title.toForm,
      "content" -> current_page.content.toForm,
      "submit"  -> SHtml.submit("Write in Vera's Journal", processJournalPage)
    )
  }
}

class AutobiographyContainer(ap: AutobiographyPage) {
  def render = ap.pageName match {
    case "index" => "#AutobiographyPagesList ^^" #> "*"
    case _       => (if (S.param("edit") isDefined)
                      "#AutobiographyEditForm ^^" #> "*"
                      else "#AutobiographyPage ^^" #> "*"
                    )
  }
}

class AutobiographyPageSnippet(ap:AutobiographyPage) {
  def render = {
    // THE FOLLOWING LINE IS NOT SECURE IS SHOULD BE CHANGED!!!
    val current_page = AutobiographyHelper.current_page(ap.pageName)
    "h2 *" #> current_page.title &
    "p *"  #> current_page.content
  }
}
