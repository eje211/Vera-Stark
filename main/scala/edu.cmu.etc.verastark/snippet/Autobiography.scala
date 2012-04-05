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

class AutobiographyParam
case class  AutobiographyPage(pageName: String) extends AutobiographyParam
case object AutobiographyIndex extends AutobiographyParam
case object AutobiographyEdit extends AutobiographyParam

object AutobiographyPagesList {
  def render(in:NodeSeq):NodeSeq =
    AutobiographyTools.getPageTitles.flatMap { text => bind("page", in, "title" -> <a href={"/journal/" + text}>{text}</a>)}
}

object AutobiographyPageMenu {
  def parse(name: String) = name match {
    case "" | "index" => Full(AutobiographyIndex)
    case "new"        => Full(AutobiographyEdit)
    case _            => Full(AutobiographyPage(name))
  }
  def encode(ap: AutobiographyParam) = ap match {
    case AutobiographyPage(name) => name
    case _                       => "index"
  }

  val menu = Menu.param[AutobiographyParam]("Autobiography", "Autobiography", parse _, encode _) / "journal"
  lazy val loc = menu.toLoc

  def render = "*" #> "Autobiography" // loc.currentValue.map(_.pageName)
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

class AutobiographyContainer(ap: AutobiographyParam) {
  def render = ap match {
    case AutobiographyIndex      => "#AutobiographyPagesList ^^" #> "*"
    case AutobiographyEdit       => "#AutobiographyEditForm ^^"  #> "*"
    case AutobiographyPage(name) =>
      if   (S.param("edit") isDefined) "#AutobiographyEditForm ^^"      #> "*"
      else "#AutobiographyPage ^^"  #> "*"
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

object AutobiographyScreen extends LiftScreen {
  object page extends ScreenVar(Autobiography.create)

  addFields(() => page.is)

  val shouldSave = field("Save", false)

  def finish() {}
}
