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
import mapper.By
import textile._

import java.lang.Integer
import java.lang.NumberFormatException

import edu.cmu.etc.verastark.model._

class AutobiographyParam
case class  AutobiographyPage(page: Autobiography) extends AutobiographyParam
case object AutobiographyIndex                     extends AutobiographyParam
case object AutobiographyEdit                      extends AutobiographyParam
case object AutobiographyNotFound                  extends AutobiographyParam

object AutobiographyPageMenu {
  def parse(id: String) = id match {
    case "" | "index" => Full(AutobiographyIndex)
    case "new"        => Full(AutobiographyEdit)
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
    case AutobiographyEdit     => "new"
    case AutobiographyNotFound => "index"
    case _                     => "index"
  }

  val menu = Menu.param[AutobiographyParam]("Autobiography", "Autobiography", parse _, encode _) / "autobiography"
  lazy val loc = menu.toLoc

  def render = "*" #> "Autobiography" // loc.currentValue.map(_.pageName)
}

class AutobiographyContainer(ap: AutobiographyParam) {
  def render = ap match {
    case AutobiographyIndex      => "#AutobiographyPagesList ^^" #> "*"
    case AutobiographyEdit       => "#AutobiographyEditForm ^^"  #> "*"
    case AutobiographyPage(a) =>
      if   (S.param("edit") isDefined) "#AutobiographyEditForm ^^"      #> "*"
      else "#AutobiographyPage ^^"  #> "*"
  }
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
