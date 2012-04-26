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
import mapper.{By, DB}
import java.text.SimpleDateFormat
import java.text.SimpleDateFormat
import java.lang.Integer

import edu.cmu.etc.verastark.model._

class UserProfile(u: User) {
  def render = {
    lazy val contributions = DB.runQuery(
      "SELECT id, '/artifact/' AS type, title, changed " +
      "FROM artifact WHERE ownerid = ? UNION " +
      "SELECT id, '/autobiography/' AS type, title, changed " + 
      "FROM autobiography WHERE ownerid = ? " +
      "ORDER BY changed DESC", List(u.id.is, u.id.is))
    lazy val count = (Artifact.count(By(Artifact.ownerid, u.id.is)) +
      Autobiography.count(By(Autobiography.ownerid, u.id.is)))
    ".user_rank *"        #> (if (u.superUser) "Curator" else if (u.editor) "Curiatorial Assistant" else "Member") &
    ".user_name *"        #> u.niceName                                                                            &
    "#joined"             #> (new SimpleDateFormat("EEEE MMMM d, yyyy") format u.memberSince.is)                   &
    "#count"              #> (if (count == 0) "none yet" else if (count == 1) "one" 
                               else if (count == 2) "two" else count.toString)                                     &
    "#contributions *"    #> (if (contributions._2.length == 0) <i>No contributions yet!</i>
                               else contributions._2.map(c => <li><a href={"" + c(1) + c(0)}>{c(2)}</a></li>))     &
    ".user_description *" #> TextileParser.toHtml(u.quotation.is)                                                  &
    ".right [class+]"     #> (if (User.currentUser.map(_.id == u.id) openOr false) "" else "clearable") andThen ClearClearable
  }
}
