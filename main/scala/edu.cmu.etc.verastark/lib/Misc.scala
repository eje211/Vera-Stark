package edu.cmu.etc.verastark.lib

import net.liftweb.common.{Box, Full, Empty}
import net.liftweb.mapper.DB
import scala.xml.{NodeSeq, Text}
import java.security.MessageDigest
import net.liftweb.util.Helpers.{year, month, day, TimeSpan, now}

import java.util.Date

import edu.cmu.etc.verastark.model.User

object Gravatar {
  private def getMD5(message: String): String = {  
    val md: MessageDigest = MessageDigest.getInstance("MD5")  
    val bytes = message.getBytes("CP1252")  
    BigInt(1,md.digest(bytes)).toString(16)  
  } 
  private def html(in: String): NodeSeq = {  
    <div id="gravatar_wrapper"><div id="gravatar_image"><img src={in} alt="Gravater" /></div></div>  
  }
  def gravatar(u: Box[User], size: Int): NodeSeq = {  
    val email: String = u.map(_.email.is) openOr "etc@andrew.cmu.edu"
    html("http://www.gravatar.com/avatar/" + getMD5(email) + "?s=" + size + "&r=G&d=identicon")  
  }

}

object RenderUser {
  def apply(u: Box[User]) = "/user/" + (u.map(_.id.is.toString) openOr "1")
}

object FlagLinks {
  def nextbio(id: Int)  = askdb("autobiography", true,  id)
  def prevbio(id: Int)  = askdb("autobiography", false, id)
  def firstbio   = askfirstlast("autobiography", true)
  def lastbio    = askfirstlast("autobiography", false)
  
  def nextnote(id: Int)  = askdb("notebook", true,  id)
  def prevnote(id: Int)  = askdb("notebook", false, id)
  def firstnote   = askfirstlast("notebook", true)
  def lastnote    = askfirstlast("notebook", false)
  
  def nextart(id: Int)  = askdb("artifact", true,  id)
  def prevart(id: Int)  = askdb("artifact", false, id)
  def firstart   = askfirstlast("artifact", true)
  def lastart    = askfirstlast("artifact", false)

  private def askdb(table: String, next: Boolean, current_id: Int): Box[List[String]] = {
    val op = (if (next) ">" else "<")
    def contributions = DB.runQuery(
      "SELECT next.id, next.title, t.id AS next FROM " + table + " AS t " +
      "JOIN " + table + " AS next ON next.id " + op + " t.id " +
      "WHERE t.id = ? " + 
      "ORDER BY next.id" + (if (next) "" else " DESC") + " LIMIT 1",
      List(current_id))
    if (contributions._2.length == 0)
    Empty
    else Full(contributions._2(0))
  }

  private def askfirstlast(table: String, first: Boolean): Box[List[String]] = {
    def contributions = DB.runQuery (
      "SELECT  t.id,  t.title " +
      "FROM " + table + " AS t ORDER BY t.id " + (if (first) "" else "DESC ") +
      "LIMIT 1")
    if (contributions._2.length == 0)
    Empty
    Full(contributions._2(0))
  }
}

object ModerateEnum extends Enumeration {
  type ModerateEnum = Value
  val Pending, Published, Rejected = Value
}
import ModerateEnum._

object TimeAgoInWords {
  def apply(d: Date) = 
    (now.getTime - d.getTime) / 1000 match {
      case d if (d / (86400 * 365) >= 2) => "%s years ago".format(d / (86400 * 365))
      case d if (d / (86400 * 365) == 1) => "one year ago"
      case d if (d / (86400 * 30) >= 2) => "%s months ago".format(d / (86400 * 30))
      case d if (d / (86400 * 30) == 1) => "one month ago"
      case d if (d / 86400 >= 21) => "three weeks ago"
      case d if (d / 86400 >= 14) => "two weeks ago"
      case d if (d / 86400 >= 7) => "one week ago"
      case d if (d / 86400 >= 2) => "%s days ago".format(d / 86400)
      case d if (d / 86400 == 1) => "one day ago"
      case d if ((d % 86400) >= 7200) => "%s hours ago".format((d % 86400) / 24)
      case d if ((d % 86400) >= 3600) => "about an hour ago"
      case d if ((d % 86400) >= 120) => "%s minutes ago".format((d % 86400) / 60)
      case d if ((d % 86400) >= 30) => "about a nimute ago"
      case _ => "seconds ago"
    }
}
