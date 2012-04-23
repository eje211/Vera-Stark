package edu.cmu.etc.verastark.model

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.sitemap.{Menu, Loc}
import _root_.net.liftweb.http.S
import net.liftweb.openid.{OpenIDProtoUser, MetaOpenIDProtoUser}
import Helpers._


/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaOpenIDProtoUser[User] with LongKeyedMetaMapper[User] with Logger {
  def openIDVendor = DefaultOpenIDVendor
  override def dbTableName = "users" // define the DB table name
  override def screenWrap = Full(<lift:surround with="login_template" at="content"><lift:bind /></lift:surround>)
  // override def screenWrap = Full(<lift:surround with="default" at="content"><lift:bind /></lift:surround>)

  override def loginXhtml =  <lift:embed what="login" />
  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email,
  locale, timezone, password)

  // comment this line out to require email validations
  override def skipEmailValidation = true
  
  override def loginMenuLoc: Box[Menu] =  
    Full(Menu(Loc("Login", loginPath, "Sign In", loginMenuLocParams)))  

  // Save the time and date the user signed up.
  override def actionsAfterSignup(theUser: TheUserType, func: () => Nothing): Nothing = {
    theUser.memberSince(now).save
    func()
  }

  // override val basePath: List[String] = "user" :: Nil
}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends LongKeyedMapper[User] with OpenIDProtoUser[User] {
  def getSingleton = User // what's the "meta" server

  object editor      extends MappedBoolean (this)
  object memberSince extends MappedDateTime(this)
  object quotation   extends MappedTextarea(this, 2000)

  //Default OpenIDProtoUser implementation uses nickname so swapping this out for standard FirstName Surname (Email) format.
  override def niceName: String = (firstName.is, lastName.is, email.is) match {
    case (f, l, _) if f.length > 1 && l.length > 1 => f+" "+l
    case (f, _, _) if f.length > 1 => f
    case (_, l, _) if l.length > 1 => l
    case (_, _, e) => e.split("@")(0)
  }

}

