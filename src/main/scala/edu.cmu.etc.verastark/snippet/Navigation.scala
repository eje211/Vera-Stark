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

class Navigation {
  def render = "*" #> ClearClearable
}
