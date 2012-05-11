package edu.cmu.etc.verastark.snippet

import scala.xml.NodeSeq
import net.liftweb.http._

class LoginForm {
  def render(in: NodeSeq) = 
    {S.set("loginFrom", S.referer openOr "/"); in}
}
