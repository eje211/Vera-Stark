package edu.cmu.etc.verastark.snippet

import net.liftweb._
import common._
import http._
import util._
import Helpers._
import mapper.By

import edu.cmu.etc.verastark.model.{User, Artifact, Autobiography}

class VeraObject

class ProcessFlags(vera: VeraObject) {
  def render = {
    val id = vera match {
      case AutobiographyPage(a: Autobiography) => Full(a.ownerid.is)
      case ArtifactPage(a: Artifact)           => Full(a.ownerid.is)
      case ArtifactNew                         => Empty: Box[Int]
    }
    def visible_? =
      User.find(By(User.id, User.currentUserId.map(_.toInt) openOr 0)) match {
        case Empty            => false
        case Full(user: User) => (user.superUser.is || (id.map(user.id.is == _) openOr false))
        case _                => false
    }

    "#flag_edit [class+]" #> (if(visible_?) "" else "clearable") andThen
    ClearClearable
  }
}
