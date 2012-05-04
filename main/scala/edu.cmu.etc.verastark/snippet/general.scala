package edu.cmu.etc.verastark.snippet

import net.liftweb._
import common._
import http._
import util._
import Helpers._
import mapper.By

import edu.cmu.etc.verastark.model.{User, Artifact, Autobiography, Notebook}
import edu.cmu.etc.verastark.lib.FlagLinks

class VeraObject

class ProcessFlags(vera: VeraObject) {
  def render = {
    val id = vera match {
      case AutobiographyPage(a: Autobiography) => Full(a.ownerid.is)
      case ArtifactPage(a: Artifact)           => Full(a.ownerid.is)
      case NotebookPage(a: Notebook)           => Full(a.ownerid.is)
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

/*
class FlagDestination {
  def render =
    "#flag_next" #> <a id="flag_next" href={"/artifact/" + FlagLinks.firstart(0)} title={FlagLinks.firstart(1)}>Next</a>
}
*/

class FlagIndexDestination {
  def render =
    "#flag_next" #> <a id="flag_next" href={"/artifact/" + (FlagLinks.firstart.map(_(0)) open_!)} title={(FlagLinks.firstart.map(_(1)) open_!)}>Next</a>
}

class FlagArtDestination(ap: ArtifactPage) {
  def render = {
    var next: Box[List[String]] = Empty
    var prev: Box[List[String]] = Empty
    next = FlagLinks.nextart(ap.a.id.is.toInt)
    if (next isEmpty) next = FlagLinks.firstart
    prev = FlagLinks.prevart(ap.a.id.is.toInt)
    if (prev isEmpty) prev = FlagLinks.lastart
    // "*" #> ClearClearable
    "#flag_next" #> <a id="flag_next" href={"/artifact/" + (next.map(_(0)) open_!)} title={(next.map(_(1)) open_!)}>Next</a> &
    "#flag_prev" #> <a id="flag_prev" href={"/artifact/" + (prev.map(_(0)) open_!)} title={(prev.map(_(1)) open_!)}>Previous</a>
  }
}

class FlagBioDestination(ap: AutobiographyPage) {
  def render = {
    var next: Box[List[String]] = Empty
    var prev: Box[List[String]] = Empty
    next = FlagLinks.nextbio(ap.a.id.is.toInt)
    if (next isEmpty) next = FlagLinks.firstbio
    prev = FlagLinks.prevbio(ap.a.id.is.toInt)
    if (prev isEmpty) prev = FlagLinks.lastbio
    // "*" #> ClearClearable
    "#flag_next" #> <a id="flag_next" href={"/autobiography/" + (next.map(_(0)) open_!)} title={(next.map(_(1)) open_!)}>Next</a> &
    "#flag_prev" #> <a id="flag_prev" href={"/autobiography/" + (prev.map(_(0)) open_!)} title={(prev.map(_(1)) open_!)}>Previous</a>
  }
}

class FlagNoteDestination(ap: NotebookPage) {
  def render = {
    var next: Box[List[String]] = Empty
    var prev: Box[List[String]] = Empty
    next = FlagLinks.nextnote(ap.a.id.is.toInt)
    if (next isEmpty) next = FlagLinks.firstbio
    prev = FlagLinks.prevnote(ap.a.id.is.toInt)
    if (prev isEmpty) prev = FlagLinks.lastbio
    // "*" #> ClearClearable
    "#flag_next" #> <a id="flag_next" href={"/autobiography/" + (next.map(_(0)) open_!)} title={(next.map(_(1)) open_!)}>Next</a> &
    "#flag_prev" #> <a id="flag_prev" href={"/autobiography/" + (prev.map(_(0)) open_!)} title={(prev.map(_(1)) open_!)}>Previous</a>
  }
}
