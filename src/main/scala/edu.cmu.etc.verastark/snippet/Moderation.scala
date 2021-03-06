package edu.cmu.etc.verastark.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb._
import net.liftweb.http._
import net.liftweb.http.js.JsCmds._
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import java.text.SimpleDateFormat
import sitemap._
import Helpers._
import mapper.By

import java.text.SimpleDateFormat

import edu.cmu.etc.verastark.model._
import edu.cmu.etc.verastark.lib.ModerateEnum._

class ModerationSnippet {
  def render =
    "#artifact-moderation *" #> Artifact.findAll(By(Artifact.published, Pending)).map(a =>
      ".art-title *"       #> a.title.is                                 &
      ".art-title [href]"  #> "/artifact/%s".format(a.id.is)             &
      ".art-author *"      #> (a.owner.map(_.niceName) openOr "unknown") &
      ".art-author [href]" #> (a.owner.map("/user/" + _.id) openOr "/")  &
      ".art-created *"     #> (new SimpleDateFormat("y-MM-d") format a.created.is) &
      ".art-status *"      #> a.published                                &
      ".art-status [id]"   #> "artifact-%s-status".format(a.id.is)       &
      ".art-app [onclick]" #> SHtml.ajaxInvoke(() => {
        a.published(Published).save
        SetHtml("artifact-%s-status".format(a.id.is), Text("Published"))     
      })                                                                 &
      ".art-den [onclick]" #> SHtml.ajaxInvoke(() => {
        a.published(Rejected).save
        SetHtml("artifact-%s-status".format(a.id.is), Text("Rejected"))
      })                                                                 &
      ClearClearable
    ) &
    "#autobiography-moderation *" #> Autobiography.findAll(By(Autobiography.published, Pending)).map(a =>
      ".bio-title *"       #> a.title.is                                 &
      ".bio-title [href]"  #> "/autobiography/%s".format(a.id.is)        &
      ".bio-author *"      #> (a.owner.map(_.niceName) openOr "unknown") &
      ".bio-author [href]" #> (a.owner.map("/user/" + _.id) openOr "/")  &
      ".bio-created *"     #> (new SimpleDateFormat("y-MM-d") format a.created.is) &
      ".bio-status *"      #> a.published                                &
      ".bio-status [id]"   #> "autobio-%s-status".format(a.id.is)        &
      ".bio-app [onclick]" #> SHtml.ajaxInvoke(() => {
        a.published(Published).save
        SetHtml("autobio-%s-status".format(a.id.is), Text("Published"))     
      })                                                                 &
      ".bio-den [onclick]" #> SHtml.ajaxInvoke(() => {
        a.published(Rejected).save
        SetHtml("autobio-%s-status".format(a.id.is), Text("Rejected"))
      })                                                                 &
      ClearClearable
    ) &
    "#notebook-moderation *" #> Notebook.findAll(By(Notebook.published, Pending)).map(a =>
      ".note-title *"       #> a.title.is                                 &
      ".note-title [href]"  #> "/notebook/%s".format(a.id.is)        &
      ".note-author *"      #> (a.owner.map(_.niceName) openOr "unknown") &
      ".note-author [href]" #> (a.owner.map("/user/" + _.id) openOr "/")  &
      ".note-created *"     #> (new SimpleDateFormat("y-MM-d") format a.created.is) &
      ".note-status *"      #> a.published                                &
      ".note-status [id]"   #> "note-%s-status".format(a.id.is)        &
      ".note-app [onclick]" #> SHtml.ajaxInvoke(() => {
        a.published(Published).save
        SetHtml("autonote-%s-status".format(a.id.is), Text("Published"))     
      })                                                                 &
      ".note-den [onclick]" #> SHtml.ajaxInvoke(() => {
        a.published(Rejected).save
        SetHtml("note-%s-status".format(a.id.is), Text("Rejected"))
      })                                                                 &
      ClearClearable
    )
}

class UserModerationSnippet {
  def render =
    "#user-moderation *" #> User.findAll().map(u =>
      ".user-name *"          #> u.niceName              &
      ".user-name [href]"     #> "/user/%s".format(u.id) &
      ".user-contributions *" #>
        (Artifact.count(By(Artifact.ownerid, u.id.is)) +
        Autobiography.count(By(Autobiography.ownerid, u.id.is)) +
        Notebook.count(By(Notebook.ownerid, u.id.is)))   &
      ".user-reported *"      #> u.report.is             &
      ".user-whistled *"      #> u.whistle.is            &
      ".user-edit *"          #> ((if (u.editor.is) "Revoke" else "Make") + " editor") &
      ".user-edit [id]"       #> "user-%s-edit".format(u.id.is) &
      ".user-edit [onclick]"  #> SHtml.ajaxInvoke(() => {
        u.editor(!u.editor.is).save
        SetHtml("user-%s-edit".format(u.id.is), Text((if (u.editor.is) "Revoke" else "Make") + " editor"))
      }) &
      ".user-super *"         #> ((if (u.superUser.is) "Revoke" else "Make") + " superuser") &
      ".user-super [id]"      #> "user-%s-super".format(u.id.is) &
      ".user-super *"         #> ((if (u.superUser.is) "Revoke" else "Make") + " superuser") &
      ".user-super [onclick]" #> SHtml.ajaxInvoke(() => {
        u.superUser(!u.superUser.is).save
        SetHtml("user-%s-super".format(u.id.is), Text((if (u.editor.is) "Revoke" else "Make") + " superuser"))
      })
    )
}
