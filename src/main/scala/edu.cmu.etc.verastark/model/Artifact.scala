package edu.cmu.etc.verastark.model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http.S
import edu.cmu.etc.verastark.lib.ModerateEnum

class Artifact extends LongKeyedMapper[Artifact] with OneToMany[Long, Artifact] with IdPK {
  def getSingleton = Artifact

  object title     extends MappedString        (this, 500) {
    override def toForm = S.fmapFunc({s: String => this.setFromAny(s)}){funcName =>  
    Full(<input name={funcName} id={fieldId} value={is} class="title text" />)}  
  }
  object content   extends MappedTextarea      (this, 60000) {
    override def toForm = S.fmapFunc({s: String => this.setFromAny(s)}){funcName =>  
    Full(<textarea name={funcName} id={fieldId} class="description text">{is}</textarea>)}  
  }
  object url       extends MappedString        (this, 300)
  object date      extends MappedString        (this, 300) {
    override def toForm = S.fmapFunc({s: String => this.setFromAny(s)}){funcName =>  
    Full(<input name={funcName} id={fieldId} value={is} class="date text" />)}  
  }
  object artist    extends MappedString        (this, 300) {
    override def toForm = S.fmapFunc({s: String => this.setFromAny(s)}){funcName =>  
    Full(<input name={funcName} id={fieldId} value={is} class="artist text" />)}  
  }
  object ownerid   extends MappedLongForeignKey(this, User)
  object item      extends MappedLongForeignKey(this, Item)
  object filename  extends MappedString        (this, 200)
  object filetype  extends MappedString        (this, 50)
  object app_date  extends MappedDate          (this) // An approximate exact date for the Timeline.
  object published extends MappedEnum          (this, ModerateEnum) // Visible to the public?
  object deleted   extends MappedBoolean       (this) // Is there a request to delete?
  object moderated extends MappedBoolean       (this) // Has a moderator seen this?
  object genuine   extends MappedBoolean       (this) // Has a moderator seen this?
  object created   extends MappedDateTime      (this)
  object changed   extends MappedDateTime      (this) // Last time this artifact was changed
  object comments  extends MappedOneToMany     (Comment, Comment.art_id, OrderBy(Comment.date, Ascending))
  def    owner = User.find(By(User.id, this.ownerid))
}

object Artifact extends Artifact with LongKeyedMetaMapper[Artifact]
