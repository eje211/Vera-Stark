package edu.cmu.etc.verastark.model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http.S

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
  object owner     extends MappedLongForeignKey(this, User)
  object item      extends MappedLongForeignKey(this, Item)
  object pubLished extends MappedBoolean       (this)
  object deleted   extends MappedBoolean       (this)
  object comments  extends MappedOneToMany     (Comment, Comment.art_id, OrderBy(Comment.date, Ascending))
}

object Artifact extends Artifact with LongKeyedMetaMapper[Artifact] 

object ArtifactTools extends Artifact with LongKeyedMetaMapper[Artifact] {
  def getArtifactById(id: Int): Box[Artifact] =
    Artifact.findAll(By(Artifact.id, id)) match {
      case head :: tail => Full(head)
      case _            => Empty
    }
  def art_owner(id: Int): Box[User] = User.findAll(By(User.id, id)) match {case head :: tail => Full(head) case _ => Empty }
}
