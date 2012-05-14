package edu.cmu.etc.verastark.model

import net.liftweb.mapper._
import net.liftweb.common.Full
import edu.cmu.etc.verastark.lib.ModerateEnum

class Comment extends LongKeyedMapper[Comment] with IdPK {
  def getSingleton = Comment

  object content   extends MappedTextarea      (this, 60000)
  object ownerid   extends MappedLongForeignKey(this, User)
  object date      extends MappedDateTime      (this)
  // object item      extends MappedLongForeignKey(this, Item)
  object art_id    extends MappedLongForeignKey(this, Artifact)
  // object com_type  extends MappedString        (this, 10)
  object published extends MappedEnum          (this, ModerateEnum)
  def owner = User.find(By(User.id, this.ownerid))
}

object Comment extends Comment with LongKeyedMetaMapper[Comment]

class Annotation extends LongKeyedMapper[Annotation] with IdPK {
  def getSingleton = Annotation

  object content   extends MappedTextarea      (this, 60000)
  object ownerid   extends MappedLongForeignKey(this, User)
  object date      extends MappedDateTime      (this)
  // object item      extends MappedLongForeignKey(this, Item)
  object bio_id    extends MappedLongForeignKey(this, Autobiography)
  // object com_type  extends MappedString        (this, 10)
  object published extends MappedEnum          (this, ModerateEnum)
  def owner = User.find(By(User.id, this.ownerid))
}

object Annotation extends Annotation with LongKeyedMetaMapper[Annotation]

class MarginNote extends LongKeyedMapper[MarginNote] with IdPK {
  def getSingleton = MarginNote

  object content   extends MappedTextarea      (this, 60000)
  object ownerid   extends MappedLongForeignKey(this, User)
  object date      extends MappedDateTime      (this)
  // object item      extends MappedLongForeignKey(this, Item)
  object bio_id    extends MappedLongForeignKey(this, Notebook)
  // object com_type  extends MappedString        (this, 10)
  object published extends MappedEnum          (this, ModerateEnum)
  def owner = User.find(By(User.id, this.ownerid))
}

object MarginNote extends MarginNote with LongKeyedMetaMapper[MarginNote]
