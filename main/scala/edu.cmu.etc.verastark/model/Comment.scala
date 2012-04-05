package edu.cmu.etc.verastark.model

import net.liftweb.mapper._
import net.liftweb.common.Full

class Comment extends LongKeyedMapper[Comment] with IdPK {
  def getSingleton = Comment

  object content   extends MappedTextarea      (this, 60000)
  object owner     extends MappedLongForeignKey(this, User)
  object date      extends MappedDateTime      (this)
  // object item      extends MappedLongForeignKey(this, Item)
  object art_id    extends MappedLongForeignKey(this, Artifact)
  // object com_type  extends MappedString        (this, 10)
  object pubLished extends MappedBoolean       (this)
}

object Comment extends Comment with LongKeyedMetaMapper[Comment]
