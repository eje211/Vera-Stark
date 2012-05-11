package edu.cmu.etc.verastark.model

import net.liftweb.mapper._
import net.liftweb.common.Full
import edu.cmu.etc.verastark.lib.ModerateEnum

class Notebook extends LongKeyedMapper[Notebook] with OneToMany[Long, Notebook] with IdPK {
  def getSingleton = Notebook

  object title       extends MappedString        (this, 140)
  object content     extends MappedTextarea      (this, 10000)
  object description extends MappedTextarea      (this, 10000)
  object date        extends MappedString        (this, 200)
  object app_date    extends MappedDate          (this)
  object published   extends MappedEnum          (this, ModerateEnum)
  object deleted     extends MappedBoolean       (this)
  object genuine     extends MappedBoolean       (this)
  object ownerid     extends MappedLongForeignKey(this, User)
  object changed     extends MappedDateTime      (this)
  object created     extends MappedDateTime      (this)
  object annotations extends MappedOneToMany     (MarginNote, MarginNote.bio_id, OrderBy(MarginNote.date, Ascending))
  def    owner =     User.find(By(User.id, this.ownerid))
}

object Notebook extends Notebook with LongKeyedMetaMapper[Notebook]
