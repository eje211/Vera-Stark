package edu.cmu.etc.verastark.model

import net.liftweb.mapper._
import net.liftweb.common.Full
import edu.cmu.etc.verastark.lib.ModerateEnum

class Autobiography extends LongKeyedMapper[Autobiography] with OneToMany[Long, Autobiography] with IdPK {
  def getSingleton = Autobiography

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
  object annotations extends MappedOneToMany     (Annotation, Annotation.bio_id, OrderBy(Annotation.date, Ascending))
  def    owner =     User.find(By(User.id, this.ownerid))
}

object Autobiography extends Autobiography with LongKeyedMetaMapper[Autobiography]
