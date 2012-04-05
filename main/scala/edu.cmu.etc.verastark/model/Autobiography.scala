package edu.cmu.etc.verastark.model

import net.liftweb.mapper._
import net.liftweb.common.Full

class Autobiography extends LongKeyedMapper[Autobiography] with IdPK {
  def getSingleton = Autobiography

  object title extends MappedString(this, 140)
  object content extends MappedTextarea(this, 10000)
  object published extends MappedBoolean(this)
}

object Autobiography extends Autobiography with LongKeyedMetaMapper[Autobiography]

object AutobiographyTools extends Autobiography with LongKeyedMetaMapper[Autobiography] {
  val allPages:List[Autobiography] = Autobiography.findAll
  def getPageTitles:List[String] = Autobiography.findAllFields(Seq[SelectableField](Autobiography.title)
    ).map(_.title.is)
  def getPageTitle(title: String): List[String] = Autobiography.findAllFields(Seq[SelectableField](Autobiography.title),
    By(Autobiography.title, title)).map(_.title.is)
  def getPageByTitle(title: String): List[Autobiography] = Autobiography.findAll(
    By(Autobiography.title, title))
}
