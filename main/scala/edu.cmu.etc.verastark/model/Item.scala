package edu.cmu.etc.verastark.model

import net.liftweb.mapper._
import net.liftweb.common.Full

/**
 * This is the global item ID for all objects.
 * This ID should be enough to retrieve any
 * user-submitted piece of content on the site.
 */

class Item extends LongKeyedMapper[Item] with IdPK {
  def getSingleton = Item
}

object Item extends Item with LongKeyedMetaMapper[Item]
