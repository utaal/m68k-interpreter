package com.github.utaal.log

import scala.annotation.elidable

abstract class Level(val value: Int) extends Ordered[Level] {
  def compare(b: Level) = this.value compare b.value
  val name = toString.toLowerCase
}

object Level {
  val DEBUG = 100
  case object Debug extends Level(DEBUG)
  case object Finest extends Level(elidable.FINEST)
  case object Finer extends Level(elidable.FINER)
  case object Fine extends Level(elidable.FINE)
  case object Config extends Level(elidable.CONFIG)
  case object Info extends Level(elidable.INFO)
  case object Warning extends Level(elidable.WARNING)
  case object Severe extends Level(elidable.SEVERE)
}

object Logger {
  var level = Level.Debug

  private def curlyFormat(fmt: String, subs: List[Any]) =
    (fmt /: subs.view.zipWithIndex) { (res, sub) => 
      res.replaceAll("\\{%s\\}".format(sub._2), sub._1.toString)
    }

  def for_[Scope <: Singleton](scope: Scope) = {
    val packageName = scope.asInstanceOf[AnyRef].getClass.getPackage match {
        case null => null
        case nonDefaultPackage => nonDefaultPackage.getName
      }
    new Logger(packageName, level)
  }

}

class Logger(name: String, minLevel: Level) {
  private def log(level: Level, fmt: String, subs: List[Any]) = {
    if (level >= minLevel)
      println("[" + level.name + "] " + name + ": " + Logger.curlyFormat(fmt, subs))
  }

  @elidable(Level.DEBUG)
  /*def DEBUG(msg: String) = log(Level.Debug, msg)*/
  def DEBUG(fmt: String, subs: Any*) = log(Level.Debug, fmt, subs.toList)
}
