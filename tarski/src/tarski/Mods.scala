package tarski

import AST.Name
import tarski.Scores._
import utility.Utility._

object Mods {
  sealed abstract class Mod
  case class Annotation(name: Name) extends Mod
  case object Abstract extends Mod
  case object Public extends Mod
  case object Protected extends Mod
  case object Private extends Mod
  case object Static extends Mod
  case object Final extends Mod
  case object Strictfp extends Mod
  case object Transient extends Mod
  case object Volatile extends Mod
  case object Synchronized extends Mod

  // Check for a list of modifiers, and bail if we see any unwanted ones
  def modifiers(mods: List[Mod], want: List[Mod]): Scored[List[Boolean]] = {
    val modSet = mods.toSet
    val bad = modSet -- want
    if (bad.nonEmpty) fail("Unexpected modifiers "+bad.mkString(", "))
    else known(want map modSet.contains)
  }
  def modifiers(mods: List[Mod], a: Mod): Scored[Boolean] = modifiers(mods,List(a)) map (_.head)
  def modifiers(mods: List[Mod], a: Mod, b: Mod): Scored[(Boolean,Boolean)] = modifiers(mods,List(a,b)) map {
    case List(a,b) => (a,b)
    case _ => impossible
  }
  def modifiers(mods: List[Mod], a: Mod, b: Mod, c: Mod): Scored[(Boolean,Boolean,Boolean)] = modifiers(mods,List(a,b,c)) map {
    case List(a,b,c) => (a,b,c)
    case _ => impossible
  }
}
