/* Mods: Java modifiers for AST and Den usage */

package tarski

import AST.Name
import tarski.Scores._
import utility.Utility._
import utility.Locations._

object Mods {
  sealed abstract class Mod
  sealed abstract class SimpleMod extends Mod
  case class Annotation(atr: SRange, name: Name, nr: SRange) extends Mod
  case object Abstract extends SimpleMod
  case object Public extends SimpleMod
  case object Protected extends SimpleMod
  case object Private extends SimpleMod
  case object Static extends SimpleMod
  case object Final extends SimpleMod
  case object Strictfp extends SimpleMod
  case object Transient extends SimpleMod
  case object Volatile extends SimpleMod
  case object Synchronized extends SimpleMod

  // Mods should usually have locations
  type Mods = List[Loc[Mod]]

  // Check for a list of modifiers, and bail if we see any unwanted ones
  def modifiers(mods: Mods, want: List[Mod]): Scored[List[Boolean]] = {
    val modSet = mods.map(_.x).toSet
    val bad = modSet -- want
    if (bad.nonEmpty) fail("Unexpected modifiers "+bad.mkString(", "))
    else known(want map modSet.contains)
  }
  def modifiers(mods: Mods, a: Mod): Scored[Boolean] = modifiers(mods,List(a)) map (_.head)
  def modifiers(mods: Mods, a: Mod, b: Mod): Scored[(Boolean,Boolean)] = modifiers(mods,List(a,b)) map {
    case List(a,b) => (a,b)
    case _ => impossible
  }
  def modifiers(mods: Mods, a: Mod, b: Mod, c: Mod): Scored[(Boolean,Boolean,Boolean)] = modifiers(mods,List(a,b,c)) map {
    case List(a,b,c) => (a,b,c)
    case _ => impossible
  }
}
