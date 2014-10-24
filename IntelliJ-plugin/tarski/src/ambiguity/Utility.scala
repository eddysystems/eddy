package ambiguity
import scala.collection.{mutable => smutable}

object Utility {
  def toMapList[A,B](c: Iterable[(A,B)]): Map[A,List[B]] =
    c.groupBy(_._1).mapValues(_.map(_._2).toList)

  def toMapSet[A,B](c: Iterable[(A,B)]): Map[A,Set[B]] =
    c.groupBy(_._1).mapValues(_.map(_._2).toSet)

  def doWhile(f: => Boolean): Unit =
    if (f) doWhile(f)

  def splitWhitespace(s: String): List[String] =
    s.split("""\s+""").toList match {
      case "" :: x => x
      case x => x
    }

  // Memoize the fixpoint of a recursive function
  def fixpoint[A,B](base: B, f: (A => B, A) => B): A => B = {
    val done = smutable.Map[A,B]()
    val next = smutable.Map[A,B]()
    val active = smutable.Set[A]()
    var changed = false
    def fix(a: A): B = {
      done get a match {
        case Some(b) => b
        case None => next get a match {
          case None =>
            changed = true
            active += a
            next(a) = base
            val b = f(fix,a)
            if (b != base)
              next(a) = b
            b
          case Some(b) =>
            if (active contains a)
              b
            else {
              active += a
              val c = f(fix,a)
              if (b != c) {
                changed = true
                next(a) = c
              }
              c
            }
        }
      }
    }
    def outer(a: A): B = {
      val b = fix(a)
      if (changed) {
        changed = false
        active.clear()
        outer(a)
      } else {
        done ++= next
        next.clear()
        active.clear()
        b
      }
    }
    outer
  }
}