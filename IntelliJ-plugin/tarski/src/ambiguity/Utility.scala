package ambiguity
import scala.collection.mutable

object Utility {
  def notImplemented[A]: A = throw new NotImplementedError("not implemented.")

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

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  // Iterate a function until referential equality fixpoint is reached
  def fixRef[A <: AnyRef](x: A)(f: A => A): A = {
    val fx = f(x)
    if (x eq fx) x else fixRef(fx)(f)
  }

  def allSome[A](xs: List[Option[A]]): Option[List[A]] = xs match {
    case Nil => Some(Nil)
    case None::_ => None
    case Some(x)::xs => allSome(xs) match {
      case None => None
      case Some(xs) => Some(x::xs)
    }
  }

  // Memoize the fixpoint of a recursive function
  def fixpoint[A,B](base: B, f: (A => B, A) => B): A => B = {
    val done = mutable.Map[A,B]()
    val next = mutable.Map[A,B]()
    val active = mutable.Set[A]()
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