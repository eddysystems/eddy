package tarski

import ambiguity.Products._
import ambiguity.Utility.impossible
import scala.language.implicitConversions
import scala.math._

object Scores {
  /* For now, we choose among options using frequentist statistics.  That is, we score A based on the probability
   *
   *   p = Pr(user chooses B | user thinks of A)
   *
   * where B is what we see as input.
   */

  // Wrapper around probabilities
  type Prob = Double
  def Prob(p: Prob): Prob = p
  case class Alt[+A](p: Prob, x: A) // Explicit class to avoid boxing the probability

  sealed abstract class Scored[+A] extends HasProduct[A,Scored] {
    // Invariant: p >= probability of any option in s
    private[Scores] val p: Prob
    private[Scores] def s: Actual[A]

    def best: Either[Error,A] = s.best
    def all: Either[Error,Stream[Alt[A]]] = s.all
    def stream: Stream[Alt[A]] = s.stream
    def bias(q: Prob): Scored[A] = delay(p*q,s bias q)
    def map[B](f: A => B): Scored[B] = delay(p,s map f)

    // f is assumed to generate conditional probabilities
    def flatMap[B](f: A => Scored[B]): Scored[B] = delay(p,s flatMap f)

    def ++[B >: A](t: Scored[B]): Scored[B] =
      if (p >= t.p) delay(p,s ++ t)
      else          delay(t.p,t.s ++ this)

    // We are assumed independent of t
    def productWith[B,C](t: Scored[B])(f: (A,B) => C): Scored[C] = delay(p*t.p,s.productWith(t)(f))

    // Queries
    def isEmpty: Boolean = s.isEmpty
    def isSingle: Boolean = s.isSingle
  }

  // Scored without laziness
  private case class Strict[+A](p: Prob, s: Actual[A]) extends Scored[A]

  // Scored with laziness
  private final class Lazy[+A](val p: Prob, _s: => Actual[A]) extends Scored[A] {
    lazy val s = _s
  }
  private def delay[A](p: Prob, s: => Actual[A]) = new Lazy(p,s)

  // When forced, Scored computes an Actual
  private sealed abstract class Actual[+A] {
    def best: Either[Error,A]
    def all: Either[Error,Stream[Alt[A]]]
    def stream: Stream[Alt[A]]
    def ++[B >: A](s: Scored[B]): Actual[B]
    def bias(p: Prob): Actual[A]
    def map[B](f: A => B): Actual[B]
    def flatMap[B](f: A => Scored[B]): Actual[B]
    def productWith[B,C](s: Scored[B])(f: (A,B) => C): Actual[C]
    def isEmpty: Boolean
    def isSingle: Boolean
  }

  // Failure
  private final class Bad(_e: => Error) extends Actual[Nothing] {
    lazy val e = _e
    def best = Left(e)
    def all = Left(e)
    def stream = Stream.Empty
    def ++[B](s: Scored[B]) = s.s match {
      case s:Bad => new Bad(NestError("++ failed",List(e,s.e)))
      case Empty => this
      case s:Best[_] => s
    }
    def map[B](f: Nothing => B) = this
    def flatMap[B](f: Nothing => Scored[B]) = this
    def bias(p: Prob) = this
    def productWith[B,C](s: Scored[B])(f: (Nothing,B) => C) = this
    def isEmpty = true
    def isSingle = false
  }

  // No options, but not Bad
  private object Empty extends Actual[Nothing] {
    def best = Left(OneError("no options"))
    def all = Right(stream)
    def stream = Stream.Empty
    def map[B](f: Nothing => B) = this
    def flatMap[B](f: Nothing => Scored[B]) = this
    def ++[B](s: Scored[B]) = s.s
    def bias(p: Prob) = this
    def productWith[B,C](s: Scored[B])(f: (Nothing,B) => C) = this
    def isEmpty = true
    def isSingle = false
  }

  // One best possibility, then lazily more
  private case class Best[+A](p: Prob, x: A, r: Scored[A]) extends Actual[A] {
    def best = Right(x)
    def all = Right(stream)
    def stream = Alt(p,x) #:: r.s.stream

    def map[B](f: A => B) = Best(p,f(x),r map f)

    def bias(q: Prob) = Best(p*q,x,r bias q)

    def ++[B >: A](s: Scored[B]): Actual[B] =
      if (p >= s.p) Best(p,x,r ++ s)
      else s.s match {
        case Empty|_:Bad => this
        case s@Best(q,y,t) =>
          if (p >= q) Best(p,x,delay(max(q,r.p),s ++ r))
          else        Best(q,y,delay(max(p,t.p),this ++ t))
      }

    def flatMap[B](f: A => Scored[B]) =
      f(x).s.bias(p) ++ r.flatMap(f)

    def productWith[B,C](s: Scored[B])(f: (A,B) => C) = s.s match {
      case Empty => Empty
      case s:Bad => s
      case Best(q,y,s) => Best(p*q,f(x,y),r.map(f(_,y)).bias(p) ++ s.map(f(x,_)).bias(q) ++ r.productWith(s)(f))
    }

    def isEmpty = false
    def isSingle = r.isEmpty
  }

  // Product support
  implicit object ScoredHasOne extends HasOne[Scored] {
    def one[A](x: A) = known(x)
  }

  // Score constructors
  val empty: Scored[Nothing] = Strict(0,Empty)
  def fail[A](error: => String): Scored[A] = delay(0,new Bad(OneError(error)))
  def known[A](x: A): Scored[A] = Strict(1,Best(1,x,empty))
  def single[A](x: A, p: Prob): Scored[A] = Strict(p,Best(p,x,empty))
  def multiple[A](xs: List[Alt[A]], error: => String): Scored[A] = delay(1,{
    def good(p: Prob, x: A, low: List[Alt[A]], xs: List[Alt[A]]): Actual[A] = xs match {
      case Nil => Best(p,x,delay(p,empty(low)))
      case (y@Alt(q,_))::xs if p >= q => good(p,x,y::low,xs)
      case Alt(q,y)::xs => good(q,y,Alt(p,x)::low,xs)
    }
    def empty(xs: List[Alt[A]]): Actual[A] = xs match {
      case Nil => Empty
      case Alt(0,_)::xs => bad(xs)
      case Alt(p,x)::xs if p > 0 => good(p,x,Nil,xs)
    }
    def bad(xs: List[Alt[A]]): Actual[A] = xs match {
      case Nil => new Bad(OneError(error))
      case Alt(0,_)::xs => bad(xs)
      case Alt(p,x)::xs if p > 0 => good(p,x,Nil,xs)
    }
    bad(xs)
  })

  // Structured errors
  sealed abstract class Error {
    def prefixed(p: String): String
    def short: String
  }
  case class OneError(e: String) extends Error {
    def prefixed(p: String) = p+e
    def short = e
  }
  case class NestError(e: String, es: List[Error]) extends Error {
    def prefixed(p: String) = (p+e :: es.map(_.prefixed(p+"  "))).mkString("\n")
    def short = e
  }
}