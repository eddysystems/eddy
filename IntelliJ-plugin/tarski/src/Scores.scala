package tarski

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

  sealed abstract class Scored[+A] {
    // Invariant: p >= probability of any option in s
    private[Scores] val p: Prob
    def s: Actual[A]

    def best: Either[Error,A] = s.best
    def all: Either[Error,Stream[Alt[A]]] = s.all
    def stream: Stream[Alt[A]] = s.stream
    def bias(q: Prob): Scored[A] = new LazyBias(this,q)
    def map[B](f: A => B): Scored[B] = new LazyMap(this,f)

    // f is assumed to generate conditional probabilities
    def flatMap[B](f: A => Actual[B]): Scored[B] = new LazyFlatMap(this,f)

    def ++[B >: A](t: Scored[B]): Scored[B] =
      if (p >= t.p) new LazyPlus(this,t)
      else          new LazyPlus(t,this)

    // We are assumed independent of t
    def productWith[B,C](t: Scored[B])(f: (A,B) => C): Scored[C] = delay(p*t.p,s.productWith(t.s)(f))

    // Filter, turning Empty into given error
    def filter(f: A => Boolean, error: => String): Scored[A] = delay(p,s.filter(f,error))

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
  @inline def delay[A](p: Prob, s: => Actual[A]): Scored[A] = new Lazy(p,s)

  // Lazy version of x bias q
  private class LazyBias[A](private var x: Scored[A], private val q: Prob) extends Scored[A] {
    val p = x.p*q
    var _s: Actual[A] = null
    def s = {
      if (_s eq null) {
        val _x = x; x = null
        _s = _x.s bias q
      }
      _s
    }
  }

  // Lazy version of x map f
  private class LazyMap[A,B](private var x: Scored[A], private var f: A => B) extends Scored[B] {
    val p = x.p
    var _s: Actual[B] = null
    def s = {
      if (_s eq null) {
        val _x = x; x = null
        val _f = f; f = null
        _s = _x.s map _f
      }
      _s
    }
  }

  // Lazy version of x ++ y, assuming x.p >= y.p
  private class LazyPlus[A](private var x: Scored[A], private var y: Scored[A]) extends Scored[A] {
    val p = x.p
    var _s: Actual[A] = null
    def s = {
      if (_s eq null) {
        val _x = x; x = null
        val _y = y; y = null
        _s = _x.s ++ _y
      }
      _s
    }
  }

  // Lazy version of x flatMap f
  private class LazyFlatMap[A,B](private var x: Scored[A], private var f: A => Actual[B]) extends Scored[B] {
    val p = x.p
    var _s: Actual[B] = null
    def s = {
      if (_s eq null) {
        val _x = x; x = null
        val _f = f; f = null
        _s = _x.s flatMap _f
      }
      _s
    }
  }

  // When forced, Scored computes an Actual
  sealed abstract class Actual[+A] {
    def best: Either[Error,A]
    def all: Either[Error,Stream[Alt[A]]]
    def stream: Stream[Alt[A]]
    def ++[B >: A](s: Scored[B]): Actual[B]
    def bias(p: Prob): Actual[A]
    def map[B](f: A => B): Actual[B]
    def flatMap[B](f: A => Actual[B]): Actual[B]
    def productWith[B,C](s: => Actual[B])(f: (A,B) => C): Actual[C]
    def filter(f: A => Boolean, error: => String): Actual[A]
    def filter(f: A => Boolean): Actual[A] // Filter with no error
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
    def flatMap[B](f: Nothing => Actual[B]) = this
    def bias(p: Prob) = this
    def productWith[B,C](s: => Actual[B])(f: (Nothing,B) => C) = this
    def filter(f: Nothing => Boolean, error: => String) = this
    def filter(f: Nothing => Boolean) = Empty
    def isEmpty = true
    def isSingle = false
  }

  // No options, but not Bad
  private object Empty extends Actual[Nothing] {
    def best = Left(OneError("no options"))
    def all = Right(stream)
    def stream = Stream.Empty
    def map[B](f: Nothing => B) = this
    def flatMap[B](f: Nothing => Actual[B]) = this
    def ++[B](s: Scored[B]) = s.s
    def bias(p: Prob) = this
    def productWith[B,C](s: => Actual[B])(f: (Nothing,B) => C) = this
    def filter(f: Nothing => Boolean, error: => String) = new Bad(OneError(error))
    def filter(f: Nothing => Boolean) = this
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
        case s@Best(q,y,t) =>
          if (p >= q) Best(p,x,delay(max(q,r.p),s ++ r))
          else        Best(q,y,delay(max(p,t.p),this ++ t))
        case Empty|_:Bad => this
      }

    def flatMap[B](f: A => Actual[B]) = {
      //println(s"Best.flatMap: f ${f.getClass}, x $x, p $p")
      f(x).bias(p) ++ r.flatMap(f)
    }

    def productWith[B,C](s: => Actual[B])(f: (A,B) => C) = s match {
      case Empty => Empty
      case s:Bad => s
      case Best(q,y,s) => Best(p*q,f(x,y),r.map(f(_,y)).bias(p) ++ s.map(f(x,_)).bias(q) ++ r.productWith(s)(f))
    }

    def filter(f: A => Boolean, error: => String) =
      if (f(x)) Best(p,x,delay(r.p,r.s.filter(f)))
      else r.s.filter(f,error)
    def filter(f: A => Boolean) =
      if (f(x)) Best(p,x,delay(r.p,r.s.filter(f)))
      else r.s.filter(f)

    def isEmpty = false
    def isSingle = r.isEmpty
  }

  // Score constructors
  val empty: Actual[Nothing] = Empty
  val strictEmpty: Scored[Nothing] = Strict(0,Empty)
  def fail[A](error: => String): Actual[A] = new Bad(OneError(error))
  def known[A](x: A): Actual[A] = Best(1,x,strictEmpty)
  def single[A](x: A, p: Prob): Actual[A] = Best(p,x,strictEmpty)

  // Bias and delay an actual
  private class LazyBiased[A](val p: Prob, _s: => Actual[A]) extends Scored[A] {
    lazy val s = _s bias p
  }
  def biased[A](p: Prob, s: => Actual[A]): Scored[A] = new LazyBiased(p,s)

  def uniform[A](p: Prob, xs: List[A], error: => String): Actual[A] =
    if (p == 0.0 || xs.isEmpty)
      fail(error)
    else {
      def good(xs: List[A]): Actual[A] = xs match {
        case Nil => Empty
        case x::xs => Best(p,x,delay(p,good(xs)))
      }
      good(xs)
    }

  def uniformArray[A](p: Prob, xs: Array[A], error: => String): Actual[A] =
    if (p == 0.0 || (xs eq null) || (xs.length == 0))
      fail(error)
    else {
      def good(i: Int): Actual[A] =
        if (i == xs.length) Empty
        else Best(p,xs(i),delay(p,good(i+1)))
      good(0)
    }

  // Helpers for multiple and multipleGood
  private def good[A](p: Prob, x: A, low: List[Alt[A]], xs: List[Alt[A]]): Actual[A] = xs match {
    case Nil => Best(p,x,delay(p,empty(low)))
    case (y@Alt(q,_))::xs if p >= q => good(p,x,y::low,xs)
    case Alt(q,y)::xs => good(q,y,Alt(p,x)::low,xs)
  }
  private def empty[A](xs: List[Alt[A]]): Actual[A] = xs match {
    case Nil => Empty
    case Alt(0,_)::xs => empty(xs)
    case Alt(p,x)::xs => good(p,x,Nil,xs)
  }

  def multiple[A](xs: List[Alt[A]], error: => String): Actual[A] = {
    def bad(xs: List[Alt[A]]): Actual[A] = xs match {
      case Nil => new Bad(OneError(error))
      case Alt(0,_)::xs => bad(xs)
      case Alt(p,x)::xs => good(p,x,Nil,xs)
    }
    bad(xs)
  }
  // Assume no error (use Empty instead of Bad)
  def multipleGood[A](xs: List[Alt[A]]): Actual[A] =
    empty(xs)

  def multiples[A](first: List[Alt[A]], andthen: => List[Alt[A]], error: => String): Actual[A] = {
    def good(p: Prob, x: A, low: List[Alt[A]], xs: List[Alt[A]], backup: List[Alt[A]]): Actual[A] = xs match {
      case Nil => Best(p,x,delay(p,possiblyEmpty(low,backup)))
      case (y@Alt(q,_))::xs if p >= q => good(p,x,y::low,xs,backup)
      case Alt(q,y)::xs => good(q,y,Alt(p,x)::low,xs,backup)
    }
    def possiblyEmpty(xs: List[Alt[A]], backup: => List[Alt[A]]): Actual[A] = xs match {
      case Nil => backup match { // if xs is empty, switch to backup
        case Nil => Empty
        case xs => possiblyEmpty(xs, Nil)
      }
      case Alt(0,_)::xs => possiblyEmpty(xs,backup)
      case Alt(p,x)::xs => good(p,x,Nil,xs,backup)
    }
    def possiblyBad(xs: List[Alt[A]], backup: => List[Alt[A]]): Actual[A] = xs match {
      case Nil => backup match { // if xs is empty, switch to backup
        case Nil => new Bad(OneError(error))
        case xs => possiblyBad(xs, Nil)
      }
      case Alt(0,_)::xs => possiblyBad(xs,backup)
      case Alt(p,x)::xs => good(p,x,Nil,xs,backup)
    }
    possiblyBad(first,andthen)
  }

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

  // Product sugar

  // Fixed size products
  def product[A,B](a: Actual[A], b: => Actual[B]): Actual[(A,B)] =
    a.productWith(b)((_,_))

  def productWith[A,B,T](a: Actual[A], b: => Actual[B])(f: (A,B) => T): Actual[T] =
    a.productWith(b)(f)

  def product[A,B,C](a: Actual[A], b: => Actual[B], c: => Actual[C]): Actual[(A,B,C)] =
    product(a,b).productWith(c)((ab,c) => (ab._1,ab._2,c))

  def productWith[A,B,C,T](a: Actual[A], b: => Actual[B], c: => Actual[C])(f: (A,B,C) => T): Actual[T] =
    product(a,b).productWith(c)((ab,c) => f(ab._1,ab._2,c))

  def product[A,B,C,D](a: Actual[A], b: => Actual[B], c: => Actual[C], d: => Actual[D]): Actual[(A,B,C,D)] =
    product(a,b).productWith(product(c,d))((ab,cd) => (ab._1,ab._2,cd._1,cd._2))

  def productWith[A,B,C,D,T](a: Actual[A], b: => Actual[B], c: => Actual[C], d: => Actual[D])(f: (A,B,C,D) => T): Actual[T] =
    product(a,b).productWith(product(c,d))((ab,cd) => f(ab._1,ab._2,cd._1,cd._2))

  // Sequence products
  def product[A](xs: Option[Actual[A]]): Actual[Option[A]] = xs match {
    case None => known(None)
    case Some(x) => x map (Some(_))
  }
  def product[A](xs: List[Actual[A]]): Actual[List[A]] = xs match {
    case Nil => known(Nil)
    case sx :: sxs => sx.productWith(product(sxs))(_::_)
  }

  def productFoldLeft[A,E](e: E)(fs: List[E => Actual[(E,A)]]): Actual[(E,List[A])] =
    fs match {
      case Nil => known((e,Nil))
      case f :: fs => f(e) flatMap {case (ex,x) => productFoldLeft(ex)(fs) map {case (exs,xs) => (exs,x::xs)}}
    }

  // thread is map followed by product
  def thread[A,B](xs: Option[A])(f: A => Actual[B]): Actual[Option[B]] = product(xs map f)
  def thread[A,B](xs: List[A])  (f: A => Actual[B]): Actual[List[B]]   = product(xs map f)
}