package tarski

import scala.math._
import ambiguity.JavaUtils._

import scala.reflect.ClassTag

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
  abstract class AltBase extends Comparable[AltBase] {
    def p: Prob
    def compareTo(o: AltBase): Int = {
      val p0: Double = p
      val p1: Double = o.p
      if (p0 > p1) -1 else if (p0 < p1) 1 else 0
    }
  }
  case class Alt[+A](p: Prob, x: A) extends AltBase // Explicit class to avoid boxing the probability

  sealed abstract class Scored[+A] {
    def p: Prob
    def best: Either[Error,A]
    def all: Either[Error,Stream[Alt[A]]]
    def stream: Stream[Alt[A]]
    def bias(p: Prob): Scored[A]
    def map[B](f: A => B): Scored[B]

    // Either this or s
    def ++[B >: A](s: LazyScored[B]): Scored[B]
    def ++[B >: A](s: Scored[B]): Scored[B]

    // f is assumed to generate conditional probabilities
    def flatMap[B](f: A => Scored[B]): Scored[B]

    // We are assumed independent of t
    def productWith[B,C](s: => Scored[B])(f: (A,B) => C): Scored[C]

    // Filter, turning Empty into given error
    def filter(f: A => Boolean, error: => String): Scored[A]
    def filter(f: A => Boolean): Scored[A] // Filter with no error

    // Queries
    def isEmpty: Boolean
    def isSingle: Boolean
  }

  // A lazy version of Scored
  sealed abstract class LazyScored[+A] {
    // Invariant: p >= probability of any option in s
    private[Scores] val p: Prob
    def s: Scored[A]
    def best: Either[Error,A] = s.best
    def all: Either[Error,Stream[Alt[A]]] = s.all
    def stream: Stream[Alt[A]] = s.stream
    def bias(q: Prob): LazyScored[A] = new LazyBias(this,q)
    def map[B](f: A => B): LazyScored[B] = new LazyMap(this,f)
    def flatMap[B](f: A => Scored[B]): LazyScored[B] = new LazyFlatMap(this,f)
    def isEmpty: Boolean = s.isEmpty
    def isSingle: Boolean = s.isSingle
    def ++[B >: A](t: LazyScored[B]): LazyScored[B] =
      if (p >= t.p) new LazyPlus(this,t)
      else          new LazyPlus(t,this)
    def productWith[B,C](t: LazyScored[B])(f: (A,B) => C): LazyScored[C] = delay(p*t.p,s.productWith(t.s)(f))
    def filter(f: A => Boolean, error: => String): LazyScored[A] = delay(p,s.filter(f,error))
  }

  // Scored without laziness
  private case class Strict[+A](p: Prob, s: Scored[A]) extends LazyScored[A]

  // Scored with laziness
  private final class Lazy[+A](val p: Prob, _s: => Scored[A]) extends LazyScored[A] {
    lazy val s = _s
  }
  @inline def delay[A](p: Prob, s: => Scored[A]): LazyScored[A] = new Lazy(p,s)

  // Lazy version of x bias q
  private class LazyBias[A](private var x: LazyScored[A], private val q: Prob) extends LazyScored[A] {
    val p = x.p*q
    var _s: Scored[A] = null
    def s = {
      if (_s eq null) {
        val _x = x; x = null
        _s = _x.s bias q
      }
      _s
    }
  }

  // Lazy version of x map f
  private class LazyMap[A,B](private var x: LazyScored[A], private var f: A => B) extends LazyScored[B] {
    val p = x.p
    var _s: Scored[B] = null
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
  private class LazyPlus[A](private var x: LazyScored[A], private var y: LazyScored[A]) extends LazyScored[A] {
    val p = x.p
    var _s: Scored[A] = null
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
  private class LazyFlatMap[A,B](private var x: LazyScored[A], private var f: A => Scored[B]) extends LazyScored[B] {
    val p = x.p
    var _s: Scored[B] = null
    def s = {
      if (_s eq null) {
        val _x = x; x = null
        val _f = f; f = null
        _s = _x.s flatMap _f
      }
      _s
    }
  }

  // If true, failure causes are tracked via Bad.  If false, only Empty and Best are used.
  private val trackErrors = false
  if (trackErrors)
    println("PERFORMANCE WARNING: Error tracking is on, Scored will be slower than otherwise")

  // Failure
  sealed abstract class EmptyOrBad extends Scored[Nothing]
  private final class Bad(_e: => Error) extends EmptyOrBad {
    lazy val e = _e
    def p = 0
    def best = Left(e)
    def all = Left(e)
    def stream = Stream.Empty
    def ++[B](s: LazyScored[B]) = s.s match {
      case s:Bad => new Bad(NestError("++ failed",List(e,s.e)))
      case Empty => this
      case s:Best[_] => s
    }
    def ++[B](s: Scored[B]) = s match {
      case s:Bad => new Bad(NestError("++ failed",List(e,s.e)))
      case Empty => this
      case s:Best[_] => s
    }
    def map[B](f: Nothing => B) = this
    def flatMap[B](f: Nothing => Scored[B]) = this
    def bias(p: Prob) = this
    def productWith[B,C](s: => Scored[B])(f: (Nothing,B) => C) = this
    def filter(f: Nothing => Boolean, error: => String) = this
    def filter(f: Nothing => Boolean) = Empty
    def isEmpty = true
    def isSingle = false
  }

  // No options, but not Bad
  object Empty extends EmptyOrBad {
    def p = 0
    def best = Left(OneError("unknown failure"))
    def all = best
    def stream = Stream.Empty
    def map[B](f: Nothing => B) = this
    def flatMap[B](f: Nothing => Scored[B]) = this
    def ++[B](s: LazyScored[B]) = s.s
    def ++[B](s: Scored[B]) = s
    def bias(p: Prob) = this
    def productWith[B,C](s: => Scored[B])(f: (Nothing,B) => C) = this
    def filter(f: Nothing => Boolean, error: => String) =
      if (trackErrors) new Bad(OneError(error))
      else this
    def filter(f: Nothing => Boolean) = this
    def isEmpty = true
    def isSingle = false
  }

  // One best possibility, then lazily more
  case class Best[+A](p: Prob, x: A, r: LazyScored[A]) extends Scored[A] {
    def best = Right(x)
    def all = Right(stream)
    def stream = Alt(p,x) #:: r.s.stream

    def map[B](f: A => B) = Best(p,f(x),r map f)

    def bias(q: Prob) = Best(p*q,x,r bias q)

    def ++[B >: A](s: LazyScored[B]): Scored[B] =
      if (p >= s.p) Best(p,x,r ++ s)
      else s.s match {
        case s@Best(q,y,t) =>
          if (p >= q) Best(p,x,delay(max(q,r.p),s ++ r))
          else        Best(q,y,delay(max(p,t.p),this ++ t))
        case _:EmptyOrBad => this
      }

    def ++[B >: A](s: Scored[B]): Scored[B] =
      if (p >= s.p) Best(p,x,r ++ delay(s.p,s))
      else s match {
        case s@Best(q,y,t) =>
          if (p >= q) Best(p,x,delay(max(q,r.p),s ++ r))
          else        Best(q,y,delay(max(p,t.p),this ++ t))
        case _:EmptyOrBad => this
      }

    def flatMap[B](f: A => Scored[B]) = new FlatMapState(this,f).extract()

    def productWith[B,C](s: => Scored[B])(f: (A,B) => C) = s match {
      case Best(q,y,s) => Best(p*q,f(x,y),r.map(f(_,y)).bias(p) ++ s.map(f(x,_)).bias(q) ++ r.productWith(s)(f))
      case s:EmptyOrBad => s
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
  val empty: Scored[Nothing] = Empty
  val strictEmpty: LazyScored[Nothing] = Strict(0,Empty)
  @inline def fail[A](error: => String): Scored[A] =
    if (trackErrors) new Bad(OneError(error))
    else Empty
  def known[A](x: A): Scored[A] = Best(1,x,strictEmpty)
  def single[A](x: A, p: Prob): Scored[A] = Best(p,x,strictEmpty)
  def orError[A](x: Scored[A], error: => String): Scored[A] = if (x.isEmpty) fail(error) else x

  // Bias and delay an actual
  private class LazyBiased[A](val p: Prob, _s: => Scored[A]) extends LazyScored[A] {
    lazy val s = _s bias p
  }
  def biased[A](p: Prob, s: => Scored[A]): LazyScored[A] = new LazyBiased(p,s)

  @inline def uniform[A <: AnyRef](p: Prob, xs: Array[A], error: => String): Scored[A] =
    new UniformState[A](p,xs).extract()

  @inline def uniform[A <: AnyRef](p: Prob, xs: Seq[A], error: => String)(implicit t: ClassTag[A]): Scored[A] =
    new UniformState[A](p,xs.toArray).extract()

  @inline def listScored[A](xs: List[Alt[A]], error: => String): Scored[A] =
    new OrderedAlternativeState[A](xs,null).extract()

  // Assume no error (use Empty instead of Bad)
  @inline def multipleGood[A](xs: List[Alt[A]]): Scored[A] =
    new OrderedAlternativeState[A](xs,null).extract()

  // Requires: prob first >= prob andThen
  @inline def orderedAlternative[A](first: List[Alt[A]], andthen: () => List[Alt[A]], error: => String): Scored[A] =
    new OrderedAlternativeState[A](first,andthen).extract()

  // several options, each with a maximum probability
  // in general, this is preferable to using uniform, listScored, multipleGood, or orderedAlternative
  @inline def multiple[A](ls: List[Alt[ () => Scored[A] ]]): Scored[A] =
    new MultipleState[A](ls).extract()

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
  def product[A,B](a: Scored[A], b: => Scored[B]): Scored[(A,B)] =
    a.productWith(b)((_,_))

  def productWith[A,B,T](a: Scored[A], b: => Scored[B])(f: (A,B) => T): Scored[T] =
    a.productWith(b)(f)

  def product[A,B,C](a: Scored[A], b: => Scored[B], c: => Scored[C]): Scored[(A,B,C)] =
    product(a,b).productWith(c)((ab,c) => (ab._1,ab._2,c))

  def productWith[A,B,C,T](a: Scored[A], b: => Scored[B], c: => Scored[C])(f: (A,B,C) => T): Scored[T] =
    product(a,b).productWith(c)((ab,c) => f(ab._1,ab._2,c))

  def product[A,B,C,D](a: Scored[A], b: => Scored[B], c: => Scored[C], d: => Scored[D]): Scored[(A,B,C,D)] =
    product(a,b).productWith(product(c,d))((ab,cd) => (ab._1,ab._2,cd._1,cd._2))

  def productWith[A,B,C,D,T](a: Scored[A], b: => Scored[B], c: => Scored[C], d: => Scored[D])(f: (A,B,C,D) => T): Scored[T] =
    product(a,b).productWith(product(c,d))((ab,cd) => f(ab._1,ab._2,cd._1,cd._2))

  // Sequence products
  def product[A](xs: Option[Scored[A]]): Scored[Option[A]] = xs match {
    case None => known(None)
    case Some(x) => x map (Some(_))
  }
  def product[A](xs: List[Scored[A]]): Scored[List[A]] = xs match {
    case Nil => known(Nil)
    case sx :: sxs => sx.productWith(product(sxs))(_::_)
  }

  def productFoldLeft[A,E](e: E)(fs: List[E => Scored[(E,A)]]): Scored[(E,List[A])] =
    fs match {
      case Nil => known((e,Nil))
      case f :: fs => f(e) flatMap {case (ex,x) => productFoldLeft(ex)(fs) map {case (exs,xs) => (exs,x::xs)}}
    }

  // thread is map followed by product
  def thread[A,B](xs: Option[A])(f: A => Scored[B]): Scored[Option[B]] = product(xs map f)
  def thread[A,B](xs: List[A])  (f: A => Scored[B]): Scored[List[B]]   = product(xs map f)
}
