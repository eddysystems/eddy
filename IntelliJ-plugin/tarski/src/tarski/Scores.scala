package tarski

import scala.annotation.tailrec
import scala.math._
import tarski.JavaScores._
import ambiguity.Utility._
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
  abstract class HasProb extends Comparable[HasProb] {
    def p: Prob
    def compareTo(o: HasProb): Int = {
      val p0: Double = p
      val p1: Double = o.p
      if (p0 > p1) -1 else if (p0 < p1) 1 else 0
    }
  }
  case class Alt[+A](p: Prob, x: A) extends HasProb // Explicit class to avoid boxing the probability

  sealed abstract class Scored[+A] extends HasProb {
    // Invariant: p >= probability of any option
    def p: Prob

    // These force some evaluation
    private final def strict: StrictScored[A] = {
      @tailrec def loop(x: Scored[A]): StrictScored[A] = x match {
        case x:StrictScored[A] => x
        case x:LazyScored[A] => loop(x force 0)
      }
      loop(this)
    }
    final def best: Either[Error,A] = strict match {
      case Best(_,x,_) => Right(x)
      case x:EmptyOrBad => Left(x.error)
    }
    final def all: Either[Error,Stream[Alt[A]]] = strict match {
      case x:Best[A] => Right(x.stream)
      case x:EmptyOrBad => Left(x.error)
    }
    final def stream: Stream[Alt[A]] = strict match {
      case Best(p,x,r) => Alt(p,x) #:: r.stream
      case x:EmptyOrBad => Stream.Empty
    }
    final def isEmpty: Boolean = strict match {
      case x:Best[_] => false
      case _:EmptyOrBad => true
    }
    final def isSingle: Boolean = strict match {
      case Best(_,_,r) => r.isEmpty
      case _:EmptyOrBad => false
    }

    // Multiply all probabilities by p
    def bias(p: Prob): Scored[A]

    // Apply f to every alternative
    def map[B](f: A => B): Scored[B] = new LazyMap(this,f)

    // Either this or s
    def ++[B >: A](s: Scored[B]): Scored[B]

    // f is assumed to generate conditional probabilities
    def flatMap[B](f: A => Scored[B]): Scored[B] = new Extractor[B](new FlatMapState(this,f))

    // We are assumed independent of t
    def productWith[B,C](s: Scored[B])(f: (A,B) => C): Scored[C] = new LazyProductWith(this,s,f)

    def productWithFilter[B,C](s: Scored[B], filter: (A,B) => Boolean)(f: (A,B) => C): Scored[C] = new LazyProductWith(this,s,f)

    // Filter, turning Empty into given error
    final def filter(f: A => Boolean, error: => String): Scored[A] = _filter(f,if (trackErrors) () => error else null)
    def _filter(f: A => Boolean, error: () => String): Scored[A] = new LazyFilter(this,f,error)
  }

  // Scored with at least one option evaluated (if any exists)
  sealed abstract class StrictScored[+A] extends Scored[A]

  // A lazy version of Scored
  abstract class LazyScored[+A] extends Scored[A] {
    // May return another LazyScored, usually with lower probability.  Optionally continue until prob <= p.
    def force(hi: Prob): Scored[A]

    def bias(p: Prob): Scored[A] = new LazyBias(this,p)
    def ++[B >: A](s: Scored[B]): Scored[B] =
      if (p >= s.p) new LazyPlus(this,s)
      else s match {
        case s:LazyScored[B] => new LazyPlus(s,this)
        case s:Best[B] => Best(s.p,s.x,s.r ++ this)
        case _:EmptyOrBad => impossible
      }
  }

  // If true, failure causes are tracked via Bad.  If false, only Empty and Best are used.
  val trackErrors = true
  if (trackErrors)
    println("PERFORMANCE WARNING: Error tracking is on, Scored will be slower than otherwise")

  // No options
  sealed abstract class EmptyOrBad extends StrictScored[Nothing] {
    def p = 0
    def error: Error
    override def map[B](f: Nothing => B) = this
    override def flatMap[B](f: Nothing => Scored[B]) = this
    override def bias(p: Prob) = this
    override def productWith[B,C](s: Scored[B])(f: (Nothing,B) => C) = this
    override def _filter(f: Nothing => Boolean, error: () => String) = this
  }
  // Failure
  final class Bad(_error: => Error) extends EmptyOrBad {
    lazy val error = _error
    def ++[B](s: Scored[B]) = s match {
      case s:LazyScored[B] => new LazyPlus(s,this)
      case s:EmptyOrBad => new Bad(NestError("++ failed",List(error,s.error)))
      case _:Best[_] => this
    }
  }
  // No options, but not Bad
  object Empty extends EmptyOrBad {
    def error = OneError("unknown error")
    override def ++[B](s: Scored[B]) = s
  }

  // One best possibility, then lazily more
  final case class Best[+A](p: Prob, x: A, r: Scored[A]) extends StrictScored[A] {
    def bias(q: Prob) = Best(p*q,x,r bias q)
    def ++[B >: A](s: Scored[B]): Scored[B] =
      if (p >= s.p) Best(p,x,r ++ s)
      else s match {
        case x:LazyScored[B] => new LazyPlus(x,this)
        case Best(q,y,s) => Best(q,y,s++this)
        case _:EmptyOrBad => impossible
      }
  }

  // Lazy version of filter
  private final class LazyFilter[A](private[this] var x: Scored[A], private[this] var f: A => Boolean,
                                    private[this] var error: () => String) extends LazyScored[A] {
    val p = x.p
    private[this] var s: Scored[A] = null
    def force(p: Prob) = {
      if (s eq null) {
        @tailrec def loop(x: Scored[A], first: Boolean): Scored[A] = x match {
          case x:LazyScored[A] => if (first || x.p > p) loop(x force p,first=false)
                                  else new LazyFilter(x,f,error)
          case Best(p,x,r) => if (f(x)) Best(p,x,r._filter(f,null))
                              else loop(r,first=false)
          case x:Bad if trackErrors => x
          case _:EmptyOrBad => if (error == null) Empty
                               else new Bad(OneError(error()))
        }
        s = loop(x,first=true)
        x = null; f = null; error = null
      }
      s
    }
  }

  // Score constructors
  @inline def fail[A](error: => String): Scored[A] =
    if (trackErrors) new Bad(OneError(error))
    else Empty
  def known[A](x: A): Scored[A] = Best(1,x,Empty)
  def single[A](x: A, p: Prob): Scored[A] = Best(p,x,Empty)
  def orError[A](x: Scored[A], error: => String): Scored[A] =
    if (trackErrors) new Bad(OneError(error)) ++ x
    else x

  // Bias and delay
  @inline def biased[A](p: Prob, s: => Scored[A]): Scored[A] = new LazyBiased(p,() => s)

  // Bound and delay
  @inline def bounded[A](p: Prob, s: => Scored[A]): Scored[A] = new LazyBound(p,() => s)

  @inline def uniform[A <: AnyRef](p: Prob, xs: Array[A], error: => String): Scored[A] =
    new UniformState[A](p,xs,if (trackErrors) () => error else null).extract(0)

  @inline def uniformGood[A <: AnyRef](p: Prob, xs: Array[A]): Scored[A] =
    new UniformState[A](p,xs,null).extract(0)

  @inline def uniform[A <: AnyRef](p: Prob, xs: Seq[A], error: => String)(implicit t: ClassTag[A]): Scored[A] =
    new UniformState[A](p,xs.toArray,if (trackErrors) () => error else null).extract(0)

  @inline def listScored[A](xs: List[Alt[A]], error: => String): Scored[A] =
    new OrderedAlternativeState[A](xs,null,if (trackErrors) () => error else null).extract(0)

  // Assume no error (use Empty instead of Bad)
  @inline def multipleGood[A](xs: List[Alt[A]]): Scored[A] =
    new OrderedAlternativeState[A](xs,null,null).extract(0)

  // Requires: prob first >= prob andThen
  @inline def orderedAlternative[A](first: List[Alt[A]], andthen: () => List[Alt[A]], error: => String): Scored[A] =
    new OrderedAlternativeState[A](first,andthen,if (trackErrors) () => error else null).extract(0)

  // Fast version of x0 ++ x1 ++ ...  The list is assumed nonempty.
  @inline def multiple[A](ls: List[Scored[A]]): Scored[A] =
    new Extractor[A](new MultipleState[A](ls))

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
  private val knownNone = known(None)
  private val knownNil = known(Nil)
  def product[A](xs: Option[Scored[A]]): Scored[Option[A]] = xs match {
    case None => knownNone
    case Some(x) => x map (Some(_))
  }
  def product[A](xs: List[Scored[A]]): Scored[List[A]] = xs match {
    case Nil => knownNil
    case List(sx) => sx map (List(_))
    case sx :: sxs => sx.productWith(product(sxs))(_::_)
  }
  def productWithReversePrefixFilter[A](xs: List[Scored[A]], reverseLastElementLegal: List[A] => Boolean): Scored[List[A]] = {
    def reverseProductWithReversePrefixFilter(xs: List[Scored[A]], reverseLastElementLegal: List[A] => Boolean): Scored[List[A]] = xs match {
      case Nil => knownNil
      case sx :: sxs => sx.productWith(reverseProductWithReversePrefixFilter(sxs, reverseLastElementLegal))(_::_) filter(reverseLastElementLegal, "filtered doesn't allow product")
    }
    reverseProductWithReversePrefixFilter(xs.reverse, reverseLastElementLegal) map (_.reverse)
  }
  def productFoldLeft[A,E](e: E)(fs: List[E => Scored[(E,A)]]): Scored[(E,List[A])] =
    fs match {
      case Nil => known((e,Nil))
      case List(f) => f(e) map {case (e,x) => (e,List(x))}
      case f :: fs => f(e) flatMap {case (ex,x) => productFoldLeft(ex)(fs) map {case (exs,xs) => (exs,x::xs)}}
    }

  // thread is map followed by product
  def thread[A,B](xs: Option[A])(f: A => Scored[B]): Scored[Option[B]] = product(xs map f)
  def thread[A,B](xs: List[A])  (f: A => Scored[B]): Scored[List[B]]   = product(xs map f)

  // Scala helpers for JavaUtils
  def nestError[A](s: String, bads: List[Bad]): Scored[A] =
    if (trackErrors) bads match {
      case List(b) => b
      case bs => new Bad(NestError(s,bads map (_.error)))
    } else Empty
  def oneError[A](error: => String): Scored[A] =
    if (trackErrors) new Bad(OneError(error))
    else Empty
}
