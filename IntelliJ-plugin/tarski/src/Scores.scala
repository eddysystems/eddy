package tarski

import scala.annotation.tailrec

object Scores {
  /* For now, we choose among options using frequentist statistics.  That is, we score A based on the probability
   *
   *   p = Pr(user chooses B | user thinks of A)
   *
   * where B is what we see as input.
   */

  // Wrapper around probabilities
  case class Prob(p: Double) extends AnyVal with Ordered[Prob] {
    override def compare(x: Prob) = p.compare(x.p)
    override def toString = p.toString
    def *(x: Prob) = Prob(p * x.p) // Valid only if independence or conditionality holds
  }

  // Nonempty lists of probability,value pairs.  These are *not* normalized since the probabilities go
  // the other way: they are frequentist information not distributions.
  type Probs[+A] = List[(Prob,A)]

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

  sealed abstract class Scored[+A] {
    def all: Either[Error,Probs[A]]
    def best: Either[Error,A]
    def map[B](f: A => B): Scored[B]
    def flatMap[B](f: A => Scored[B]): Scored[B] // f is assumed to generate conditional probabilities
    def ++[B >: A](s: Scored[B]): Scored[B]
  }

  // Failure
  private case class Bad(e: Error) extends Scored[Nothing] {
    def all = Left(e)
    def best = Left(e)
    def map[B](f: Nothing => B) = Bad(e)
    def flatMap[B](f: Nothing => Scored[B]) = Bad(e)
    def ++[B](s: Scored[B]) = s match {
      case Bad(f) => Bad(NestError("++ failed",List(e,f)))
      case Good(_) => s
    }
  }

  // Nonempty list of possibilities
  private case class Good[+A](c: Probs[A]) extends Scored[A] {
    def all = Right(c)
    def best = Right(c.maxBy(_._1.p)._2)

    def map[B](f: A => B) = Good(
      c map {case (s,a) => (s,f(a))})

    def flatMap[B](f: A => Scored[B]): Scored[B] = {
      def absorb(sa: Prob, bs: Probs[B], good: Probs[B]): Probs[B] = bs match {
        case Nil => good
        case (sb,b)::bs => absorb(sa,bs,(sa*sb,b)::good)
      }
      def processGood(as: Probs[A], good: Probs[B]): Scored[B] = as match {
        case Nil => Good(good)
        case (sa,a)::as => processGood(as, f(a) match {
          case Bad(_) => good
          case Good(bs) => absorb(sa,bs,good)
        })
      }
      def processBad(bad: List[Error], as: Probs[A]): Scored[B] = as match {
        case Nil => bad match {
          case List(e) => Bad(e)
          case es => Bad(NestError("flatMap failed",es))
        }
        case (sa,a)::as => f(a) match {
          case Bad(e) => processBad(e::bad,as)
          case Good(bs) => processGood(as,absorb(sa,bs,Nil))
        }
      }
      processBad(Nil,c)
    }

    def ++[B >: A](s: Scored[B]): Scored[B] = Good(s match {
      case Bad(_) => c
      case Good(sc) => c++sc
    })
  }

  // Score constructors
  def fail[A](error: String): Scored[A] = Bad(OneError(error))
  def single[A](x: A): Scored[A] = Good(List((Prob(1.0),x)))
  def single[A](x: A, p: Prob): Scored[A] = Good(List((p,x)))

  def bias[A](s: Scored[A], b: Prob): Scored[A] = s match {
    case Bad(_) => s
    case Good(g) => Good(g.map( { case (p,a) => (p*b,a) } ))
  }

  // TODO: This one is nonsense, and needs to go.
  def simple[A](xs: List[A], error: => String): Scored[A] = xs match {
    case Nil => Bad(OneError(error))
    case _ => Good(xs.map((Prob(.5),_)))
  }

  // a and b are assumed independent
  def product[A,B](a: Scored[A], b: => Scored[B]): Scored[(A,B)] = a match {
    case Bad(e) => Bad(e)
    case Good(as) => b match {
      case Bad(e) => Bad(e)
      case Good(bs) => Good(for ((sa,a) <- as; (sb,b) <- bs) yield (sa*sb,(a,b)))
    }
  }
  def product[A,B,C](a: Scored[A], b: => Scored[B], c: => Scored[C]): Scored[(A,B,C)] = a match {
    case Bad(e) => Bad(e)
    case Good(as) => b match {
      case Bad(e) => Bad(e)
      case Good(bs) => c match {
        case Bad(e) => Bad(e)
        case Good(cs) => Good(for ((sa,a) <- as; (sb,b) <- bs; (sc,c) <- cs) yield (sa*sb*sc,(a,b,c)))
      }
    }
  }
  def productWith[A,B,T](a: Scored[A], b: => Scored[B])(f: (A,B) => T): Scored[T] = a match {
    case Bad(e) => Bad(e)
    case Good(as) => b match {
      case Bad(e) => Bad(e)
      case Good(bs) => Good(for ((sa,a) <- as; (sb,b) <- bs) yield (sa*sb,f(a,b)))
    }
  }
  def productWith[A,B,C,T](a: Scored[A], b: => Scored[B], c: => Scored[C])(f: (A,B,C) => T): Scored[T] = a match {
    case Bad(e) => Bad(e)
    case Good(as) => b match {
      case Bad(e) => Bad(e)
      case Good(bs) => c match {
        case Bad(e) => Bad(e)
        case Good(cs) => Good(for ((sa,a) <- as; (sb,b) <- bs; (sc,c) <- cs) yield (sa*sb*sc,f(a,b,c)))
      }
    }
  }
  def product[A,B,C,D](a: Scored[A], b: => Scored[B], c: => Scored[C], d: => Scored[D]): Scored[(A,B,C,D)] =
    productWith(product(a,b),product(c,d))((ab,cd) => (ab._1,ab._2,cd._1,cd._2))
  def productWith[A,B,C,D,T](a: Scored[A], b: => Scored[B], c: => Scored[C], d: => Scored[D])(f: (A,B,C,D) => T): Scored[T] =
    productWith(product(a,b),product(c,d))((ab,cd) => f(ab._1,ab._2,cd._1,cd._2))

  // xs are assumed independent
  def product[A](xs: Option[Scored[A]]): Scored[Option[A]] = xs match {
    case None => single(None)
    case Some(x) => x map (Some(_))
  }
  def product[A](xs: List[Scored[A]]): Scored[List[A]] = xs match {
    case Nil => single(Nil)
    case sx :: sxs => productWith(sx,product(sxs))(_::_)
  }

  def productFoldLeft[A,E](e: E)(fs: List[E => Scored[(E,A)]]): Scored[(E,List[A])] = fs match {
    case Nil => single((e,Nil))
    case f :: fs =>
      f(e) flatMap {case (ex,x) =>
      productFoldLeft(ex)(fs) map {case (exs,xs) =>
        (exs,x::xs)}}
  }

  // thread is map followed by product
  def thread[A,B](xs: Option[A])(f: A => Scored[B]): Scored[Option[B]] = product(xs map f)
  def thread[A,B](xs: List[A])  (f: A => Scored[B]): Scored[List[B]]   = product(xs map f)
}
