package tarski

object Scores {
  /* For now, we choose among options using frequentist statistics.  That is, we score A based on the probability
   *
   *   p = Pr(user chooses B | user thinks of A)
   *
   * where B is what we see as input.
   */

  // Wrapper around probabilities
  case class Prob(p: Float) extends AnyVal with Ordered[Prob] {
    override def compare(x: Prob) = p.compare(x.p)
    override def toString = p.toString
    def *(x: Prob) = Prob(p * x.p) // Valid only if independence or conditionality holds
  }

  // Distributions, which must be nonzero length and normalized
  type Dist[+A] = List[(Prob,A)]

  // Normalize a distribution into a valid Scored
  def normalize[A](xs: Dist[A]): Scored[A] = {
    assert(xs.nonEmpty)
    def total(xs: Dist[A], t: Float): Float = xs match {
      case Nil => t
      case (Prob(p),_)::xs => total(xs,t+p)
    }
    val s = 1/total(xs,0)
    Good(xs map {case (Prob(p),x) => (Prob(s*p),x)})
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

  sealed abstract class Scored[+A] {
    def all: Either[Error,Dist[A]]
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
  private case class Good[+A](c: Dist[A]) extends Scored[A] {
    def all = Right(c)
    def best = Right(c.maxBy(_._1.p)._2)

    def map[B](f: A => B) = Good(
      c map {case (s,a) => (s,f(a))})

    def flatMap[B](f: A => Scored[B]): Scored[B] = {
      def absorb(good: Dist[B], sa: Prob, as: Dist[A], bs: Dist[B]): Scored[B] = bs match {
        case Nil => processGood(good,as)
        case (sb,b)::bs => absorb((sa*sb,b)::good,sa,as,bs)
      }
      def processGood(good: Dist[B], as: Dist[A]): Scored[B] = as match {
        case Nil => normalize(good)
        case (sa,a)::as => f(a) match {
          case Bad(_) => processGood(good,as)
          case Good(bs) => absorb(good,sa,as,bs)
        }
      }
      def processBad(bad: List[Error], as: Dist[A]): Scored[B] = as match {
        case Nil => bad match {
          case List(e) => Bad(e)
          case es => Bad(NestError("flatMap failed",es))
        }
        case (sa,a)::as => f(a) match {
          case Bad(e) => processBad(e::bad,as)
          case Good(bs) => absorb(Nil,sa,as,bs)
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
  def single[A](x: A): Scored[A] = Good(List((Prob(1),x)))
  def simple[A](xs: List[A], error: => String): Scored[A] = xs match {
    case Nil => Bad(OneError(error))
    case _ =>
      val p = Prob(1/xs.size)
      Good(xs.map((p,_)))
  }
  def orFail[A](x: Option[A], error: => String): Scored[A] = x match {
    case Some(s) => single(s)
    case None => fail(error)
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
  def productWith[A,B,C](a: Scored[A], b: => Scored[B])(f: (A,B) => C): Scored[C] = a match {
    case Bad(e) => Bad(e)
    case Good(as) => b match {
      case Bad(e) => Bad(e)
      case Good(bs) => Good(for ((sa,a) <- as; (sb,b) <- bs) yield (sa*sb,f(a,b)))
    }
  }

  // xs are assumed independent
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
}
