package tarski

object Scores {
    /**
   * A score.
   * Really just a Float, but with some functions to encapsulate the averaging and such that happens
   */
  case class Score(s: Float) extends AnyVal with Ordered[Score] {
    override def compare(s2: Score) = s.compare(s2.s)
    override def toString = s.toString
    def +(t: Score): Score = Score(s+t.s)
  }
  val ZeroScore = Score(0)
  val OneScore = Score(1)

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
    def all: Either[Error,List[(Score,A)]]
    def best: Either[Error,A]
    def map[B](f: A => B): Scored[B]
    def filter(p: A => Boolean): Scored[A]
    def withFilter[B >: A](p: B => Boolean): Scored[B]
    def flatMap[B](f: A => Scored[B]): Scored[B]
    def product[B](f: => Scored[B]): Scored[(A,B)]
    def productWith[B,C](f: => Scored[B])(g: (A,B) => C): Scored[C]
    def collect[B](f: PartialFunction[A,B]): Scored[B]
    def ++[B >: A](s: Scored[B]): Scored[B]
  }

  // Failure
  private case class Bad[+A](e: Error) extends Scored[A] {
    def all = Left(e)
    def best = Left(e)
    def map[B](f: A => B) = Bad(e)
    def filter(p: A => Boolean) = Bad(e)
    def withFilter[B >: A](p: B => Boolean) = Bad(e)
    def flatMap[B](f: A => Scored[B]) = Bad(e)
    def product[B](f: => Scored[B]) = Bad(e)
    def productWith[B,C](f: => Scored[B])(g: (A,B) => C) = Bad(e)
    def collect[B](f: PartialFunction[A,B]) = Bad(e)
    def ++[B >: A](s: Scored[B]) = s match {
      case Bad(f) => Bad(NestError("++ failed",List(e,f)))
      case Good(_) => s
    }
  }

  // Nonempty list of possibilities
  private case class Good[+A](c: List[(Score,A)]) extends Scored[A] {
    def all = Right(c)
    def best = Right(c.maxBy(_._1.s)._2)

    def map[B](f: A => B) = Good(
      c map {case (s,a) => (s,f(a))})

    def filter(p: A => Boolean): Scored[A] =
      c filter {case (_,a) => p(a)} match {
        case Nil => Bad(OneError("filter failed"))
        case c => Good(c)
      }

    // TODO: Optimize
    def withFilter[B >: A](p: B => Boolean) = filter(p)

    def flatMap[B](f: A => Scored[B]): Scored[B] = {
      def process(good: List[(Score,B)], bad: List[Error], as: List[(Score,A)]): Scored[B] = as match {
        case Nil =>
          if (!good.isEmpty) Good(good)
          else bad match {
            case List(e) => Bad(e)
            case es => Bad(NestError("flatMap failed",es))
          }
        case (sa,a)::as => f(a) match {
          case Bad(e) => process(good,e::bad,as)
          case Good(bs) => {
            def absorb(good: List[(Score,B)], bs: List[(Score,B)]): Scored[B] = bs match {
              case Nil => process(good,bad,as)
              case (sb,b)::bs => absorb((sa+sb,b)::good,bs)
            }
            absorb(good,bs)
          }
        }
      }
      process(Nil,Nil,c)
    }

    def product[B](f: => Scored[B]): Scored[(A,B)] = f match {
      case Bad(e) => Bad(e)
      case Good(bs) => Good(for ((sa,a) <- c; (sb,b) <- bs) yield (sa+sb,(a,b)))
    }

    def productWith[B,C](f: => Scored[B])(g: (A,B) => C) = f match {
      case Bad(e) => Bad(e)
      case Good(bs) => Good(for ((sa,a) <- c; (sb,b) <- bs) yield (sa+sb,g(a,b)))
    }
        
    def collect[B](f: PartialFunction[A,B]): Scored[B] =
      (for ((s,a) <- c; b <- f.lift(a).toList) yield (s,b)) match {
        case Nil => Bad(OneError("collect failed"))
        case bs => Good(bs)
      }

    def ++[B >: A](s: Scored[B]): Scored[B] = Good(s match {
      case Bad(_) => c
      case Good(sc) => c++sc
    })
  }

  // Score constructors
  def fail[A](error: String): Scored[A] = Bad(OneError(error))
  def single[A](x: A): Scored[A] = Good(List((ZeroScore,x)))
  def simple[A](xs: List[A], error: => String): Scored[A] = xs match {
    case Nil => Bad(OneError(error))
    case _ => Good(xs.map((OneScore,_)))
  }
  def orFail[A](x: Option[A], error: => String): Scored[A] = x match {
    case Some(s) => single(s)
    case None => fail(error)
  }

  def product[A](xs: List[Scored[A]]): Scored[List[A]] = xs match {
    case Nil => single(Nil)
    case sx :: sxs => sx.productWith(product(sxs))(_::_)
  }

  def partialProduct[A,B](xs: List[Scored[A]])(f: PartialFunction[A,B]): Scored[List[B]] =
    product(xs.map(_.collect(f)))

  def productFoldLeft[A,E](e: E)(fs: List[E => Scored[(E,A)]]): Scored[(E,List[A])] = fs match {
    case Nil => single((e,Nil))
    case f :: fs =>
      for ((ex,x) <- f(e);
           (exs,xs) <- productFoldLeft(ex)(fs))
        yield (exs,x::xs)
  }
}
