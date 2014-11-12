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
    def flatMap[B](f: A => Scored[B]): Scored[B]
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
  private case class Good[+A](c: List[(Score,A)]) extends Scored[A] {
    def all = Right(c)
    def best = Right(c.maxBy(_._1.s)._2)

    def map[B](f: A => B) = Good(
      c map {case (s,a) => (s,f(a))})

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

  def product[A,B](a: Scored[A], b: => Scored[B]): Scored[(A,B)] = a match {
    case Bad(e) => Bad(e)
    case Good(as) => b match {
      case Bad(e) => Bad(e)
      case Good(bs) => Good(for ((sa,a) <- as; (sb,b) <- bs) yield (sa+sb,(a,b)))
    }
  }
  def product[A,B,C](a: Scored[A], b: => Scored[B], c: => Scored[C]): Scored[(A,B,C)] = a match {
    case Bad(e) => Bad(e)
    case Good(as) => b match {
      case Bad(e) => Bad(e)
      case Good(bs) => c match {
        case Bad(e) => Bad(e)
        case Good(cs) => Good(for ((sa,a) <- as; (sb,b) <- bs; (sc,c) <- cs) yield (sa+sb+sc,(a,b,c)))
      }
    }
  }
  def productWith[A,B,C](a: Scored[A], b: => Scored[B])(f: (A,B) => C): Scored[C] = a match {
    case Bad(e) => Bad(e)
    case Good(as) => b match {
      case Bad(e) => Bad(e)
      case Good(bs) => Good(for ((sa,a) <- as; (sb,b) <- bs) yield (sa+sb,f(a,b)))
    }
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
}
