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

  def combine(s: List[Score]): Score =
    s.reduce((x,y) => Score(x.s + y.s))

  case class Scored[+A](c: List[(Score,A)]) extends AnyVal {
    // If there are any options, pick the best one
    def best: Option[A] = c match {
      case Nil => None
      case c => Some(c.maxBy(_._1.s)._2)
    }

    def map[B](f: A => B): Scored[B] = Scored(
      c map {case (s,a) => (s,f(a))})

    def filter(f: A => Boolean): Scored[A] = Scored(
      c filter {case (_,a) => f(a)})

    def withFilter[B >: A](p: B => Boolean) = FilteredScored[B](c,p)

    def flatMap[B](f: A => Scored[B]): Scored[B] = Scored(
      for ((sa,a) <- c; (sb,b) <- f(a).c) yield (sa+sb,b))

    def product[B](f: => Scored[B]): Scored[(A,B)] = Scored(
      if (c.isEmpty) Nil
      else {
        val bs = f.c
        if (bs.isEmpty) Nil
        else for ((sa,a) <- c; (sb,b) <- bs) yield (sa+sb,(a,b))
      })

    def productWith[B,C](f: => Scored[B])(g: (A,B) => C) = Scored(
      if (c.isEmpty) Nil
      else {
        val bs = f.c
        if (bs.isEmpty) Nil
        else for ((sa,a) <- c; (sb,b) <- bs) yield (sa+sb,g(a,b))
      })

    def collect[B](f: PartialFunction[A,B]): Scored[B] = Scored(
      for ((s,a) <- c; b <- f.lift(a).toList) yield (s,b))

    def ++[B >: A](s: Scored[B]): Scored[B] = Scored(c++s.c)
  }

  case class FilteredScored[A](c: List[(Score,A)], p: A => Boolean) {
    def map[B](f: A => B): Scored[B] = Scored(
      c collect {case (s,a) if p(a) => (s,f(a))})

    def filter(q: A => Boolean) = FilteredScored[A](c,x => p(x) && q(x))

    def flatMap[B](f: A => Scored[B]): Scored[B] = Scored(
      for ((sa,a) <- c; if p(a); (sb,b) <- f(a).c) yield (sa+sb,b))
  }

  // Score constructors
  val fail = Scored(Nil)
  def single[A](x: A): Scored[A] = Scored(List((ZeroScore,x)))
  def simple[A](xs: List[A]): Scored[A] = Scored(xs.map((OneScore,_)))
  def option[A](x: Option[A]): Scored[A] = x match {
    case Some(s) => single(s)
    case None => fail
  }

  def product[A](xs: List[Scored[A]]): Scored[List[A]] = xs match {
    case Nil => single(Nil)
    case Scored(Nil) :: _ => fail
    case sx :: sxs => sx.productWith(product(sxs))(_::_)
  }

  def partialProduct[A,B](xs: List[Scored[A]])(f: PartialFunction[A,B]): Scored[List[B]] =
    product(xs.map(_.collect(f)))
}
