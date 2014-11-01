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
    def map[B](f: A => B): Scored[B] = Scored(
      c map {case (s,a) => (s,f(a))})

    def filter(f: A => Boolean): Scored[A] = Scored(
      c filter {case (_,a) => f(a)})

    // TODO: Make an optimized version of withFilter.
    // This one exists purely to silence warnings.
    def withFilter(f: A => Boolean) = filter(f)

    def flatMap[B](f: A => Scored[B]): Scored[B] = Scored(
      for ((sa,a) <- c; (sb,b) <- f(a).c) yield (sa+sb,b))

    def product[B](f: => Scored[B]): Scored[(A,B)] = Scored(
      if (c.isEmpty) Nil
      else {
        val bs = f.c
        if (bs.isEmpty) Nil
        else for ((sa,a) <- c; (sb,b) <- bs) yield (sa+sb,(a,b))
      })

    def collect[B](f: PartialFunction[A,B]): Scored[B] = Scored(
      for ((s,a) <- c; b <- f.lift(a).toList) yield (s,b))
  }

  // Score constructors
  val fail = new Scored(Nil)
  def single[A](x: A): Scored[A] = Scored(List((ZeroScore,x)))
  def simple[A](xs: List[A]): Scored[A] = Scored(xs.map((OneScore,_)))
  def option[A](x: Option[A]): Scored[A] = x match {
    case Some(s) => single(s)
    case None => fail
  }

  def product[A](xs: List[Scored[A]]): Scored[List[A]] = xs match {
    case Nil => single(Nil)
    case Scored(Nil) :: _ => fail
    case sx :: sxs => sx product product(sxs) map {case (x,xs) => x::xs}
  }

  def partialProduct[A,B](xs: List[Scored[A]])(f: PartialFunction[A,B]): Scored[List[B]] =
    product(xs.map(_.collect(f)))
}
