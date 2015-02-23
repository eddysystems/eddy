package tarski

import utility.Utility._
import utility.JavaUtils.hash
import tarski.JavaScores._
import tarski.Scores._
import scala.util.Random
import org.testng.annotations.Test
import org.testng.AssertJUnit._

class TestScores {
  @Test def scores() = scoped("scores",{
    // flatMap n things m times
    val n = 1000
    val m = 1000

    // Precompute probabilities so that everything is order independent
    val random = new Random(17311)
    val rn = 2417
    val pa = new Array[Prob](rn)
    def fill(i: Int): Unit = if (i < pa.size) {
      pa(i) = Prob("random",.9+.1*random.nextDouble())
      fill(i+1)
    }
    fill(0)
    def p(i: Int, j: Int) = pa((j*n+i)%rn)

    // Make some alternatives
    case class D(s: Int, f: Int)
    var scores = scoped("start",list((0 until n map (i => Alt(p(i,0),D(i,0)))).toList, "unlikely"))

    // Repeatedly flatMap
    def f(d: D) = d match { case D(i,j) => single(D(i,j+1),p(i,j+1)) }
    0 until m foreach (i => scores = scores flatMap f)

    // Compute results independently
    def pi(i: Int, j: Int = 0, r: Double = 1): Double =
      if (j > m) r else pi(i,j+1,r*pp(p(i,j)))
    val correct = (0 until n map (i => Alt(Prob("pi",pi(i)),D(i,m)))).toList.sortBy(-_.p)
    val result = scoped("force",scores.stream.toList)
    scoped("check",assert(result == correct))
  })

  def alts[A](name: String, n: Int, seed: Int): List[Alt[String]] = {
    val random = new Random(seed)
    (0 until n).toList map (i => { val s = s"$name$i"; Alt(Prob(s,random.nextDouble()),s) })
  }

  def test[A](x: List[Alt[A]], y: Scored[A]) =
    assertEquals(x sortBy (-_.p),y.stream.toList)

  @Test def bias() = {
    val ax = alts("x",20,seed=81241)
    val p = Prob("bias",.7)
    test(ax map {case Alt(q,x) => Alt(pmul(p,q),x)},biased(p,listGood(ax)))
  }

  @Test def map() = {
    val ax = alts("x",20,seed=1281)
    val x = listGood(ax)
    def f(s: String) = ("f",s)
    test(ax map {case Alt(p,x) => Alt(p,f(x))},x map f)
  }

  @Test def plus() = {
    val ax = alts("x",20,seed=13841)
    val ay = alts("y",23,seed=12731)
    test(listGood(ax++ay).stream.toList,listGood(ax)++listGood(ay))
  }

  @Test def flatMap() = {
    val ax = alts("x",20,seed=531)
    val x = listGood(ax)
    def f(x: String) = alts("fx",17,seed=x.hashCode) map {case Alt(p,y) => Alt(p,(x,y))}
    def af(x: Alt[String]) = f(x.x) map {case Alt(p,y) => Alt(pmul(x.dp,p),y)}
    def sf(x: String) = listGood(f(x))
    test(ax flatMap af,x flatMap sf)
  }

  @Test def flatMapBad() = {
    for (bad <- 0 until 7) {
      val ax = alts("x",20,seed=531)
      val x = listGood(ax)
      def f(x: String) = if ((x.hashCode&7)==bad) Nil else alts("fx",17,seed=x.hashCode) map {case Alt(p,y) => Alt(p,(x,y))}
      def af(x: Alt[String]) = f(x.x) map {case Alt(p,y) => Alt(pmul(x.dp,p),y)}
      def sf(x: String) = list(f(x),"bad")
      test(ax flatMap af,x flatMap sf)
    }
  }

  @Test def productWith() = {
    val ax = alts("x",20,seed=12412)
    val ay = alts("y",17,seed=2412)
    val x = listGood(ax)
    val y = listGood(ay)
    def f[A,B](x: A,y: B) = ("f",x,y)
    test(for (Alt(p,x) <- ax; Alt(q,y) <- ay) yield Alt(pmul(p,q),f(x,y)),x.productWith(y)(f))
  }

  @Test def filter() = {
    val ax = alts("x",30,seed=8931)
    val x = listGood(ax)
    def f(s: String) = (s.hashCode&1)==0
    val af = ax filter (a => f(a.x))
    assertEquals(15,af.size)
    test(af,x.filter(f,"fail"))
  }

  @Test def collect() = {
    case class B(s: String)
    val ax = alts("x",30,seed=8931)
    val x = listGood(ax)
    def good(s: String): Boolean = (s.hashCode&1)==0
    def f: PartialFunction[String,B] = { case x if good(x) => B(x) }
    val af = ax collect {case Alt(p,x) if good(x) => Alt(p,B(x))}
    assertEquals(15,af.size)
    test(af,x.collect(f,"fail"))
  }

  @Test def whatever() = {
    sealed abstract class Exp                               { def expand: Scored[Exp] }
    case object No extends Exp                              { def expand = known(this) }
    case class Yes(n: Int, x: Exp) extends Exp              { def expand = x.expand map (Yes(n,_)) }
    case class Whatever(b: Exp, r: Scored[Exp]) extends Exp { def expand = b.expand ++ r.flatMap(_.expand) }

    def list(key: Int, n: Int): Exp =
      if (n == 0) No
      else Yes(key^n,list(key,n-1))

    def p(n: Int): Prob = Prob(s"n$n",1.0/Int.MaxValue*(hash(n)&Int.MaxValue))

    def tweak(wk: Int, e: Exp): Scored[Exp] = e match {
      case No => known(No)
      case Yes(n,x) => biased(Prob("blah",p(n)),{
        val s = tweak(wk,x) flatMap (x =>
          if (hash(n)%3 == 0) known(Yes(hash(n^12132),x)) ++ single(Yes(hash(n^981231),x),p(n^5311))
          else known(Yes(hash(n^812321),x))
        )
        if (hash(n^wk)%3 == 0) Scores.whatever(s)(Whatever)
        else s
      })
    }

    def close[A](xs: List[Alt[A]], ys: List[Alt[A]], tol: Double = 1e-10): Boolean = (xs,ys) match {
      case (Nil,Nil) => true
      case (Alt(p,x)::xs,Alt(q,y)::ys) => Math.abs(p-q)<tol && x==y && close(xs,ys)
      case _ => false
    }

    val base = list(1721,20)
    def run(wk: Int) = tweak(wk,base).flatMap(_.expand).stream.toList
    val correct = run(0)
    assert(correct.size == 32)
    for (wk <- 1281 to 1300)
      assert(close(correct,run(wk)))
  }

  // Warn if debugging is left on
  @Test def noTrackErrors() = assertEquals(false,trackErrors)
  @Test def noTrackProbabilities() = assertEquals(false,trackProbabilities)
}
