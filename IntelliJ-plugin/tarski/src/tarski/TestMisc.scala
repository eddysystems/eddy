package tarski

import tarski.Scores._
import tarski.JavaScores._
import ambiguity.Utility._
import scala.util.Random
import org.testng.annotations.Test
import org.testng.AssertJUnit._

class TestMisc {
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
    0 until m foreach (i => scores = scoped(s"f $i",scores flatMap f))

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
}
