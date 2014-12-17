package tarski

import tarski.Scores._
import ambiguity.Utility._
import scala.util.Random
import org.testng.annotations.Test

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
      pa(i) = .9+.1*random.nextDouble()
      fill(i+1)
    }
    fill(0)
    def p(i: Int, j: Int) = pa((j*n+i)%rn)

    // Make some alternatives
    case class D(s: Int, f: Int)
    var scores = scoped("start",listScored((0 until n map (i => Alt(p(i,0),D(i,0)))).toList, "unlikely"))

    // Repeatedly flatMap
    def f(d: D) = d match { case D(i,j) => single(D(i,j+1),p(i,j+1)) }
    0 until m foreach (i => scores = scoped(s"f $i",scores flatMap f))

    // Compute results independently
    def pi(i: Int, j: Int = 0, r: Prob = 1): Prob =
      if (j > m) r else pi(i,j+1,r*p(i,j))
    val correct = (0 until n map (i => Alt(pi(i),D(i,m)))).toList.sortBy(-_.p)
    assert(scores.stream.toList == correct)
  })
}
