package tarski

import java.util

import ambiguity.Utility._
import tarski.Scores._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Tries {

  trait Named extends Comparable[Named] {
    def name: String
    def compareTo(o: Named) = name.compareTo(o.name)
  }

  trait TrieVisitor[V,X] {
    // If you see k, do you want to continue descending?
    // Return None if no, a new visitor that has absorbed the k if yes.
    def next(k: Char): Option[TrieVisitor[V,X]]

    // We found these values, filter and transform to output type
    def found(t: TraversableOnce[V]): X
  }

  class Trie[V <: Named](val structure: Array[Int], val values: Array[V]) {
    // structure contains tuples: (start_values,n_children,(character,start_idx)*)

    // A node in structure is
    //   lo,hi: Int = start and end index of values for this node
    //   count: number of children
    //   sorted list of (char, child node index) pairs, one for each child node
    // This representation is O(n) insert, iterating over values is easy though (to merge Tries)
    // There is a sentinel node on the end, with only a start_values.

    // Trie   a -> x, ab -> y, ac -> z
    // structure = [0,1,'a',5,
    //              0,2,'b',12,'c',14
    //              1,0,
    //              2] // sentinel
    // values = [x,y,z]

    def nodeValues(node: Int) =
      values.view(structure(node),structure(node+2+2*structure(node+1)))

    def children(node: Int): IndexedSeq[(Char,Int)] = {
      class ChildList extends IndexedSeq[(Char,Int)] {
        def length: Int = structure(node+1)
        def apply(i: Int): (Char,Int) = {
          val child = node+2+2*i
          (structure(child).toChar,structure(child+1))
        }
      }
      new ChildList
    }

    def lookup[X](v: TrieVisitor[V,List[X]]): List[X] = {
      var all: List[List[X]] = Nil
      def loop(v: TrieVisitor[V,List[X]], node: Int): Unit = {
        all = v.found(nodeValues(node)) :: all
        children(node) foreach { case (c,i) => v next c foreach (loop(_,i)) }
      }
      loop(v,0)
      all.flatten
    }

    // Find an exact match
    def exact(s: String): List[V] = {
      val n = s.size
      @tailrec
      def loop(node: Int, depth: Int): List[V] =
        if (depth == n) nodeValues(node).toList
        else {
          val c = s(depth).toInt
          var lo = 0
          var hi = structure(node+1)
          while (lo < hi) {
            val mid = (lo+hi)>>1
            val x = structure(node+2+2*mid)
            if (c == x) { lo = mid; hi = mid-1 }
            else if (c < x) hi = mid
            else lo = mid+1
          }
          if (lo == hi) Nil
          else loop(structure(node+2+2*lo+1),depth+1)
        }
      loop(0,0)
    }

    // The two keys are assumed equal
    def ++(t: Trie[V])(implicit tt: ClassTag[V]): Trie[V] =
      scoped("trie merge",makeHelper(merge(values,t.values)))
    def ++(t: Iterable[V])(implicit tt: ClassTag[V]): Trie[V] =
      scoped("trie extend",makeHelper(merge(values,toSorted(t))))
  }

  object Trie {
    def apply[V <: Named](input: Iterable[V])(implicit tt: ClassTag[V]): Trie[V] =
      scoped("trie create",makeHelper(toSorted(input)))

    def empty[V <: Named](implicit tt: ClassTag[V]): Trie[V] =
      makeHelper(Array.empty)
  }

  // Sort input into an array
  private def toSorted[V <: Named](input: Iterable[V])(implicit tt: ClassTag[V]): Array[V] =
    scoped("sort array",{
      var values = input.toArray
      util.Arrays.sort(values.asInstanceOf[Array[Object]])
      values
    })

  // Merge two sorted arrays
  private def merge[V <: Named](v0: Array[V], v1: Array[V])(implicit tt: ClassTag[V]): Array[V] = {
    val n0 = v0.size
    val n1 = v1.size
    val both = new Array[V](n0+n1)
    var i = 0
    var j = 0
    for (k <- 0 until n0+n1)
      both(k) = if (j==n1 || (i!=n0 && v0(i).name <= v1(j).name)) { i += 1; v0(i-1) }
                else                                              { j += 1; v1(j-1) }
    both
  }

  // Assumes values is already sorted.  values must never change.
  private def makeHelper[V <: Named](values: Array[V]): Trie[V] = {
    new Trie(JavaTrie.makeTrieStructure(values.asInstanceOf[Array[Named]]),values)
  }

  def levenshteinLookup[V <: Named](t: Trie[V], typed: String, maxDistance: Float, expected: Double, minProb: Double): List[Alt[V]] = {
    JavaTrie.levenshteinLookup(t,typed,maxDistance,expected,minProb)
/*
    case class LevenshteinVisitor(dist: StringMatching.IncrementalDistance) extends TrieVisitor[V,List[Alt[V]]] {
      def next(k: Char): Option[LevenshteinVisitor] = {
        val d = new StringMatching.IncrementalLevenshteinBound(typed,dist,k)
        if (d.min > maxDistance) None else Some(LevenshteinVisitor(d))
      }
      def found(xs: TraversableOnce[V]): List[Alt[V]] = {
        if (dist.distance > maxDistance) Nil else {
        val d = StringMatching.levenshteinDistance(typed,dist.current)
        if (d > maxDistance) Nil else {
        val p = Pr.poissonPDF(expected,math.ceil(d).toInt)
        if (p < minProb) Nil else {
        xs.toList map (Alt(p,_))}}}
      }
    }
    t.lookup(LevenshteinVisitor(StringMatching.EmptyIncrementalLevenshteinBound))
  */

  }
}

