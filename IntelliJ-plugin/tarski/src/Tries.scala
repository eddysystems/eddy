package tarski

import ambiguity.Utility._
import StringMatching.{IncrementalLevenshteinBound, EmptyIncrementalLevenshteinBound, IncrementalDistance, levenshteinDistance}
import tarski.Scores.{Alt,Prob}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Sorting
import scala.math._

object Tries {

  trait TrieVisitor[V,X] {
    // If you see k, do you want to continue descending?
    // Return None if no, a new visitor that has absorbed the k if yes.
    def next(k: Char): Option[TrieVisitor[V,X]]

    // We found these values, filter and transform to output type
    def found(t: TraversableOnce[V]): X
  }

  class Trie[V](val structure: Array[Int], val values: Array[V], private val key: V => String) {
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
          var c = s(depth).toInt
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
      makeHelper(merge(values,t.values,key),key)
    def ++(t: Iterable[V])(implicit tt: ClassTag[V]): Trie[V] =
      makeHelper(merge(values,toSorted(t,key),key),key)
  }

  object Trie {
    def apply[V](input: Iterable[V])(key: V => String)(implicit tt: ClassTag[V]): Trie[V] =
      timed("trie",makeHelper(toSorted(input,key),key))
  }

  // Sort input into an array
  private def toSorted[V](input: Iterable[V], key: V => String)(implicit tt: ClassTag[V]): Array[V] =
    timed("sort array",{
      var values = input.toArray
      Sorting.quickSort(values)(new Ordering[V] {
        def compare(x: V, y: V) = key(x).compare(key(y))
      })
      values
    })

  // Merge two sorted arrays
  private def merge[V](v0: Array[V], v1: Array[V], key: V => String)(implicit tt: ClassTag[V]): Array[V] = {
    val n0 = v0.size
    val n1 = v1.size
    val both = new Array[V](n0+n1)
    var i = 0
    var j = 0
    for (k <- 0 until n0+n1)
      both(k) = if (j==n1 || (i!=n0 && key(v0(i)) <= key(v1(j)))) { i += 1; v0(i-1) }
                else                                              { j += 1; v1(j-1) }
    both
  }

  // Assumes values is already sorted.  values must never change.
  private def makeHelper[V](values: Array[V], key: V => String): Trie[V] = {
    // Size of common prefix of two strings
    def common(x: String, y: String): Int = {
      val n = min(x.size,y.size)
      def loop(j: Int): Int =
        if (j==n || x(j)!=y(j)) j
        else loop(j+1)
      loop(0)
    }

    // Count nodes and determine maximum depth
    //      : *0-         : 1,3
    // a    : *1a#*0-     : 2,7
    // a b  : *2a#b#*0*0- : 3,11
    // a ab : *1a#*1b#*0- : 3,11
    val (nodes,depth) = timed("count",{
      var prev = ""
      var nodes = 1
      var maxSize = 0
      for (i <- 0 until values.size) {
        val k = key(values(i))
        maxSize = max(maxSize,k.size)
        nodes += k.size-common(prev,k)
        prev = k
      }
      (nodes,maxSize+1)
    })
    val structureSize = 4*nodes-1

    // Determine node information: an array of (position,start) pairs.
    val info = timed("position",{
      // At first, each info pair is (children,start)
      val info = new Array[Int](2*nodes+1)
      val stack = new Array[Int](depth)
      var prev = ""
      var n = 1
      for (i <- 0 until values.size) {
        val k = key(values(i))
        val c = common(prev,k) // Implicit truncate stack to size c+1
        if (c < k.size) {
          info(2*stack(c)) += 1
          for (j <- c+1 until k.size) {
            info(2*n) += 1
            info(2*n+1) = i
            stack(j) = n
            n += 1
          }
          info(2*n+1) = i
          stack(k.size) = n
          n += 1
        }
        prev = k
      }
      assert(n == nodes)
      // Accumulate children into position
      var total = 0
      for (n <- 0 until nodes) {
        val next = total+2+2*info(2*n)
        info(2*n) = total
        total = next
      }
      assert(total+1 == structureSize)
      info(2*nodes) = total
      info
    })

    // Allocate structure
    val structure = timed(s"allocate structure (${4*structureSize} bytes)",
      new Array[Int](structureSize))

    // Generate tree
    timed("structure",{
      // Initialize value starts.  Child counts are correctly already zero.
      for (n <- 0 until nodes)
        structure(info(2*n)) = info(2*n+1)
      structure(info(2*nodes)) = values.size
      // Fill in children
      val stack = new Array[Int](depth)
      var prev = ""
      var n = 1
      for (i <- 0 until values.size) {
        val k = key(values(i))
        val c = common(prev,k) // Implicit truncate stack to size c+1
        if (c < k.size) {
          def link(parent: Int, j: Int) = {
            val pn = info(2*parent)
            val cn = structure(pn+1)
            structure(pn+1) = cn+1
            structure(pn+2+2*cn) = k(j)
            structure(pn+2+2*cn+1) = info(2*n)
            n += 1
          }
          link(stack(c),c)
          for (j <- c+1 until k.size) {
            stack(j) = n-1
            link(n-1,j)
          }
          stack(k.size) = n-1
        }
        prev = k
      }
      assert(n == nodes)
    })

    // All done!
    new Trie(structure,values,key)
  }

  def levenshteinLookup[V](t: Trie[V], typed: String, maxDistance: Float, expected: Double, minProb: Prob): List[Alt[V]] = {
    case class LevenshteinVisitor[V](dist: IncrementalDistance) extends TrieVisitor[V,List[Alt[V]]] {
      def next(k: Char): Option[LevenshteinVisitor[V]] = {
        val d = new IncrementalLevenshteinBound(typed,dist,k)
        if (d.min > maxDistance) None else Some(LevenshteinVisitor[V](d))
      }
      def found(xs: TraversableOnce[V]): List[Alt[V]] = {
        if (dist.distance > maxDistance) Nil else {
        val d = levenshteinDistance(typed,dist.current)
        if (d > maxDistance) Nil else {
        val p = Pr.poissonPDF(expected,math.ceil(d).toInt)
        if (p < minProb) Nil else {
        xs.toList map (Alt(p,_))}}}
      }
    }
    t.lookup(LevenshteinVisitor[V](EmptyIncrementalLevenshteinBound))
  }
}