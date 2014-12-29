package tarski

import java.util

import tarski.Scores._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Tries {

  trait Named extends Comparable[Named] {
    def name: String
    def compareTo(o: Named) = name.compareTo(o.name)
  }

  trait Delable {
    var deleted: Boolean = false
    def delete(): Unit = {
      deleted = true
    }
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

    // Find an exact match
    @inline def exact(s: Array[Char]): List[V] = {
      val n = JavaTrie.exactNode(this,s)
      if (n < 0) Nil
      else nodeValues(n).toList
    }

    // The two keys are assumed equal
    def ++(t: Trie[V])(implicit tt: ClassTag[V]): Trie[V] =
      makeHelper(merge(values,t.values))
    def ++(t: Iterable[V])(implicit tt: ClassTag[V]): Trie[V] =
      makeHelper(merge(values,toSorted(t)))
  }

  object Trie {
    def apply[V <: Named](input: Iterable[V])(implicit tt: ClassTag[V]): Trie[V] =
      makeHelper(toSorted(input))

    def empty[V <: Named](implicit tt: ClassTag[V]): Trie[V] =
      makeHelper(Array.empty)
  }

  // a trie from which you can delete items
  class DTrie[V <: Named with Delable](override val structure: Array[Int], override val values: Array[V]) extends Trie[V](structure, values) {

    // nodevalues is a low-level function which will return deleted values. filter them yourself.

    @inline override def exact(s: Array[Char]): List[V] = {
      super.exact(s) filter (!_.deleted)
    }
    override def ++(t: Trie[V])(implicit tt: ClassTag[V]): DTrie[V] =
      makeDHelper(mergeDelable(values,t.values)) // some wasted ifs if t is not a DTrie
    override def ++(t: Iterable[V])(implicit tt: ClassTag[V]): DTrie[V] =
      makeDHelper(mergeDelable(values,toSorted(t))) // some wasted ifs because t shouldn't contain deleted items
  }

  object DTrie {
    def apply[V <: Named with Delable](input: Iterable[V])(implicit tt: ClassTag[V]): DTrie[V] =
      makeDHelper(toSorted(input))

    def empty[V <: Named with Delable](implicit tt: ClassTag[V]): DTrie[V] =
      makeDHelper(Array.empty)
  }

  // Sort input into an array
  private def toSorted[V <: Named](input: Iterable[V])(implicit tt: ClassTag[V]): Array[V] = {
    val values = input.toArray
    util.Arrays.sort(values.asInstanceOf[Array[Object]])
    values
  }

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
  private def mergeDelable[V <: Named with Delable](v0: Array[V], v1: Array[V])(implicit tt: ClassTag[V]): Array[V] = {
    val n0 = v0.size
    val n1 = v1.size
    var nd = 0
    for (k <- 0 until n0)
      if (v0(k).deleted) nd += 1
    for (k <- 0 until n1)
      if (v1(k).deleted) nd += 1
    val nc = n0+n1-nd
    val both = new Array[V](nc)
    var i = 0
    var j = 0
    var k = 0

    while(k != nc) {
      if (i == n0) {
        // copy the rest of v1
        while (k != nc) {
          while(v1(j).deleted) j += 1
          both(k) = v1(j)
          k += 1
        }
      } else if (j == n1) {
        // copy the rest of v0
        while (k != nc) {
          while(v0(i).deleted) i += 1
          both(k) = v0(i)
          k += 1
        }
      } else {
        while(i != n0 && v0(i).deleted) i += 1
        while(j != n1 && v1(j).deleted) j += 1
        both(k) = if (v0(i).name <= v1(j).name) { i+=1; v0(i-1) } else { j+=1; v1(j-1) }
         k += 1
      }
    }
    both
  }

  // Assumes values is already sorted.  values must never change.
  private def makeHelper[V <: Named](values: Array[V]): Trie[V] = {
    new Trie(JavaTrie.makeTrieStructure(values.asInstanceOf[Array[Named]]),values)
  }

  private def makeDHelper[V <: Named with Delable](values: Array[V]): DTrie[V] = {
    new DTrie(JavaTrie.makeTrieStructure(values.asInstanceOf[Array[Named]]),values)
  }
}

