package tarski

import java.util
import tarski.JavaTrie.Generator;
import tarski.Items.Item
import tarski.Scores._

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Tries {

  trait Named extends Comparable[Named] {
    def name: String
    def compareTo(o: Named) = name.compareTo(o.name)
  }

  trait Queriable[V] {
    def exact(s: Array[Char]): List[V]
    def typoQuery(typed: Array[Char]): Scored[V]
  }

  class Trie[V <: Named](val structure: Array[Int], val values: Array[V]) extends Queriable[V] {
    // structure contains tuples: (start_values,n_children,(character,start_idx)*)

    // A node in structure is
    //   lo: Int = start index of values for this node (end index is in next node, or sentinel if last)
    //   count: number of children
    //   sorted list of (char, child node index) pairs, one for each child node
    // This representation is O(n) insert, iterating over values is easy though (to merge Tries)
    // There is a sentinel node on the end, with only a start index in values array.

    // Trie   a -> x, ab -> y, ac -> z
    // structure = [0,1,'a',5,
    //              0,2,'b',11,'c',13
    //              1,0,
    //              2,0,
    //              3] // sentinel (always size of values)
    // values = [x,y,z]

    protected def nodeValues(node: Int) =
      values.view(structure(node),structure(node+2+2*structure(node+1)))

    protected def children(node: Int): IndexedSeq[(Char,Int)] = {
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
      val n = JavaTrie.exactNode(this.structure,s)
      if (n < 0) Nil
      else nodeValues(n).toList
    }

    // Lookup a string approximately only (ignoring exact matches)
    @inline def typoQuery(typed: Array[Char]): Scored[V] =
      JavaTrie.levenshteinLookup(structure, values, typed, Pr.maxTypos(typed.length), Pr.expectedTypos(typed.length), Pr.minimumProbability)

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

  class LazyTrie[V](structure: Array[Int], lookup: Generator[V]) extends Queriable[V] {
    override def exact(s: Array[Char]): List[V] =
      if (JavaTrie.exactNode(structure,s) < 0) Nil
      else lookup.lookup(new String(s)) match {
        case null => Nil
        case xs => xs.toList
      }

    override def typoQuery(typed: Array[Char]): Scored[V] =
      JavaTrie.levenshteinLookupGenerated(structure, lookup, typed, Pr.maxTypos(typed.length), Pr.expectedTypos(typed.length), Pr.minimumProbability)
  }

  object LazyTrie {
    def apply[V](input: Iterable[String], lookup: Generator[V]) =
      new LazyTrie[V](JavaTrie.makeTrieStructure(toSorted(input)), lookup)
  }

  // Sort input into an array
  private def toSorted[V <: Named](input: Iterable[V])(implicit tt: ClassTag[V]): Array[V] = {
    val values = input.toArray
    util.Arrays.sort(values.asInstanceOf[Array[Object]])
    values
  }

  private def toSorted(input: Iterable[String]): Array[String] = {
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

  // Assumes values is already sorted.  values must never change.
  private def makeHelper[V <: Named](values: Array[V]): Trie[V] =
    new Trie(JavaTrie.makeTrieStructure(values.asInstanceOf[Array[Named]]),values)
}

