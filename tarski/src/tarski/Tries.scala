package tarski

import java.util
import tarski.JavaTrie.Generator;
import tarski.Items.Item
import tarski.Scores._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.collection.JavaConverters._

object Tries {

  trait Named extends Comparable[Named] {
    def name: String
    def compareTo(o: Named) = name.compareTo(o.name)
  }

  trait Queriable[V] {
    def exact(s: Array[Char]): List[V]
    def typoQuery(typed: Array[Char]): Scored[V]
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

  class Trie[V <: Named](val structure: Array[Int], val values: Array[V], lookup: Generator[V])
                         extends LazyTrie[V](structure,lookup) {
    def ++(t: Trie[V])(implicit tt: ClassTag[V]): Trie[V] =
      Trie.makeHelper(merge(values,t.values))
    def ++(t: Iterable[V])(implicit tt: ClassTag[V]): Trie[V] =
      Trie.makeHelper(merge(values,toSorted(t)))
  }

  object Trie {
    def apply[V <: Named](input: Iterable[V])(implicit tt: ClassTag[V]): Trie[V] =
      makeHelper(toSorted(input))

    def empty[V <: Named](implicit tt: ClassTag[V]): Trie[V] =
      makeHelper(Array.empty)

    // Assumes values is already sorted.  values must never change.
    private def makeHelper[V <: Named](values: Array[V]): Trie[V] = {
      val map = values.groupBy(_.name).asJava
      new Trie(JavaTrie.makeTrieStructure(values.map(_.name)),values,new Generator[V] {
        def lookup(name: String) = map.get(name)
      })
    }
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
}

