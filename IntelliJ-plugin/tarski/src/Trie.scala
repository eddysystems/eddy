package tarski

import tarski.StringMatching.{IncrementalLevenshteinBound, EmptyIncrementalLevenshteinBound, IncrementalDistance, levenshteinDistance}

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Trie {

  trait TrieVisitor[V,X] {
    // you see k, want to continue descending? Return none if no, a new visitor that has absorbed the k if yes.
    def pass(k: Char): Option[TrieVisitor[V,X]]
    // we found these values, filter and transform to output type
    def found(t: TraversableOnce[V]): X
  }

  trait Lookupable[V] {
    def lookup[X](visitor: TrieVisitor[V,List[X]]): List[X]
  }

  class CompactTrie[V]( _values: Iterable[V], key: V => String )(implicit t: ClassTag[V]) extends Lookupable[V] {

    val initial_size_multiplier = 10 // average 10 ints per input value -- this is likely much too low
    val grow_size_multiplier = 1.2 // grow by 20 percent when running out of space

    def ensureSpace(size: Int): Unit = if (size > structure.size) {
      println("increasing trie memory size to " + structure.size * 4)
      structure = structure.padTo((size * grow_size_multiplier).ceil.toInt, 0)
    }

    private val vsize = _values.size

    println("allocating (hopefully) " + 4 * initial_size_multiplier*vsize + " bytes.")
    private var structure = new Array[Int](initial_size_multiplier*vsize)
    val values = new Array[V](vsize)

    private var structure_pos = 0
    private var values_pos = 0

    makeNode(_values, 0)

    // structure contains tuples: (begin_values,n_values,n_children,(character,start_idx)*)

    // A node in structure is
    //   lo,hi: Int = start and end index of values for this node
    //   count: number of children
    //   sorted list of (char, child node index) pairs, one for each child node
    // this representation is O(n) insert, iterating over values is easy though (to merge Tries)

    // Trie   a -> x, ab -> y, ac -> z
    // structure = [0,0,1,'a',5,
    //              0,1,2,'b',12,'c',14
    //              1,2,0]
    // values = [x,y,z]

    // make a node whose subtree contains the given array, and write the structure into the structure array (and values
    // into the values array), at the position given by structure_pos (and values_pos for values), which are adjusted.
    private def makeNode(vs: Iterable[V], depth: Int): Unit = {
      val parts = vs groupBy { a => key(a).lift(depth) }
      val nodevalues = parts.getOrElse(None, Nil)
      val nvalues = nodevalues.size
      val nchildren = if (nodevalues == Nil) parts.size else parts.size-1
      val nodesize = 3 + 2 * nchildren
      val nodestart = structure_pos

      // resize structure to fit this node
      ensureSpace(nodestart+nodesize)

      // write value begin/end to structure
      structure(nodestart) = values_pos
      structure(nodestart+1) = values_pos + nvalues

      // write our own values to values (and move value_pos)
      assert(values_pos + nvalues <= values.length)
      nodevalues.copyToArray(values, values_pos)
      values_pos += nvalues

      // write number of children to structure
      structure(nodestart+2) = nchildren
      // advance structure_pos (leave space for child list)
      structure_pos += nodesize

      // for each child:
      var entryidx = nodestart + 3
      parts.foreach { case (Some(c),vs) => {
        // - fill in correct child character and start index
        structure(entryidx) = c.toInt
        structure(entryidx+1) = structure_pos
        entryidx += 2

        // - call makeNode for c's subtree
        makeNode(vs, depth+1)
      } case (None,_) => Unit }
    }

    private def nodevalues(nodeidx: Int) = values.view(structure(nodeidx), structure(nodeidx+1))

    private def children(nodeidx: Int): List[(Char,Int)] =
      (0 until structure(nodeidx+2)).map { x => {
        val childidx = nodeidx + 3 + 2 * x
        (structure(childidx).toChar, structure(childidx+1))
      } }.toList

    // TODO: for single items, adding them by inserting nodes is probably faster
    def add(vs: Iterable[V]): CompactTrie[V] = {
      new CompactTrie(vs++values, key)
    }

    def lookup[X](visitor: TrieVisitor[V,List[X]], nodeidx: Int): List[X] = {
      val myValues = visitor.found(nodevalues(nodeidx))
      val childValues = children(nodeidx) flatMap { case (c,idx) =>
        visitor.pass(c) match {
          case None => Nil
          case Some(v) => lookup(v, idx)
        }
      }
      myValues ++ childValues
    }

    def lookup[X](visitor: TrieVisitor[V,List[X]]): List[X] = lookup(visitor,0)
  }



  // create a new Trie with the given xs more efficiently than using add
  def fromIterable[V](xs: Iterable[(String,V)], pos: Int = 0): Trie[V] = {
    println(s"making Trie level $pos, ${xs.size} items")
    // partition xs according to char at position pos. All those with no char there (because they're too short) end up in values
    val parts = xs groupBy { case (s,v) => s.lift(pos) }
    // make Map of Tries and values
    val values = parts.getOrElse(None, Nil) map (_._2)
    val nodes = parts collect { case (Some(c),xs) => (c,fromIterable(xs,pos+1)) }
    Trie(values.toList,nodes)
  }

  // if we have a different type in the list and a builder for what we need, use that
  def fromIterableBuilder[A,V](xs: Iterable[A], build: A => (String,V), pos: Int = 0): Trie[V] = {
    println(s"making Trie level $pos, ${xs.size} items")
    // partition xs according to char at position pos. All those with no char there (because they're too short) end up in values
    val parts = xs groupBy { a => build(a)._1.lift(pos) }
    // make Map of Tries and values
    val values = parts.getOrElse(None, Nil) map (build(_)._2)
    val nodes = parts collect { case (Some(c),xs) => (c,fromIterableBuilder(xs,build,pos+1)) }
    Trie[V](values.toList,nodes)
  }

  case class Trie[V](private val values: List[V] = Nil, private val nodes: Map[Char, Trie[V]] = Map[Char,Trie[V]]()) extends Lookupable[V] {

    // return a new trie containing everything in here and the values in xs
    @tailrec
    final def add(xs: Iterable[(String,V)]): Trie[V] = xs match {
      case x if xs.isEmpty => this
      case (s,v)::xs => add(s.toSeq,v).add(xs)
    }

    // return a new Trie with s,v added
    def add(s: Seq[Char], v: V): Trie[V] = {
      s match {
        case Seq() => Trie[V](v :: values, nodes)
        case Seq(h,t @ _*) => Trie[V](values, nodes.updated(h, nodes.getOrElse(h, { Trie[V]() }).add(t,v)))
      }
    }

    // find the node for a query string
    def node(k: Seq[Char]): Option[Trie[V]] = {
      k match {
        case Seq()          => Some(this)
        case Seq(h, t @ _*) => nodes.get(h).flatMap { n => n.node(t) }
      }
    }
    def node(k: String): Option[Trie[V]] = node(k.toSeq)

    // return all exact matches for the given key
    def get(k: String): List[V] = {
      node(k).toList.flatMap { (n) => n.values }
    }

    // return all values stored at and below this node
    def getAll : List[V] = {
      values ++ nodes.values.flatMap { n => n.getAll }
    }

    // for inexact queries
    def lookup[X](visitor: TrieVisitor[V,List[X]]): List[X] = {
      val here = visitor.found(values)
      val children: List[X] = nodes.toList flatMap {
        case (k,t) => visitor.pass(k) match {
          case None => Nil
          case Some(v) => t.lookup(v)
        }
      }
      here ++ children
    }
  }

  def levenshteinLookup[V](t: Lookupable[V], typed: String, maxDistance: Float): List[(Float,V)] = {

    class LevenshteinVisitor[V](val dist: IncrementalDistance = EmptyIncrementalLevenshteinBound) extends TrieVisitor[V,List[(Float,V)]] {

      def pass(k: Char): Option[LevenshteinVisitor[V]] = {
        val down = new LevenshteinVisitor[V](new IncrementalLevenshteinBound(typed, dist, k))
        if (down.dist.min > maxDistance) None else Some(down)
      }

      def found(xs: TraversableOnce[V]): List[(Float,V)] = {
        if (dist.distance > maxDistance)
          Nil
        else
          // must recheck distance -- dist only gives a bound
          (xs flatMap { v => { val d = levenshteinDistance(typed, dist.current); if (d > maxDistance) Nil else List((d,v)) }}).toList
      }
    }

    t.lookup(new LevenshteinVisitor[V])
  }
}