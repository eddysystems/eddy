package tarski

import ambiguity.Utility
import ambiguity.Utility.binarySearch
import StringMatching.{IncrementalLevenshteinBound, EmptyIncrementalLevenshteinBound, IncrementalDistance, levenshteinDistance}

import scala.annotation.tailrec
import scala.collection.mutable
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

  class CompactTrie[V](_values: Iterable[V])(key: V => String, _initial_size: Int = 0, vsize: Int = _values.size)(implicit tt: ClassTag[V]) extends Lookupable[V] {

    private val initial_size_multiplier = 10
    private val grow_size_multiplier = 1.2 // grow by 20 percent when running out of space
    private val initial_size = if (_initial_size == 0) initial_size_multiplier * vsize else _initial_size

    def ensureSpace(size: Int): Unit = if (size > structure.size) {
      val newsize = (size * grow_size_multiplier).ceil.toInt
      println("increasing trie memory size to " + newsize * 4)
      structure = structure.padTo(newsize, 0)
    }

    println("allocating " + 4 * initial_size + " bytes.")
    private var structure = new Array[Int](initial_size)
    private var structure_pos = 0

    val values = new Array[V](vsize)
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
      assert(nodevalues.forall( i => key(i) == key(nodevalues.head)))
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
      parts.keys.toList.sorted.foreach {
        case k@Some(c) => {
          // - fill in correct child character and start index
          structure(entryidx) = c.toInt
          structure(entryidx+1) = structure_pos
          entryidx += 2

          // - call makeNode for c's subtree
          makeNode(parts.get(k).get, depth+1)
        }
        case None => Unit
      }
    }

    private def copyNode(t: CompactTrie[V], node: Int): Unit = {
      // transscribe the subtree to this trie to match our structure/value positions

      // copy values and write range to structure
      val vs = t.nodevalues(node)
      structure(structure_pos) = values_pos
      structure(structure_pos+1) = values_pos + vs.size
      vs.copyToArray(values,values_pos)
      values_pos += vs.size

      // write size and advance structure_pos
      val cs = t.children(node)
      val nodestart = structure_pos
      structure(nodestart+2) = cs.size
      assert(structure_pos + 3 + 2 * cs.size <= structure.length)
      structure_pos += 3 + 2 * cs.size

      // write children
      var entryidx = nodestart + 3
      cs.foreach { case (c,idx) =>
        structure(entryidx) = c.toInt
        structure(entryidx+1) = structure_pos
        entryidx += 2
        copyNode(t,idx)
      }
    }

    private def makeMergedNode(t1: CompactTrie[V], n1: Int, t2: CompactTrie[V], n2: Int): Unit = {

      val nodestart = structure_pos

      // write out values of both
      val n1values = t1.nodevalues(n1)
      val n2values = t2.nodevalues(n2)

      structure(nodestart) = values_pos
      structure(nodestart+1) = values_pos + n1values.size + n2values.size
      assert(values_pos + n1values.size + n2values.size <= values.length)
      n1values.copyToArray(values,values_pos)
      values_pos += n1values.size
      n2values.copyToArray(values,values_pos)
      values_pos += n2values.size

      // merge children
      val cs = new mutable.TreeSet[(Char,Option[Int],Option[Int])]()(math.Ordering.by(_._1))
      val cs2 = t2.children(n2)
      t1.children(n1).foreach { case (c,idx) =>
        binarySearch(cs2, (c,0))(Ordering.by(_._1)) match {
          case None => cs += ((c,Some(idx),None))
          case Some(ci) => cs += ((c,Some(idx),Some(cs2(ci)._2)))
        }
      }
      cs2.foreach { case (c,idx2) =>
        if (!cs.contains((c,None,None)))
          cs += ((c,None,Some(idx2)))
      }

      // advance structure_pos
      structure(nodestart+2) = cs.size
      assert(nodestart + 3 + 2 * cs.size <= structure.length)
      structure_pos += 3 + 2 * cs.size

      // for each child
      var entryidx = nodestart + 3
      cs.foreach { case (c,i1,i2) =>
        // - fill in correct child character and start index
        structure(entryidx) = c.toInt
        structure(entryidx+1) = structure_pos
        entryidx += 2
        (i1,i2) match {
          // - if either didn't have it, copy the subtree from the Trie who had it
          case (Some(idx1),None) => copyNode(t1,idx1)
          case (None,Some(idx2)) => copyNode(t2,idx2)
          // - if both had this child, call makeMergedNode to make the child node
          case (Some(idx1),Some(idx2)) => makeMergedNode(t1,idx1,t2,idx2)
          case (None,None) => Utility.impossible
        }
      }
    }

    def mergeWith(t: CompactTrie[V]): CompactTrie[V] = {
      // we'll need at most the sum of both our structure sizes
      val newt = new CompactTrie[V](Nil)(key, structure_pos + t.structure_pos, values_pos + t.values_pos)(tt)

      println(s"merging $structure_pos + ${t.structure_pos} -> ${newt.structure.size}")

      // blank
      newt.structure_pos = 0
      newt.values_pos = 0

      // merge the two
      newt.makeMergedNode(this, 0, t, 0)

      newt
    }

    private def nodevalues(nodeidx: Int) = {
      val vs = values.view(structure(nodeidx), structure(nodeidx+1))
      assert(vs.forall( key(_) == key(vs.head) ))
      vs
    }

    private def children(nodeidx: Int): IndexedSeq[(Char,Int)] = {

      class ChildList(nodeidx: Int) extends IndexedSeq[(Char,Int)] {
        def length: Int = structure(nodeidx+2)
        def apply(idx: Int): (Char, Int) = {
          val childidx = nodeidx + 3 + 2 * idx
          (structure(childidx).toChar, structure(childidx+1))
        }
      }

      new ChildList(nodeidx)
    }

    private def findChild(nodeidx: Int, c: Char): Option[Int] = {
      val cs = children(nodeidx)
      binarySearch(cs, (c,0))(Ordering.by[(Char,Int),Char](_._1)) map (cs(_)._2)
    }

    def add(vs: Iterable[V]): CompactTrie[V] =
      mergeWith(new CompactTrie(vs)(key))

    private def exact(idx: String, depth: Int, nodeidx: Int): List[V] = {
      idx.lift(depth) match {
        case None => nodevalues(nodeidx).toList // we're done here, return values
        case Some(c) => // search our children for c,
          findChild(nodeidx,c) match {
            case None => Nil // not found
            case Some(i) => exact(idx, depth+1, i)
          }
      }
    }
    def exact(idx: String): List[V] = exact(idx,0,0)

    private def lookup[X](visitor: TrieVisitor[V,List[X]], nodeidx: Int): List[X] = {
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