package tarski

import tarski.StringMatching.{IncrementalLevenshteinBound, EmptyIncrementalLevenshteinBound, IncrementalDistance, levenshteinDistance}

import scala.collection.immutable.HashMap

object Trie {

  trait TrieVisitor[V,X] {
    // you see k, want to continue descending? Return none if no, a new visitor that has absorbed the k if yes.
    def pass(k: Char): Option[TrieVisitor[V,X]]
    // we found these values, filter and transform to output type
    def found(t: List[V]): X
  }

  case class Trie[V](values: List[V] = Nil, nodes: Map[Char, Trie[V]] = Map[Char,Trie[V]]()) {

    // create as a shallow copy of another Trie (only used because of constructor restrictions
    def this(t: Trie[V]) = {
      this(t.values, t.nodes)
    }

    // create from a list of things
    def this(xs: List[(String,V)]) = {
      this(Trie[V]().add(xs))
    }

    // return a new trie containing everything in here and the values in xs
    def add(xs: List[(String,V)]): Trie[V] = xs match {
      case Nil => this
      case (s,v)::xs => add(s.toSeq,v).add(xs)
    }

    // return a new Trie with s,v added
    def add(s: Seq[Char], v: V): Trie[V] = {
      s match {
        case Seq() => new Trie[V](values :+ v, nodes)
        case Seq(h,t @ _*) => new Trie[V](values, nodes.updated(h, nodes.getOrElse(h, { new Trie[V]() }).add(t,v)))
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

  def levenshteinLookup[V](t: Trie[V], typed: String, maxDistance: Float): List[(Float,V)] = {

    class LevenshteinVisitor[V](val dist: IncrementalDistance = EmptyIncrementalLevenshteinBound) extends TrieVisitor[V,List[(Float,V)]] {

      def pass(k: Char): Option[LevenshteinVisitor[V]] = {
        val down = new LevenshteinVisitor[V](new IncrementalLevenshteinBound(typed, dist, k))
        if (down.dist.min > maxDistance) None else Some(down)
      }

      def found(xs: List[V]): List[(Float,V)] = {
        if (dist.distance > maxDistance)
          Nil
        else
          // must recheck distance -- dist only gives a bound
          xs flatMap { v => { val d = levenshteinDistance(typed, dist.current); if (d > maxDistance) Nil else List((d,v)) }}
      }
    }

    t.lookup(new LevenshteinVisitor[V])
  }
}