package ambiguity

import scala.collection.immutable.Set
import scala.collection.mutable

object Utility {
  def notImplemented = throw new NotImplementedError("not implemented")
  def impossible = throw new InternalError("impossible")

  def binarySearch[A](s: Seq[A], key: A)(implicit ord: math.Ordering[A]): Option[Int] = {
    var left: Int = 0
    var right: Int = s.length - 1
    while (right >= left) {
      val mid = left + (right - left) / 2
      val comp = ord.compare(s(mid), key)
      if (comp == 0) // s(mid) == key
        return Some(mid)
      else if (comp > 0) // s(mid) > key
        right = mid - 1
      else if (comp < 0) // s(mid) < key
        left = mid + 1
    }
    None
  }

  def toMapList[A,B](c: Iterable[(A,B)]): Map[A,List[B]] = {
    val m = mutable.Map[A,List[B]]()
    c.foreach { case (a,b) => m.update(a, b :: m.getOrElse(a,Nil)) }
    m.toMap
  }

  def addToMapList[A,B](m0: Map[A,List[B]], bs: Iterable[(A,B)]): Map[A,List[B]] = {
    val m = mutable.Map[A,List[B]]()
    m0.foreach { case (a,bs) => m.update(a, bs ::: m.getOrElse(a,Nil)) }
    bs.foreach { case (a,b) => m.update(a, b :: m.getOrElse(a,Nil)) }
    m.toMap
  }

  def toMapSet[A,B](c: Iterable[(A,B)]): Map[A,Set[B]] =
    toMapList(c) mapValues (_.toSet)

  def mergeMapSets[A,B](t: Map[A,Set[B]], t2: Map[A,Set[B]]): Map[A,Set[B]] =
    ((t.keySet ++ t2.keySet) map { s => (s, t.getOrElse(s,Set()) ++ t2.getOrElse(s,Set())) } ).toMap

  def doWhile(f: => Boolean): Unit =
    if (f) doWhile(f)

  def splitWhitespace(s: String): List[String] =
    s.split("""\s+""").toList match {
      case "" :: x => x
      case x => x
    }

  def intersperse[A](s: A, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case List(_) => xs
    case x::xs => x :: s :: intersperse(s,xs)
  }

  // Run-length encode a list
  def runs[A](xs: List[A]): List[(A,Int)] = {
    def loop(next: List[A], prev: List[(A,Int)]): List[(A,Int)] = next match {
      case Nil => prev.reverse
      case n::ns => loop(ns,prev match {
        case (p,i)::ps if p==n => (p,i+1)::ps
        case ps => (n,1)::ps
      })
    }
    loop(xs,Nil)
  }
  def unruns[A](xs: List[(A,Int)]): List[A] = xs flatMap {case (a,n) => List.fill(n)(a)}

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  // Iterate a function until referential equality fixpoint is reached
  def fixRef[A <: AnyRef](x: A)(f: A => A): A = {
    val fx = f(x)
    if (x eq fx) x else fixRef(fx)(f)
  }

  def allSome[A](xs: List[Option[A]]): Option[List[A]] = xs match {
    case Nil => Some(Nil)
    case None::_ => None
    case Some(x)::xs => allSome(xs) match {
      case None => None
      case Some(xs) => Some(x::xs)
    }
  }
  def allSome[A](xs: Set[Option[A]]): Option[Set[A]] = allSome(xs.toList) map (_.toSet)

  def collectOne[A,B](xs: List[A])(f: PartialFunction[A,B]): Option[B] = xs match {
    case Nil => None
    case x::_ if f.isDefinedAt(x) => Some(f(x))
    case _::xs => collectOne(xs)(f)
  }
  def collectOne[A,B](xs: Set[A])(f: PartialFunction[A,B]): Option[B] = collectOne(xs.toList)(f)

  // Memoize the fixpoint of a recursive function.  Usage:
  //   lazy val f = fixpoint(base, a => b) // where b refers to f
  def fixpoint[A,B](base: B, f: A => B): A => B = {
    val done = mutable.Map[A,B]()
    val next = mutable.Map[A,B]()
    val active = mutable.Set[A]()
    var changed = false
    var outer = true
    def fix(a: A): B = done.getOrElse(a, {
      def inner = next get a match {
        case None =>
          changed = true
          active += a
          next(a) = base
          val b = f(a)
          if (b != base)
            next(a) = b
          b
        case Some(b) =>
          if (active contains a)
            b
          else {
            active += a
            val c = f(a)
            if (b != c) {
              changed = true
              next(a) = c
            }
            c
          }
      }
      if (!outer) inner
      else {
        outer = false
        def loop: B = {
          val b = inner
          if (changed) {
            changed = false
            active.clear()
            loop
          } else {
            outer = true
            done ++= next
            next.clear()
            active.clear()
            b
          }
        }
        loop
      }
    })
    fix
  }
  def fixpoint[A,B,C](base: C, f: (A,B) => C): (A,B) => C = {
    lazy val g: ((A,B)) => C = fixpoint(base, x => f(x._1,x._2))
    (a,b) => g((a,b))
  }

  // Memoization.  Usage:
  //   val f = memoize(a => b)      // b doesn't depend on f
  //   lazy val f = memoize(a => b) // b depends on f, but with laziness on all cycles
  def memoize[A,B](f: A => B): A => B = {
    val cache = mutable.Map[A,B]()
    def mem(a: A): B = cache.getOrElse(a,{
      val b = f(a)
      cache(a) = b
      b
    })
    mem
  }
  def memoize[A,B,C](f: (A,B) => C): (A,B) => C = {
    val cache = mutable.Map[(A,B),C]()
    def mem(a: A, b: B): C = cache.getOrElse((a,b),{
      val c = f(a,b)
      cache((a,b)) = c
      c
    })
    mem
  }

  // Write to a file.  From http://stackoverflow.com/questions/4604237/how-to-write-to-a-file-in-scala
  def writeTo(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }

  // Create and then destroy a temporary file
  def withTemp[A](prefix: String, suffix: String, delete: Boolean = true)(f: java.io.File => A): A = {
    val file = java.io.File.createTempFile(prefix,suffix)
    try { f(file) } finally { if (delete) file.delete }
  }

  // Trait for comparison by referential equality
  trait RefEq extends AnyRef {
    override def hashCode = System.identityHashCode(this)
    override def equals(x: Any) = x match {
      case x:AnyRef => this eq x
      case _ => false
    }
  }

  def scoped[R](name: String, f: => R): R = {
    JavaUtils.pushScope(name);
    val r = f
    JavaUtils.popScope();
    r
  }
}