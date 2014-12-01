package ambiguity

import scala.language.higherKinds

object Products {

  // Collection classes with Cartesian products
  trait HasProduct[+A, M[+X] <: HasProduct[X,M]] {
    def map[B](f: A => B): M[B]
    def flatMap[B](f: A => M[B]): M[B]
    def productWith[B,C](x: M[B])(f: (A,B) => C): M[C]
    def product[B](x: M[B]): M[(A,B)] = productWith(x)((_,_))
  }

  // The type of singleton generators (for Option and List products)
  abstract class HasOne[+M[+_]] {
    def one[A](x: A): M[A]
  }

  // Fixed size products
  def product[M[+X]<:HasProduct[X,M],A,B](a: M[A], b: M[B]): M[(A,B)] =
    a product b

  def productWith[M[+X]<:HasProduct[X,M],A,B,T](a: M[A], b: M[B])(f: (A,B) => T): M[T] =
    a.productWith(b)(f)

  def product[M[+X]<:HasProduct[X,M],A,B,C](a: M[A], b: M[B], c: M[C]): M[(A,B,C)] =
    (a product b).productWith(c)((ab,c) => (ab._1,ab._2,c))

  def productWith[M[+X]<:HasProduct[X,M],A,B,C,T](a: M[A], b: M[B], c: M[C])(f: (A,B,C) => T): M[T] =
    (a product b).productWith(c)((ab,c) => f(ab._1,ab._2,c))

  def product[M[+X]<:HasProduct[X,M],A,B,C,D](a: M[A], b: M[B], c: M[C], d: M[D]): M[(A,B,C,D)] =
    (a product b).productWith(c product d)((ab,cd) => (ab._1,ab._2,cd._1,cd._2))

  def productWith[M[+X]<:HasProduct[X,M],A,B,C,D,T](a: M[A], b: M[B], c: M[C], d: M[D])(f: (A,B,C,D) => T): M[T] =
    (a product b).productWith(c product d)((ab,cd) => f(ab._1,ab._2,cd._1,cd._2))

  // Option products
  def product[M[+X]<:HasProduct[X,M],A](xs: Option[M[A]])(implicit o: HasOne[M]): M[Option[A]] = xs match {
    case None => o one None
    case Some(x) => x map (Some(_))
  }
  def product[M[+X]<:HasProduct[X,M],A](xs: List[M[A]])(implicit o: HasOne[M]): M[List[A]] = xs match {
    case Nil => o one Nil
    case sx :: sxs => sx.productWith(product(sxs))(_::_)
  }

  def productFoldLeft[M[+X]<:HasProduct[X,M],A,E](e: E)(fs: List[E => M[(E,A)]])(implicit o: HasOne[M]): M[(E,List[A])] =
    fs match {
      case Nil => o one ((e,Nil))
      case f :: fs => f(e) flatMap {case (ex,x) => productFoldLeft(ex)(fs) map {case (exs,xs) => (exs,x::xs)}}
    }

  // thread is map followed by product
  def thread[M[+X]<:HasProduct[X,M],A,B](xs: Option[A])(f: A => M[B])(implicit o: HasOne[M]): M[Option[B]] = product(xs map f)
  def thread[M[+X]<:HasProduct[X,M],A,B](xs: List[A])  (f: A => M[B])(implicit o: HasOne[M]): M[List[B]]   = product(xs map f)
}