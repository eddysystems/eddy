package tarski

import ambiguity.Utility._
import ambiguity.Locations._
import tarski.Scores._
import tarski.Tokens._
import scala.annotation.tailrec
import scala.math._

// Repair mismatches parentheses in token streams
object Mismatch {
  sealed abstract class Part { def t: Token }
  sealed abstract class Group extends Part
  case class Left(t: Token) extends Group
  case class Right(t: Token) extends Group
  case class Other(t: Token) extends Part

  def part(t: Located[Token]): Located[Part] = t map {
    case t@(LParenTok|LBrackTok|LCurlyTok) => Left(t)
    case t@(RParenTok|RBrackTok|RCurlyTok) => Right(t)
    case t => Other(t)
  }
  type Segments = List[(Located[Part],List[Located[Part]])]

  def matched(ks: Segments): Boolean = {
    @tailrec
    def loop(ks: Segments, d: Int): Boolean = ks match {
      case Nil => d == 0
      case (Located(_:Other,_),_)::r => loop(r,d)
      case (Located(_:Left ,_),k)::r => loop(r,d+k.size)
      case (Located(_:Right,_),k)::r => val kn = k.size
                                        d >= kn && loop(r,d-kn)
    }
    loop(ks,0)
  }

  // Penalty turning from parens into to parens
  def pr(from: Int, to: Int): Prob = Prob(s"change parens $from -> $to",
    if (from == to) 1
    else if (from == 0) 1.0 / to // Adding parentheses on the ends is more likely
    else .5 / (from+abs(from-to))
  )

  // Ensure that ps starts and ends with all kinds of parentheses
  def ensure(ps: Segments): Segments = {
    def add(t: Located[Part], ps: Segments): Segments = {
      def startsWith(ps: Segments): Boolean = ps match {
        case (p,_)::r => t.x==p.x || startsWith(r)
        case _ => false
      }
      if (startsWith(ps)) ps else (t,Nil)::ps
    }
    val rL = ps.head._1.r
    val rR = ps.last._1.r
    def L(t: Token, r: Segments) = add(Located(Left(t) ,rL),r)
    def R(t: Token, r: Segments) = add(Located(Right(t),rR),r)
    L(LCurlyTok,L(LParenTok,L(LBrackTok,
    R(RCurlyTok,R(RParenTok,R(RBrackTok,ps.reverse))).reverse)))
  }

  // Coerce a segment of one length into another, keeping track of as much location information as possible
  def tweak(p: Located[Part], ps: List[Located[Part]], n: Int): List[Located[Part]] = ps match {
    case Nil => List.fill(n)(p)
    case _ =>
      val m = ps.size
      if (n < m) ps.take(n/2) ::: ps.drop(m-(n+1)/2)
      else ps splitAt (m/2) match {
        case (s0,s1@(Located(_,r)::_)) =>
          val q = Located(p.x,r)
          s0 ::: List.fill(n-m)(q) ::: s1
        case (_,Nil) => impossible
      }
  }

  // Make at most n mutations to a kind stream
  def mutate(rs: Segments, n: Int): Scored[Segments] =
    if (n == 0) known(rs)
    else rs match {
      case Nil => known(Nil)
      case (o@(Located(_:Other,_),_))::rs => mutate(rs,n) map (o::_)
      case (k,is)::rs =>
        val i = is.size
        val j = listGood(for (j <- (max(0,i-n) to (i+n)).toList) yield Alt(pr(i,j),j))
        j flatMap (j => mutate(rs,n-abs(i-j)) map ((k,tweak(k,is,j))::_))
    }

  def repair(ts: List[Located[Token]]): Scored[List[Located[Token]]] = {
    // FIXME: breaks if ensure gets empty list
    val rs = ensure(segmentBy(ts map part)(_.x==_.x) map { case ps => (ps.head,ps) })
    if (matched(rs)) known(ts)
    else {
      // Mutate at most 2 times
      mutate(rs,2).filter(matched,"Mismatched parentheses") map (s => s.map(_._2).flatten map (_ map (_.t)))
    }
  }
}
