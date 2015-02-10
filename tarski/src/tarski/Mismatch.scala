package tarski

import utility.Utility._
import utility.Locations._
import tarski.Scores._
import tarski.JavaScores._
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

  def part(t: Loc[Token]): Loc[Part] = t map {
    case t@(LParenTok|LBrackTok|LCurlyTok) => Left(t)
    case t@(RParenTok|RBrackTok|RCurlyTok|RightAnyTok) => Right(t)
    case t => Other(t)
  }
  type Segments = List[(Loc[Part],List[Loc[Part]])]

  def matched(ks: Segments): Boolean = {
    @tailrec
    def loop(ks: Segments, d: Int): Boolean = ks match {
      case Nil => d == 0
      case (Loc(_:Other,_),_)::r => loop(r,d)
      case (Loc(_:Left ,_),k)::r => loop(r,d+k.size)
      case (Loc(_:Right,_),k)::r => val kn = k.size
                                        d >= kn && loop(r,d-kn)
    }
    loop(ks,0)
  }

  // Penalty turning from parens into to parens
  def pr(from: Int, to: Int): Prob = Prob(s"change parens $from -> $to",
    if (from == to) 1
    else if (from == 0) 1.0 / (1+to)
    else from.toDouble / (from+abs(from-to))
  )

  // Ensure that ps starts and ends with all kinds of parentheses
  def ensure(ps: Segments): Segments = {
    def add(t: Loc[Part], ps: Segments): Segments = {
      def startsWith(ps: Segments): Boolean = ps match {
        case (p,_)::r => t.x==p.x || startsWith(r)
        case _ => false
      }
      if (startsWith(ps)) ps else (t,Nil)::ps
    }
    val rL = ps.head._1.r
    val rR = ps.last._1.r
    def L(t: Token, r: Segments) = add(Loc(Left(t) ,rL),r)
    def R(t: Token, r: Segments) = add(Loc(Right(t),rR),r)
    L(LCurlyTok,L(LParenTok,L(LBrackTok,R(RightAnyTok,ps.reverse).reverse)))
  }

  // Coerce a segment of one length into another, keeping track of as much location information as possible
  def tweak(p: Loc[Part], ps: List[Loc[Part]], n: Int): List[Loc[Part]] = ps match {
    case Nil => List.fill(n)(p)
    case _ =>
      val m = ps.size
      if (n < m) ps.take(n/2) ::: ps.drop(m-(n+1)/2)
      else ps splitAt (m/2) match {
        case (s0,s1@(Loc(_,r)::_)) =>
          val q = Loc(p.x,r)
          s0 ::: List.fill(n-m)(q) ::: s1
        case (_,Nil) => impossible
      }
  }

  // Make at most n mutations to a kind stream
  def mutate(rs: Segments, n: Int): Scored[Segments] =
    if (n == 0) known(rs)
    else rs match {
      case Nil => known(Nil)
      case (o@(Loc(_:Other,_),_))::rs => mutate(rs,n) map (o::_)
      case (k,is)::rs =>
        val i = is.size
        val j = listGood(for (j <- (max(0,i-n) to (i+n)).toList) yield Alt(pr(i,j),j))
        j flatMap (j => mutate(rs,n-abs(i-j)) map ((k,tweak(k,is,j))::_))
    }

  def kindErrors(ts: List[Loc[Token]]): Int = {
    @tailrec def loop(ts: List[Token], stack: List[Token], errors: Int): Int = (ts,stack) match {
      case (Nil,Nil) => errors
      case ((t@(LParenTok|LBrackTok|LCurlyTok))::ts,s) => loop(ts,t::s,errors)
      case (RightAnyTok::ts,_::s) => loop(ts,s,errors)
      case (RParenTok::ts,LParenTok::s) => loop(ts,s,errors)
      case (RBrackTok::ts,LBrackTok::s) => loop(ts,s,errors)
      case (RCurlyTok::ts,LCurlyTok::s) => loop(ts,s,errors)
      case ((RParenTok|RBrackTok|RCurlyTok)::ts,_::s) => loop(ts,s,errors+1)
      case (_::ts,s) => loop(ts,s,errors)
      case (Nil,_) => impossible
    }
    loop(ts map (_.x),Nil,0)
  }

  def repair(ts: List[Loc[Token]]): Scored[List[Loc[Token]]] = if (ts.isEmpty) known(ts) else {
    val rs = ensure(segmentBy(ts map part)(_.x==_.x) map { case ps => (ps.head,ps) })
    if (matched(rs)) known(ts)
    else {
      // Mutate at most 2 times
      val ts0 = mutate(rs,2).filter(matched,"Mismatched parentheses") map (s => s.map(_._2).flatten map (_ map (_.t)))
      val ts1 = ts0 flatMap (ts => single(ts,Prob("kind errors",pow(.5,kindErrors(ts)))))
      if (false) {
        println("repaired to:")
        implicit val f = fullShowFlags
        for (Alt(p,ts) <- ts1.stream.toList)
          println(s"  $p : ${ts map (_.x.show) mkString " "}")
      }
      ts1
    }
  }
}
