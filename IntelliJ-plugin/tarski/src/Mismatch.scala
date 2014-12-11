package tarski

import tarski.Scores._
import tarski.Tokens._
import ambiguity.Utility._
import scala.annotation.tailrec
import scala.math._

// Repair mismatches parentheses in token streams
object Mismatch {
  sealed abstract class Part { def t: Token }
  sealed abstract class Group extends Part
  case class Left(t: Token) extends Group
  case class Right(t: Token) extends Group
  case class Other(t: Token) extends Part

  def part(t: Token): Part = t match {
    case LParenTok()|LBrackTok()|LCurlyTok() => Left(t)
    case RParenTok()|RBrackTok()|RCurlyTok() => Right(t)
    case _ => Other(t)
  }
  type Runs = List[(Part,Int)]

  def matched(ks: Runs): Boolean = {
    @tailrec
    def loop(ks: Runs, d: Int): Boolean = ks match {
      case Nil => d == 0
      case (Other(_),_)::r => loop(r,d)
      case (Left (_),k)::r => loop(r,d+k)
      case (Right(_),k)::r => d >= k && loop(r,d-k)
    }
    loop(ks,0)
  }

  // Penalty turning from parens into to parens
  def pr(from: Int, to: Int): Prob =
    if (from == to) 1
    else if (from == 0) 1.0 / to // Adding parentheses on the ends is more likely
    else .5 / (from+abs(from-to))

  // Ensure that ps starts and ends with all kinds of parentheses
  def ensure(ps: Runs): Runs = {
    def add(t: Part, ps: Runs) = {
      def startsWith(ps: Runs): Boolean = ps match {
        case (p,_)::r => t==p || startsWith(r)
        case _ => false
      }
      if (startsWith(ps)) ps else (t,0)::ps
    }
    def L(t: () => Token, r: Runs) = add(Left(t()),r)
    def R(t: () => Token, r: Runs) = add(Right(t()),r)
    L(LCurlyTok,L(LParenTok,L(LBrackTok,
    R(RCurlyTok,R(RParenTok,R(RBrackTok,ps.reverse))).reverse)))
  }

  // Make at most n mutations to a kind stream
  def mutate(rs: List[(Part,Int)], n: Int): Scored[List[(Part,Int)]] =
    if (n == 0) known(rs)
    else rs match {
      case Nil => known(Nil)
      case (o@(Other(_),_))::rs => mutate(rs,n) map (o::_)
      case (k,i)::rs => {
        val j = multipleGood(for (j <- (max(0,i-n) to (i+n)).toList) yield Alt(pr(i,j),j))
        j flatMap (j => mutate(rs,n-abs(i-j)) map ((k,j)::_))
      }
    }

  def repair(ts: List[Token]): Scored[List[Token]] = {
    val rs = ensure(runs(ts map part))
    if (matched(rs)) known(ts)
    else {
      // Mutate at most 2 times
      mutate(rs,2).filter(matched,"Mismatched parentheses") map (unruns(_) map (_.t))
    }
  }
}
