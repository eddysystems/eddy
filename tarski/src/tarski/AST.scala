package tarski

import tarski.Mods.Mod
import tarski.Operators.{AssignOp, BinaryOp, UnaryOp}
import tarski.Tokens.StmtTok
import utility.Locations._

object AST {
  type Name = String

  sealed abstract class Bound
  case object Extends extends Bound
  case object Super extends Bound

  sealed abstract class KList[+A] {
    def list: List[A]
    def map[B](f: A => B): KList[B]
    def size = list.size
  }
  case object EmptyList extends KList[Nothing] {
    def list = Nil
    def map[B](f: Nothing => B) = EmptyList
  }
  case class SingleList[+A](x: A) extends KList[A] {
    def list = List(x)
    def map[B](f: A => B) = SingleList(f(x))
  }
  case class CommaList[+A](list: List[A]) extends KList[A] {
    def map[B](f: A => B) = CommaList(list map f)
  }
  case class JuxtList[+A](list: List[A]) extends KList[A] {
    def map[B](f: A => B) = CommaList(list map f)
  }
  case class AndList[+A](list: List[A]) extends KList[A] {
    def map[B](f: A => B) = CommaList(list map f)
  }
  object CommaList { def apply[A](x0: A, x1: A, xs: A*): CommaList[A] = CommaList(x0::x1::xs.toList) }
  object JuxtList  { def apply[A](x0: A, x1: A, xs: A*): JuxtList[A]  =  JuxtList(x0::x1::xs.toList) }
  object AndList   { def apply[A](x0: A, x1: A, xs: A*): AndList[A]   =   AndList(x0::x1::xs.toList) }

  sealed abstract class Group
  case object Paren extends Group
  case object Brack extends Group
  case object Curly extends Group
  sealed abstract class Around
  case object NoAround extends Around
  case class Grouped(l: Group, r: Group) extends Around
  val ParenAround = Grouped(Paren,Paren)
  val BrackAround = Grouped(Brack,Brack)
  val CurlyAround = Grouped(Curly,Curly)

  type Block = List[AStmt]
  type ADims = Int
  type AVarDecl = (Name,ADims,Option[AExp])

  sealed abstract class AStmt
  sealed abstract class AStmtLoc extends AStmt with HasRange
  case object EmptyAStmt extends AStmt
  case object HoleAStmt extends AStmt
  case class VarAStmt(m: List[Mod], t: Option[AExp], v: KList[AVarDecl], r: SRange) extends AStmtLoc
  case class BlockAStmt(b: Block, r: SRange) extends AStmtLoc
  case class TokAStmt(b: StmtTok, r: SRange) extends AStmtLoc
  case class ExpAStmt(e: AExp) extends AStmtLoc { def r = e.r }
  case class AssertAStmt(cond: AExp, msg: Option[AExp], r: SRange) extends AStmtLoc
  case class BreakAStmt(label: Option[Name], r: SRange) extends AStmtLoc
  case class ContinueAStmt(label: Option[Name], r: SRange) extends AStmtLoc
  case class ReturnAStmt(e: Option[AExp], r: SRange) extends AStmtLoc
  case class ThrowAStmt(e: AExp, r: SRange) extends AStmtLoc
  case class SyncAStmt(e: AExp, s: AStmt, a: Around, r: SRange) extends AStmtLoc
  sealed trait IfsAStmt extends AStmtLoc with SetRange[IfsAStmt]
  case class IfAStmt(cond: AExp, t: AStmt, a: Around, r: SRange) extends IfsAStmt {
    def setR(r: SRange) = IfAStmt(cond,t,a,r)
  }
  case class IfElseAStmt(cond: AExp, t: AStmt, f: AStmt, a: Around, r: SRange) extends IfsAStmt {
    def setR(r: SRange) = IfElseAStmt(cond,t,f,a,r)
  }
  case class WhileAStmt(cond: AExp, s: AStmt, flip: Boolean, a: Around, r: SRange) extends AStmtLoc
  case class DoAStmt(s: AStmt, cond: AExp, flip: Boolean, a: Around, r: SRange) extends AStmtLoc
  case class ForAStmt(i: ForInfo, s: AStmt, a: Around, r: SRange) extends AStmtLoc

  sealed abstract class ForInfo extends HasRange
  case class For(i: List[AStmt], cond: Option[AExp], u: List[AExp], r: SRange) extends ForInfo
  case class Foreach(m: List[Mod], t: Option[AExp], v: Name, n: ADims, e: AExp, r: SRange) extends ForInfo

  sealed abstract class AExp extends HasRange
  case class NameAExp(name: Name, r: SRange) extends AExp
  case class ParenAExp(e: AExp, a: Grouped, r: SRange) extends AExp
  case class FieldAExp(e: AExp, t: Option[Loc[KList[AExp]]], f: Name, r: SRange) extends AExp
  case class MethodRefAExp(e: AExp, t: Option[Loc[KList[AExp]]], f: Name, r: SRange) extends AExp
  case class NewRefAExp(e: AExp, t: Option[Loc[KList[AExp]]], r: SRange) extends AExp
  case class TypeApplyAExp(e: AExp, t: KList[AExp], tr: SRange, after: Boolean, r: SRange) extends AExp // after ? A<T> : <T>A
  case class ApplyAExp(e: AExp, xs: KList[AExp], l: Around, r: SRange) extends AExp
  case class NewAExp(t: Option[Loc[KList[AExp]]], e: AExp, r: SRange) extends AExp
  case class WildAExp(b: Option[(Bound,AExp)], r: SRange) extends AExp
  case class UnaryAExp(op: UnaryOp, e: AExp, r: SRange) extends AExp
  case class BinaryAExp(op: BinaryOp, e0: AExp, e1: AExp, r: SRange) extends AExp
  case class CastAExp(t: AExp, e: AExp, r: SRange) extends AExp
  case class CondAExp(cond: AExp, t: AExp, f: AExp, r: SRange) extends AExp
  case class AssignAExp(op: Option[AssignOp], left: AExp, right: AExp, r: SRange) extends AExp
  case class ArrayAExp(e: KList[AExp], a: Around, r: SRange) extends AExp
  case class InstanceofAExp(e: AExp, t: AExp, r: SRange) extends AExp

  sealed abstract class ALit extends AExp
  case class IntALit(v: String, r: SRange) extends ALit
  case class LongALit(v: String, r: SRange) extends ALit
  case class FloatALit(v: String, r: SRange) extends ALit
  case class DoubleALit(v: String, r: SRange) extends ALit
  case class CharALit(v: String, r: SRange) extends ALit
  case class StringALit(v: String, r: SRange) extends ALit
}
