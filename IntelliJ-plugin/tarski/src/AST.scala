package tarski

import Tokens.Token
import Environment.Env
import tarski.Types.PrimType
import tarski.Operators._

object AST {

  def parse(tokens: java.util.List[Token], env: Env) = println("scala: " + tokens)

  type Name = String

  sealed abstract class Mod
  case class Annotation(name: Name) extends Mod
  case object Abstract extends Mod
  case object Public extends Mod
  case object Protected extends Mod
  case object Private extends Mod
  case object Static extends Mod
  case object Final extends Mod
  case object Strictfp extends Mod
  case object Transient extends Mod
  case object Volatile extends Mod
  case object Synchronized extends Mod

  sealed abstract class Bound
  case class Extends() extends Bound
  case class Super() extends Bound

  sealed abstract class KList[+A] {
    def list: List[A]
    def map[B](f: A => B): KList[B]
  }
  case object EmptyList extends KList {
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

  sealed abstract class Group
  case object Paren extends Group
  case object Brack extends Group
  case object Curly extends Group
  sealed abstract class Around
  case object NoAround extends Around
  case class Grouped(l: Group, r: Group) extends Around
  val ParenAround = Grouped(Paren,Paren)
  val BrackAround = Grouped(Brack,Brack)

  type Block = List[AStmt]
  type ADims = Int
  type AVarDecl = (Name,ADims,Option[AExp])

  sealed abstract class AStmt
  case class EmptyAStmt() extends AStmt
  case class VarAStmt(m: List[Mod], t: AExp, v: KList[AVarDecl]) extends AStmt
  case class BlockAStmt(b: Block) extends AStmt
  case class ExpAStmt(e: AExp) extends AStmt
  case class AssertAStmt(cond: AExp, msg: Option[AExp]) extends AStmt
  case class BreakAStmt(label: Option[Name]) extends AStmt
  case class ContinueAStmt(label: Option[Name]) extends AStmt
  case class ReturnAStmt(e: Option[AExp]) extends AStmt
  case class ThrowAStmt(e: AExp) extends AStmt
  case class SyncAStmt(e: AExp, s: AStmt, a: Around) extends AStmt
  case class IfAStmt(cond: AExp, t: AStmt, a: Around) extends AStmt
  case class IfElseAStmt(cond: AExp, t: AStmt, f: AStmt, a: Around) extends AStmt
  case class WhileAStmt(cond: AExp, s: AStmt, flip: Boolean, a: Around) extends AStmt
  case class DoAStmt(s: AStmt, cond: AExp, flip: Boolean, a: Around) extends AStmt
  case class ForAStmt(i: ForInfo, s: AStmt, a: Around) extends AStmt
  case class HoleAStmt() extends AStmt

  sealed abstract class ForInfo
  case class For(i: List[AStmt], cond: Option[AExp], u: List[AExp]) extends ForInfo
  case class Foreach(m: List[Mod], t: Option[AExp], v: Name, n: ADims, e: AExp) extends ForInfo

  sealed abstract class AExp
  case class NameAExp(name: Name) extends AExp
  case class ParenAExp(e: AExp, a: Grouped) extends AExp
  case class FieldAExp(e: AExp, t: Option[KList[AExp]], f: Name) extends AExp
  case class MethodRefAExp(e: AExp, t: Option[KList[AExp]], f: Name) extends AExp
  case class NewRefAExp(e: AExp, t: Option[KList[AExp]]) extends AExp
  case class TypeApplyAExp(e: AExp, t: KList[AExp]) extends AExp
  case class ApplyAExp(e: AExp, xs: KList[AExp], l: Around = ParenAround) extends AExp
  case class NewAExp(t: Option[KList[AExp]], e: AExp) extends AExp
  case class WildAExp(b: Option[(Bound,AExp)]) extends AExp
  case class UnaryAExp(op: UnaryOp, e: AExp) extends AExp
  case class BinaryAExp(op: BinaryOp, e0: AExp, e1: AExp) extends AExp
  case class CastAExp(t: AExp, e: AExp) extends AExp
  case class CondAExp(cond: AExp, t: AExp, f: AExp) extends AExp
  case class AssignAExp(op: Option[AssignOp], left: AExp, right: AExp) extends AExp
  case class ArrayAExp(e: KList[AExp], a: Around) extends AExp
  case class InstanceofAExp(e: AExp, t: AExp) extends AExp

  sealed abstract class ALit extends AExp
  case class IntALit(v: String) extends ALit
  case class LongALit(v: String) extends ALit
  case class FloatALit(v: String) extends ALit
  case class DoubleALit(v: String) extends ALit
  case class BoolALit(v: Boolean) extends ALit
  case class CharALit(v: String) extends ALit
  case class StringALit(v: String) extends ALit
  case class NullALit() extends ALit
}
