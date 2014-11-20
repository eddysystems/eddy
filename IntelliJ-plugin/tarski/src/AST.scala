package tarski

import Tokens.Token
import Environment.Env
import tarski.Types.PrimType

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

  sealed abstract class KList[+A](l: List[A]) { val list: List[A] = l }
  case object EmptyList extends KList(Nil)
  case class SingleList[+A](val x: A) extends KList[A](List(x))
  case class CommaList[+A](override val list: List[A]) extends KList[A](list)
  case class JuxtList[+A](override val list: List[A]) extends KList[A](list)
  case class AndList[+A](override val list: List[A]) extends KList[A](list)

  sealed abstract class Around
  case object NoAround extends Around
  case object ParenAround extends Around
  case object BrackAround extends Around
  case object CurlyAround extends Around

  sealed abstract class AType
  case class NameAType(name: Name) extends AType
  case class ModAType(mod: Mod, t: AType) extends AType
  case class ArrayAType(t: AType) extends AType
  case class ApplyAType(t: AType, a: KList[AType]) extends AType
  case class FieldAType(t: AType, f: Name) extends AType
  case class WildAType(b: Option[(Bound,AType)]) extends AType
  case class PrimAType(t: PrimType) extends AType
  case class VoidAType() extends AType

  type Block = List[AStmt]
  type ADims = Int
  type AVarDecl = (Name,ADims,Option[AExp])

  sealed abstract class AStmt
  case class EmptyAStmt() extends AStmt
  case class VarAStmt(m: List[Mod], t: AType, v: KList[AVarDecl]) extends AStmt
  case class BlockAStmt(b: Block) extends AStmt
  case class ExpAStmt(e: AExp) extends AStmt
  case class AssertAStmt(cond: AExp, msg: Option[AExp]) extends AStmt
  case class BreakAStmt(label: Option[Name]) extends AStmt
  case class ContinueAStmt(label: Option[Name]) extends AStmt
  case class ReturnAStmt(e: Option[AExp]) extends AStmt
  case class ThrowAStmt(e: AExp) extends AStmt
  case class SyncAStmt(e: AExp, s: AStmt) extends AStmt
  case class IfAStmt(cond: AExp, t: AStmt) extends AStmt
  case class IfElseAStmt(cond: AExp, t: AStmt, f: AStmt) extends AStmt
  case class WhileAStmt(cond: AExp, s: AStmt, flip: Boolean) extends AStmt
  case class DoAStmt(s: AStmt, cond: AExp, flip: Boolean) extends AStmt
  case class ForAStmt(i: List[AStmt], cond: Option[AExp], u: List[AExp], s: AStmt) extends AStmt
  case class ForeachAStmt(m: List[Mod], t: Option[AType], v: Name, n: ADims, e: AExp, s: AStmt) extends AStmt
  case class HoleAStmt() extends AStmt

  sealed abstract class AExp
  case class NameAExp(name: Name) extends AExp
  case class ParenAExp(e: AExp) extends AExp
  case class FieldAExp(e: AExp, t: Option[KList[AType]], f: Name) extends AExp
  case class MethodRefAExp(e: AExp, t: Option[KList[AType]], f: Name) extends AExp
  case class NewRefAExp(e: AExp, t: Option[KList[AType]]) extends AExp
  case class TypeApplyAExp(e: AExp, t: KList[AType]) extends AExp
  case class ApplyAExp(e: AExp, xs: KList[AExp], a: Around = ParenAround) extends AExp
  case class NewAExp(t: Option[KList[AType]], e: AExp) extends AExp
  case class WildAExp(b: Option[(Bound,AType)]) extends AExp
  case class UnaryAExp(op: UnaryOp, e: AExp) extends AExp
  case class BinaryAExp(op: BinaryOp, e0: AExp, e1: AExp) extends AExp
  case class CastAExp(t: AType, e: AExp) extends AExp
  case class CondAExp(cond: AExp, t: AExp, f: AExp) extends AExp
  case class AssignAExp(op: Option[AssignOp], left: AExp, right: AExp) extends AExp
  case class ArrayAExp(e: KList[AExp], a: Around) extends AExp
  case class InstanceofAExp(e: AExp, t: AType) extends AExp

  sealed abstract class ALit extends AExp
  case class IntALit(v: String) extends ALit
  case class LongALit(v: String) extends ALit
  case class FloatALit(v: String) extends ALit
  case class DoubleALit(v: String) extends ALit
  case class BoolALit(v: Boolean) extends ALit
  case class CharALit(v: String) extends ALit
  case class StringALit(v: String) extends ALit
  case class NullALit() extends ALit

  sealed abstract class UnaryOp
  sealed abstract class ImpOp extends UnaryOp
  case object PreDecOp extends ImpOp
  case object PreIncOp extends ImpOp
  case object PostDecOp extends ImpOp
  case object PostIncOp extends ImpOp
  case object PosOp extends UnaryOp
  case object NegOp extends UnaryOp
  case object CompOp extends UnaryOp
  case object NotOp extends UnaryOp

  sealed abstract class BinaryOp
  sealed abstract class AssignOp extends BinaryOp
  case object MulOp extends AssignOp
  case object DivOp extends AssignOp
  case object ModOp extends AssignOp
  case object AddOp extends AssignOp
  case object SubOp extends AssignOp
  case object LShiftOp extends AssignOp
  case object RShiftOp extends AssignOp
  case object UnsignedRShiftOp extends AssignOp
  case object LtOp extends BinaryOp
  case object GtOp extends BinaryOp
  case object LeOp extends BinaryOp
  case object GeOp extends BinaryOp
  case object EqOp extends BinaryOp
  case object NeOp extends BinaryOp
  case object AndOp extends AssignOp
  case object XorOp extends AssignOp
  case object OrOp extends AssignOp
  case object AndAndOp extends BinaryOp
  case object OrOrOp extends BinaryOp
}
