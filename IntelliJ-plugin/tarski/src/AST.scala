package tarski

import Tokens.Token
import Environment.Env
import tarski.Types.PrimType

object AST {

  def parse(tokens: java.util.List[Token], env: Env) = println("scala: " + tokens)

  type Name = String

  sealed abstract class Mod
  case class Annotation(name: Name) extends Mod
  case class Abstract() extends Mod
  case class Public() extends Mod
  case class Protected() extends Mod
  case class Private() extends Mod
  case class Static() extends Mod
  case class Final() extends Mod
  case class Strictfp() extends Mod
  case class Transient() extends Mod
  case class Volatile() extends Mod
  case class Synchronized() extends Mod

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
  case class VarAStmt(mod: List[Mod], t: AType, v: KList[AVarDecl]) extends AStmt
  case class BlockAStmt(b: Block) extends AStmt
  case class ExpAStmt(e: AExp) extends AStmt
  case class AssertAStmt(cond: AExp, msg: Option[AExp]) extends AStmt
  case class BreakAStmt(label: Option[Name]) extends AStmt
  case class ContinueAStmt(label: Option[Name]) extends AStmt
  case class ReturnAStmt(e: Option[AExp]) extends AStmt
  case class ThrowAStmt(e: AExp) extends AStmt
  case class SyncAStmt(e: AExp, b: Block) extends AStmt

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
  case class PreDecOp() extends ImpOp
  case class PreIncOp() extends ImpOp
  case class PostDecOp() extends ImpOp
  case class PostIncOp() extends ImpOp
  case class PosOp() extends UnaryOp
  case class NegOp() extends UnaryOp
  case class CompOp() extends UnaryOp
  case class NotOp() extends UnaryOp

  sealed abstract class BinaryOp
  sealed abstract class AssignOp extends BinaryOp
  case class MulOp() extends AssignOp
  case class DivOp() extends AssignOp
  case class ModOp() extends AssignOp
  case class AddOp() extends AssignOp
  case class SubOp() extends AssignOp
  case class LShiftOp() extends AssignOp
  case class RShiftOp() extends AssignOp
  case class UnsignedRShiftOp() extends AssignOp
  case class LtOp() extends BinaryOp
  case class GtOp() extends BinaryOp
  case class LeOp() extends BinaryOp
  case class GeOp() extends BinaryOp
  case class InstanceofOp() extends BinaryOp
  case class EqOp() extends BinaryOp
  case class NeOp() extends BinaryOp
  case class AndOp() extends AssignOp
  case class XorOp() extends AssignOp
  case class OrOp() extends AssignOp
  case class AndAndOp() extends BinaryOp
  case class OrOrOp() extends BinaryOp
}
