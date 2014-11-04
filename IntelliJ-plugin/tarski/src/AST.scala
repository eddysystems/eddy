package tarski

import Tokens.Token
import Environment.Env

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
  case class EmptyList() extends KList(Nil)
  case class CommaList[+A](override val list: List[A]) extends KList[A](list)
  case class JuxtList[+A](override val list: List[A]) extends KList[A](list)
  case class AndList[+A](override val list: List[A]) extends KList[A](list)

  sealed abstract class Type
  case class NameType(name: Name) extends Type
  case class ModType(mod: Mod, t: Type) extends Type
  case class ArrayType(t: Type) extends Type
  case class ApplyType(t: Type, a: KList[Type]) extends Type
  case class FieldType(t: Type, f: Name) extends Type
  case class WildType(b: Option[(Bound,Type)]) extends Type

  type NameDims = (Name,Int)

  type Block = List[Stmt]

  sealed abstract class Stmt
  case class EmptyStmt() extends Stmt
  case class VarStmt(mod: List[Mod], t: Type, v: KList[((NameDims,Option[Exp]))]) extends Stmt
  case class BlockStmt(b: Block) extends Stmt
  case class ExpStmt(e: Exp) extends Stmt
  case class AssertStmt(cond: Exp, msg: Option[Exp]) extends Stmt
  case class BreakStmt(label: Option[Name]) extends Stmt
  case class ContinueStmt(label: Option[Name]) extends Stmt
  case class ReturnStmt(e: Option[Exp]) extends Stmt
  case class ThrowStmt(e: Exp) extends Stmt
  case class SyncStmt(e: Exp, b: Block) extends Stmt

  sealed abstract class Exp
  case class NameExp(name: Name) extends Exp
  case class LitExp(l: Lit) extends Exp
  case class ParenExp(e: Exp) extends Exp
  case class FieldExp(e: Exp, t: Option[KList[Type]], f: Name) extends Exp
  case class IndexExp(e: Exp, i: KList[Exp]) extends Exp
  case class MethodRefExp(e: Exp, t: Option[KList[Type]], f: Name) extends Exp
  case class NewRefExp(e: Exp, t: Option[KList[Type]]) extends Exp
  case class TypeApplyExp(e: Exp, t: KList[Type]) extends Exp
  case class ApplyExp(e: Exp, args: KList[Exp]) extends Exp
  case class NewExp(t: Option[KList[Type]], e: Exp) extends Exp
  case class WildExp(b: Option[(Bound,Type)]) extends Exp
  case class UnaryExp(op: UnaryOp, e: Exp) extends Exp
  case class BinaryExp(op: BinaryOp, e0: Exp, e1: Exp) extends Exp
  case class CastExp(t: Type, e: Exp) extends Exp
  case class CondExp(cond: Exp, t: Exp, f: Exp) extends Exp
  case class AssignExp(op: Option[AssignOp], left: Exp, right: Exp) extends Exp
  case class ArrayExp(e: KList[Exp]) extends Exp

  sealed abstract class Lit
  case class IntLit(v: String) extends Lit
  case class LongLit(v: String) extends Lit
  case class FloatLit(v: String) extends Lit
  case class DoubleLit(v: String) extends Lit
  case class BoolLit(v: Boolean) extends Lit
  case class CharLit(v: String) extends Lit
  case class StringLit(v: String) extends Lit
  case class NullLit() extends Lit

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
