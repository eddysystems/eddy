package tarski

import Tokens.Token
import Environment.JavaEnvironment

object AST {

  def parse(tokens: java.util.List[Token], env: JavaEnvironment) = println("scala: " + tokens)

  type Name = String

  sealed abstract class Node

  sealed abstract class Mod extends Node
  case class Annotation(name: Name) extends Mod
  case class Public() extends Mod
  case class Protected() extends Mod
  case class Private() extends Mod
  case class Static() extends Mod
  case class Final() extends Mod
  case class Strictfp() extends Mod
  case class Transient() extends Mod
  case class Volatile() extends Mod
  case class Synchronized() extends Mod

  sealed abstract class Bound extends Node
  case class Extends() extends Bound
  case class Super() extends Bound

  sealed abstract class KList[+A](l: List[A]) extends Node { val list: List[A] = l }
  case class CommaList[+A <: Node](override val list: List[A]) extends KList[A](list)
  case class JuxtList[+A <: Node](override val list: List[A]) extends KList[A](list)
  case class AndList[+A <: Node](override val list: List[A]) extends KList[A](list)

  sealed abstract class Type extends Node
  case class NameType(name: Name) extends Type
  case class ModType(mod: Mod, t: Type) extends Type
  case class ArrayType(t: Type) extends Type
  case class ApplyType(t: Type, a: KList[Type]) extends Type
  case class FieldType(t: Type, f: Name) extends Type
  case class WildType(b: Option[(Bound,Type)]) extends Type

  type NameDims = (Name,Int)

  type Block = List[Stmt]

  sealed abstract class Stmt extends Node
  case class EmptyStmt() extends Stmt
  case class VarStmt(mod: Mod, t: Type, v: KList[((NameDims,Option[Exp]))]) extends Stmt
  case class BlockStmt(b: Block) extends Stmt
  case class ExpStmt(e: Exp) extends Stmt
  case class AssertStmt(cond: Exp, msg: Option[Exp]) extends Stmt
  case class BreakStmt(label: Option[Name]) extends Stmt
  case class ContinueStmt(label: Option[Name]) extends Stmt
  case class ReturnStmt(e: Option[Exp]) extends Stmt
  case class ThrowStmt(e: Exp) extends Stmt
  case class SyncStmt(e: Exp, b: Block) extends Stmt

  sealed abstract class Exp extends Node
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
  case class BinaryExp(e1: Exp, op: BinaryOp, e2: Exp) extends Exp
  case class CastExp(t: Type, e: Exp) extends Exp
  case class CondExp(cond: Exp, t: Exp, f: Exp) extends Exp
  case class AssignExp(left: Exp, op: Option[AssignOp], right: Exp) extends Exp

  sealed abstract class Lit extends Node
  case class IntLit(v: Long) extends Lit
  case class FloatLit(v: Double) extends Lit
  case class BooleanLit(v: Boolean) extends Lit
  case class CharLit(v: Char) extends Lit
  case class StringLit(v: String) extends Lit

  sealed abstract class UnaryOp extends Node
  sealed abstract class ImpOp extends UnaryOp
  case class PreDec() extends ImpOp
  case class PreInc() extends ImpOp
  case class PostDec() extends ImpOp
  case class PostInc() extends ImpOp
  case class PosOp() extends UnaryOp
  case class NegOp() extends UnaryOp
  case class ComplementOp() extends UnaryOp
  case class NotOp() extends UnaryOp

  sealed abstract class BinaryOp extends Node
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

  def children(n: Node): List[Node] = n match {

    case _: Mod => List()
    case _: Bound => List()
    case _: UnaryOp => List()
    case _: BinaryOp => List()
    case _: Lit => List()

    // Type
    case NameType(_) => List()
    case ModType(m, t) => List(m, t)
    case ArrayType(t) => List(t)
    case ApplyType(t, l) => List(t) ++ l.list
    case FieldType(t,f) => List(t)
    case WildType(b) => if (b.isEmpty) Nil else List(b.get._1, b.get._2)

    // Statements
    case EmptyStmt() => List()
    case VarStmt(m, t, v) => List(m, t) ++ v.list.flatMap( _._2.toList )
    case BlockStmt(b) => b
    case ExpStmt(e) => List(e)
    case AssertStmt(cond, msg) => cond :: msg.toList
    case BreakStmt(_) => List()
    case ContinueStmt(_) => List()
    case ReturnStmt(e) => e.toList
    case ThrowStmt(e) => List(e)
    case SyncStmt(e, b) => e :: b

    // Expressions
    case NameExp(_) => List()
    case LitExp(_) => List()
    case ParenExp(e) => List(e)
    case FieldExp(e,t,f) => e :: t.toList.flatMap( _.list )
    case IndexExp(e, i) => i :: i.list
    case MethodRefExp(e, t, f) => e :: t.toList.flatMap( _.list )
    case NewRefExp(e, t) => e :: t.toList.flatMap( _.list )
    case TypeApplyExp(e, t) => e :: t.list
    case ApplyExp(e, a) => e :: a.list
    case NewExp(t, e) => t.toList.flatMap( _.list) :+ e
    case WildExp(b) => if (b.isEmpty) Nil else List(b.get._1, b.get._2)
    case UnaryExp(op, e) => List(op,e)
    case BinaryExp(e1, op, e2) => List(e1, op, e2)
    case CastExp(t, e) => List(t, e)
    case CondExp(c, t, f) => List(c,t,f)
    case AssignExp(left, op, right) => (left :: op.toList) :+ right
  }
}
