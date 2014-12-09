package tarski

import tarski.Operators._
import tarski.Items._
import tarski.Types._
import tarski.Base._
import ambiguity.Utility._

object Denotations {
  // Callables
  sealed abstract class Callable extends Signature {
    val f: CallableItem
    def callItem: TypeItem
    def callType(ts: List[RefType]): Type
  }
  sealed abstract class NonNewCallable extends Callable {
    def tparams: List[TypeVar] = f.tparams
    def params: List[Type] = f.params
  }
  case class MethodDen(obj: Exp, override val f: MethodItem) extends NonNewCallable {
    def callItem = f.retVal.item
    def callType(ts: List[RefType]) = substitute(f.tparams,ts,f.retVal)
  }
  case class LocalMethodDen(override val f: MethodItem) extends NonNewCallable {
    def callItem = f.retVal.item
    def callType(ts: List[RefType]) = substitute(f.tparams,ts,f.retVal)
  }
  case class StaticMethodDen(obj: Option[Exp], override val f: MethodItem) extends NonNewCallable {
    def callItem = f.retVal.item
    def callType(ts: List[RefType]) = substitute(f.tparams,ts,f.retVal)
  }
  case class ForwardDen(override val f: ConstructorItem) extends NonNewCallable {
    def callItem = VoidItem
    def callType(ts: List[RefType]) = VoidType
  }
  case class NewDen(override val f: ConstructorItem) extends Callable {
    def tparams = f.parent.tparams ++ f.tparams // TODO: May need to recursively include higher parent parameters
    def params = f.params
    def callItem = f.parent
    def callType(ts: List[RefType]) = f.parent.generic(ts.take(f.parent.arity),f.parent.parent.simple)
  }

  type Dims = Int
  type VarDecl = (LocalVariableItem,Dims,Option[Exp]) // name,dims,init

  // Statements
  sealed abstract class Stmt
  sealed trait ForInit
  case object EmptyStmt extends Stmt
  case object HoleStmt extends Stmt
  case class VarStmt(t: Type, vs: List[VarDecl]) extends Stmt with ForInit
  case class ExpStmt(e: StmtExp) extends Stmt
  case class BlockStmt(b: List[Stmt]) extends Stmt
  case class AssertStmt(c: Exp, m: Option[Exp]) extends Stmt
  case object BreakStmt extends Stmt    // TODO: optional label
  case object ContinueStmt extends Stmt // TODO: optional label
  case class ReturnStmt(e: Option[Exp]) extends Stmt
  case class ThrowStmt(e: Exp) extends Stmt
  case class IfStmt(c: Exp, t: Stmt) extends Stmt
  case class IfElseStmt(c: Exp, t: Stmt, f: Stmt) extends Stmt
  case class WhileStmt(c: Exp, s: Stmt) extends Stmt
  case class DoStmt(s: Stmt, c: Exp) extends Stmt
  case class ForStmt(i: ForInit, c: Option[Exp], u: List[Exp], s: Stmt) extends Stmt
  case class ForExps(i: List[Exp]) extends ForInit
  case class ForeachStmt(t: Type, v: LocalVariableItem, e: Exp, s: Stmt) extends Stmt
  case class SyncStmt(e: Exp, s: Stmt) extends Stmt

  // It's all expressions from here
  sealed abstract class Exp {
    def ty: Type
    def item: TypeItem // Faster version of ty.item
  }
  sealed trait StmtExp extends Exp

  // Literals
  sealed abstract class Lit extends Exp
  case class ByteLit(b: Byte, text: String) extends Lit     { def ty = ByteType;    def item = ubByteItem }
  case class ShortLit(s: Short, text: String) extends Lit   { def ty = ShortType;   def item = ubShortItem }
  case class IntLit(i: Int, text: String) extends Lit       { def ty = IntType;     def item = ubIntItem }
  case class LongLit(l: Long, text: String) extends Lit     { def ty = LongType;    def item = ubLongItem }
  case class BooleanLit(b: Boolean) extends Lit             { def ty = BooleanType; def item = ubBooleanItem }
  case class StringLit(s: String, text: String) extends Lit { def ty = StringType;  def item = StringItem }
  case class FloatLit(f: Float, text: String) extends Lit   { def ty = FloatType;   def item = ubFloatItem }
  case class DoubleLit(d: Double, text: String) extends Lit { def ty = DoubleType;  def item = ubDoubleItem }
  case class CharLit(c: Char, text: String) extends Lit     { def ty = CharType;    def item = ubCharItem }
  case object NullLit extends Lit                           { def ty = NullType;    def item = NullType.item }

  // Expressions
  case class ParameterExp(x: ParameterItem) extends Exp {
    def item = x.item
    def ty = x.ty
  }
  case class LocalVariableExp(x: LocalVariableItem) extends Exp {
    def item = x.item
    def ty = x.ty
  }
  case class EnumConstantExp(obj: Option[Exp], c: EnumConstantItem) extends Exp {
    def item = c.item
    def ty = c.ty
  }
  case class StaticFieldExp(obj: Option[Exp], field: StaticFieldItem) extends Exp {
    def item = field.item
    def ty = field.ty
  }
  case class LocalFieldExp(field: FieldItem) extends Exp {
    def item = field.item
    def ty = field.inside
  }
  case class FieldExp(obj: Exp, field: FieldItem) extends Exp {
    def item = field.item
    def ty = {
      val t = obj.ty
      val fp = field.parent
      collectOne(supers(t)){
        case t:ClassType if t.item==fp => field.inside.substitute(t.env)
      }.getOrElse(throw new RuntimeException(s"Field $field not found in $t"))
    }
  }
  case class ThisExp(t: ThisItem) extends Exp {
    def item = t.item
    def ty = t.inside
  }
  case class SuperExp(t: ThisItem) extends Exp { // t is the type super is used in, t.base is the type of this expression
    def item = t.self.base.item
    def ty = t.self.base
  }
  case class CastExp(ty: Type, e: Exp) extends Exp {
    def item = ty.item
  }
  sealed abstract class UnaryExp extends Exp {
    def op: UnaryOp
    def e: Exp
    def ty = unaryType(op,e.ty) getOrElse (throw new RuntimeException("type error"))
    def item = ty.item
  }
  case class ImpExp(op: ImpOp, e: Exp) extends UnaryExp with StmtExp
  case class NonImpExp(op: NonImpOp, e: Exp) extends UnaryExp
  case class BinaryExp(op: BinaryOp, e0: Exp, e1: Exp) extends Exp {
    def ty = binaryType(op,e0.ty,e1.ty) getOrElse (throw new RuntimeException("type error"))
    def item = ty.item
  }
  case class AssignExp(op: Option[AssignOp], left: Exp, right: Exp) extends StmtExp {
    def item = left.item
    def ty = left.ty
  }
  case class ParenExp(e: Exp) extends Exp {
    def item = e.item
    def ty = e.ty
  }
  case class ApplyExp(f: Callable, targs: List[RefType], args: List[Exp]) extends StmtExp {
    def item = f.callItem
    def ty = f.callType(targs)
  }
  case class IndexExp(e: Exp, i: Exp) extends Exp {
    def item = e.ty match {
      case ArrayType(t) => t.item
      case _ => throw new RuntimeException("type error")
    }
    def ty = e.ty match {
      case ArrayType(t) => t
      case _ => throw new RuntimeException("type error")
    }
  }
  case class CondExp(c: Exp, t: Exp, f: Exp, ty: Type) extends Exp {
    def item = ty.item
  }
  case class ArrayExp(t: Type, i: List[Exp]) extends StmtExp { // t is the inner type
    def item = ArrayItem
    def ty = ArrayType(t)
  }
  case class EmptyArrayExp(t: Type, i: List[Exp]) extends StmtExp { // new t[i]
    def item = ArrayItem
    def ty = i.foldLeft(t)((t,i) => ArrayType(t))
  }

  def typeOf(e: Option[Exp]): Type = e match {
    case None => VoidType
    case Some(e) => e.ty
  }

  // Is an expression definitely side effect free?
  def noEffects(e: Exp): Boolean = e match {
    case _:Lit|_:ParameterExp|_:LocalVariableExp|_:EnumConstantExp|_:StaticFieldExp|_:LocalFieldExp
        |_:ThisExp|_:SuperExp => true
    case _:CastExp|_:AssignExp|_:ApplyExp|_:IndexExp|_:ArrayExp|_:EmptyArrayExp|_:ImpExp => false
    case FieldExp(x,f) => noEffects(x)
    case NonImpExp(op,x) => pure(op) && noEffects(x)
    case BinaryExp(op,x,y) => pure(op,x.ty,y.ty) && noEffects(x) && noEffects(y)
    case ParenExp(x) => noEffects(x)
    case CondExp(c,x,y,_) => noEffects(c) && noEffects(x) && noEffects(y)
  }
  def pure(op: UnaryOp) = !op.isInstanceOf[ImpOp]
  def pure(op: BinaryOp, x: Type, y: Type) = (x,y) match {
    case (x:PrimType,y:PrimType) => op match {
      case DivOp|ModOp => x.isInstanceOf[FloatingType] || y.isInstanceOf[FloatingType]
      case MulOp|AddOp|SubOp|LShiftOp|RShiftOp|UnsignedRShiftOp
          |AndAndOp|OrOrOp|AndOp|XorOp|OrOp|LtOp|GtOp|LeOp|GeOp|EqOp|NeOp => true
    }
    case _ => false // If we unbox, there can always be null
  }

  // Extract effects.  TODO: This discards certain exceptions, such as for casts, null errors, etc.
  def effects(e: Exp): List[Stmt] = e match {
    case e:StmtExp => List(ExpStmt(e))
    case _:Lit|_:ParameterExp|_:LocalVariableExp|_:EnumConstantExp|_:StaticFieldExp|_:LocalFieldExp
        |_:ThisExp|_:SuperExp => Nil
    case _:CastExp|_:IndexExp => Nil
    case FieldExp(x,_) => effects(x)
    case NonImpExp(_,x) => effects(x)
    case BinaryExp(_,x,y) => effects(x)++effects(y)
    case CondExp(c,x,y,_) => List(IfElseStmt(c,blocked(effects(x)),blocked(effects(y))))
    case ParenExp(x) => effects(x)
  }

  def blocked(ss: List[Stmt]): Stmt = ss match {
    case Nil => EmptyStmt
    case List(s) => s
    case ss => BlockStmt(ss)
  }
}
