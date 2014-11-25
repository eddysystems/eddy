package tarski

import tarski.Operators._
import tarski.Items._
import tarski.Types._
import ambiguity.Utility._

object Denotations {
  // Callables
  sealed abstract class Callable extends Signature {
    val f: CallableItem
  }
  sealed abstract class NonNewCallable extends Callable {
    def tparams: List[TypeVar] = f.tparams
    def params: List[Type] = f.params
  }
  case class MethodDen(obj: Exp, override val f: MethodItem) extends NonNewCallable
  case class LocalMethodDen(override val f: MethodItem) extends NonNewCallable
  case class StaticMethodDen(obj: Option[Exp], override val f: StaticMethodItem) extends NonNewCallable
  case class ForwardDen(override val f: ConstructorItem) extends NonNewCallable
  case class NewDen(override val f: ConstructorItem) extends Callable {
    def tparams = f.parent.tparams ++ f.tparams // TODO: May need to recursively include higher parent parameters
    def params = f.params
  }

  type Dims = Int
  type VarDecl = (LocalVariableItem,Dims,Option[Exp]) // name,dims,init

  // Statements
  sealed abstract class Stmt
  sealed trait ForInit
  case object EmptyStmt extends Stmt
  case object HoleStmt extends Stmt
  case class VarStmt(t: Type, vs: List[VarDecl]) extends Stmt with ForInit
  case class ExpStmt(e: Exp) extends Stmt
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
  sealed abstract class Exp

  // Literals
  sealed abstract class Lit extends Exp
  case class ByteLit(b: Byte, text: String) extends Lit
  case class ShortLit(s: Short, text: String) extends Lit
  case class IntLit(i: Int, text: String) extends Lit
  case class LongLit(l: Long, text: String) extends Lit
  case class BooleanLit(b: Boolean) extends Lit
  case class StringLit(s: String, text: String) extends Lit
  case class FloatLit(f: Float, text: String) extends Lit
  case class DoubleLit(d: Double, text: String) extends Lit
  case class CharLit(c: Char, text: String) extends Lit
  case object NullLit extends Lit

  // Expressions
  case class ParameterExp(item: ParameterItem) extends Exp
  case class LocalVariableExp(item: LocalVariableItem) extends Exp
  case class EnumConstantExp(obj: Option[Exp], item: EnumConstantItem) extends Exp
  case class StaticFieldExp(obj: Option[Exp], field: StaticFieldItem) extends Exp
  case class LocalFieldExp(field: FieldItem) extends Exp
  case class FieldExp(obj: Exp, field: FieldItem) extends Exp
  case class ThisExp(t: ThisItem) extends Exp
  case class SuperExp(t: ThisItem) extends Exp // t is the type super is used in, t.base is the type of this expression
  case class CastExp(t: Type, e: Exp) extends Exp
  case class UnaryExp(op: UnaryOp, e: Exp) extends Exp
  case class BinaryExp(op: BinaryOp, e0: Exp, e1: Exp) extends Exp
  case class AssignExp(op: Option[AssignOp], left: Exp, right: Exp) extends Exp
  case class ParenExp(e: Exp) extends Exp
  case class ApplyExp(f: Callable, targs: List[RefType], args: List[Exp]) extends Exp
  case class IndexExp(e: Exp, i: Exp) extends Exp
  case class CondExp(c: Exp, t: Exp, f: Exp, r: Type) extends Exp
  case class ArrayExp(t: Type, i: List[Exp]) extends Exp // t is the inner type
  case class EmptyArrayExp(t: Type, i: List[Exp]) extends Exp // new t[i]

  def typeOf(d: Exp): Type = d match {
    // Literals
    case ByteLit(_,_) => ByteType
    case ShortLit(_,_) => ShortType
    case IntLit(_,_) => IntType
    case LongLit(_,_) => LongType
    case BooleanLit(_) => BooleanType
    case StringLit(_,_) => StringType
    case FloatLit(_,_) => FloatType
    case DoubleLit(_,_) => DoubleType
    case CharLit(_,_) => CharType
    case NullLit => NullType
    // Names
    case ParameterExp(i) => i.ty
    case LocalVariableExp(i) => i.ty
    case EnumConstantExp(_,i) => i.ty
    case ThisExp(t) => t.inside
    case SuperExp(ThisItem(c:NormalClassItem)) => c.base
    case SuperExp(_) => throw new RuntimeException("type error")
    case CastExp(t,_) => t
    case UnaryExp(op,e) => unaryType(op,typeOf(e)) getOrElse (throw new RuntimeException("type error"))
    case BinaryExp(op,x,y) => binaryType(op,typeOf(x),typeOf(y)) getOrElse (throw new RuntimeException("type error"))
    case AssignExp(op,left,right) => typeOf(left)
    case ParenExp(e) => typeOf(e)
    case ApplyExp(f,ts,_) => f match {
      case MethodDen(_,f)       => substitute(f.tparams,ts,f.retVal)
      case LocalMethodDen(f)    => substitute(f.tparams,ts,f.retVal)
      case StaticMethodDen(_,f) => substitute(f.tparams,ts,f.retVal)
      case NewDen(c) => c.parent.generic(ts.take(c.parent.arity),c.parent.parent.simple)
      case ForwardDen(_) => VoidType
    }
    case FieldExp(x,f) => {
      val t = typeOf(x)
      val fp = f.parent
      collectOne(supers(t)){
        case t:ClassType if t.item==fp => f.inside.substitute(t.env)
      }.getOrElse(throw new RuntimeException(s"Field $f not found in $t"))
    }
    case LocalFieldExp(f) => f.inside
    case StaticFieldExp(_,f) => f.ty
    case IndexExp(e,i) => typeOf(e) match {
      case ArrayType(t) => t
      case _ => throw new RuntimeException("type error")
    }
    case CondExp(_,_,_,r) => r
    // Arrays
    case ArrayExp(t,_) => ArrayType(t)
    case EmptyArrayExp(t,is) => is.foldLeft(t)((t,i) => ArrayType(t))
  }

  def typeOf(e: Option[Exp]): Type = e match {
    case None => VoidType
    case Some(e) => typeOf(e)
  }

  // Is an expression definitely side effect free?
  def noEffects(e: Exp): Boolean = e match {
    case _:Lit|_:ParameterExp|_:LocalVariableExp|_:EnumConstantExp|_:StaticFieldExp|_:LocalFieldExp
        |_:ThisExp|_:SuperExp => true
    case _:CastExp|_:AssignExp|_:ApplyExp|_:IndexExp|_:ArrayExp|_:EmptyArrayExp => false
    case FieldExp(x,f) => noEffects(x)
    case UnaryExp(op,x) => pure(op) && noEffects(x)
    case BinaryExp(op,x,y) => pure(op,typeOf(x),typeOf(y)) && noEffects(x) && noEffects(y)
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
}
