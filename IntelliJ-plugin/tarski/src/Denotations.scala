package tarski

import tarski.Operators._
import tarski.Items._
import tarski.Types._
import tarski.Base._
import ambiguity.Utility._
import scala.language.implicitConversions

object Denotations {
  // Support for keeping track of side effects
  trait HasDiscard[B] {
    def discard(ds: List[Stmt]): B
  }
  trait HasDiscards[+A] {
    def discards: List[Stmt]
    def stripDiscards: A
  }
  def discardsOption[A <: HasDiscards[A]](x: Option[A]) = x match {
    case None => Nil
    case Some(x) => x.discards
  }

  // Callables
  sealed abstract class Callable extends Signature with HasDiscard[Callable] with HasDiscards[Callable] {
    val f: CallableItem
    def callItem: TypeItem
    def callType(ts: List[RefType]): Type
    def discard(ds: List[Stmt]) = DiscardCallableDen(ds,this)
  }
  sealed abstract class NonNewCallable extends Callable {
    def tparams: List[TypeVar] = f.tparams
    def params: List[Type] = f.params
  }
  case class MethodDen(obj: Exp, override val f: MethodItem) extends NonNewCallable {
    def callItem = f.retVal.item
    def callType(ts: List[RefType]) = substitute(f.tparams,ts,f.retVal)
    def discards = obj.discards
    def stripDiscards = MethodDen(obj.stripDiscards,f)
  }
  case class LocalMethodDen(override val f: MethodItem) extends NonNewCallable {
    def callItem = f.retVal.item
    def callType(ts: List[RefType]) = substitute(f.tparams,ts,f.retVal)
    def discards = Nil
    def stripDiscards = this
  }
  case class StaticMethodDen(obj: Option[Exp], override val f: MethodItem) extends NonNewCallable {
    def callItem = f.retVal.item
    def callType(ts: List[RefType]) = substitute(f.tparams,ts,f.retVal)
    def discards = discardsOption(obj)
    def stripDiscards = StaticMethodDen(obj map (_.stripDiscards),f)
  }
  case class ForwardDen(override val f: ConstructorItem) extends NonNewCallable {
    def callItem = VoidItem
    def callType(ts: List[RefType]) = VoidType
    def discards = Nil
    def stripDiscards = this
  }
  case class NewDen(override val f: ConstructorItem) extends Callable {
    def tparams = f.parent.tparams ++ f.tparams // TODO: May need to recursively include higher parent parameters
    def params = f.params
    def callItem = f.parent
    def callType(ts: List[RefType]) = f.parent.generic(ts.take(f.parent.arity),f.parent.parent.simple)
    def discards = Nil
    def stripDiscards = this
  }
  // Evaluate and discard s, then be f
  case class DiscardCallableDen(s: List[Stmt], c: Callable) extends Callable {
    val f = c.f
    def tparams = c.tparams
    def params = c.params
    def callItem = c.callItem
    def callType(ts: List[RefType]) = c.callType(ts)
    def discards = s ::: c.discards
    def stripDiscards = c.stripDiscards
  }

  type Dims = Int
  type VarDecl = (LocalVariableItem,Dims,Option[Exp]) // name,dims,init

  // Statements
  sealed abstract class Stmt extends HasDiscard[Stmt] with HasDiscards[Stmt] {
    def discard(ds: List[Stmt]) = ds match {
      case Nil => this
      case ds => DiscardStmt(ds,this)
    }
  }
  sealed trait ForInit extends HasDiscards[ForInit]
  case object EmptyStmt extends Stmt {
    def discards = Nil
    def stripDiscards = this
  }
  case object HoleStmt extends Stmt {
    def discards = Nil
    def stripDiscards = this
  }
  case class VarStmt(t: Type, vs: List[VarDecl]) extends Stmt with ForInit with HasDiscards[VarStmt] {
    def discards = vs flatMap (v => discardsOption(v._3))
    def stripDiscards = VarStmt(t,vs map { case (v,n,e) => (v,n,e map (_.stripDiscards)) })
  }
  case class ExpStmt(e: StmtExp) extends Stmt {
    def discards = e.discards
    def stripDiscards = ExpStmt(e.stripDiscards)
  }
  case class BlockStmt(b: List[Stmt]) extends Stmt {
    def discards = b flatMap (_.discards)
    def stripDiscards = BlockStmt(b map (_.stripDiscards))
  }
  case class AssertStmt(c: Exp, m: Option[Exp]) extends Stmt {
    def discards = m match { case None => c.discards; case Some(m) => m.discards ::: c.discards }
    def stripDiscards = AssertStmt(c.stripDiscards,m map (_.stripDiscards))
  }
  case object BreakStmt extends Stmt { // TODO: optional label
    def discards = Nil
    def stripDiscards = this
  }
  case object ContinueStmt extends Stmt { // TODO: optional label
    def discards = Nil
    def stripDiscards = this
  }
  case class ReturnStmt(e: Option[Exp]) extends Stmt {
    def discards = e match { case None => Nil; case Some(e) => e.discards }
    def stripDiscards = ReturnStmt(e map (_.stripDiscards))
  }
  case class ThrowStmt(e: Exp) extends Stmt {
    def discards = e.discards
    def stripDiscards = ThrowStmt(e.stripDiscards)
  }
  case class IfStmt(c: Exp, t: Stmt) extends Stmt {
    def discards = c.discards
    def stripDiscards = IfStmt(c.stripDiscards,t)
  }
  case class IfElseStmt(c: Exp, t: Stmt, f: Stmt) extends Stmt {
    def discards = c.discards
    def stripDiscards = IfElseStmt(c.stripDiscards,t,f)
  }
  case class WhileStmt(c: Exp, s: Stmt) extends Stmt {
    def discards = c.discards
    def stripDiscards = WhileStmt(c.stripDiscards,s)
  }
  case class DoStmt(s: Stmt, c: Exp) extends Stmt {
    def discards = c.discards
    def stripDiscards = DoStmt(s,c.stripDiscards)
  }
  case class ForStmt(i: ForInit, c: Option[Exp], u: List[Exp], s: Stmt) extends Stmt {
    def discards = i.discards
    def stripDiscards = ForStmt(i.stripDiscards,c,u,s)
  }
  case class ForExps(i: List[Exp]) extends ForInit {
    def discards = i flatMap (_.discards)
    def stripDiscards = ForExps(i map (_.stripDiscards))
  }
  case class ForeachStmt(t: Type, v: LocalVariableItem, e: Exp, s: Stmt) extends Stmt {
    def discards = e.discards
    def stripDiscards = ForeachStmt(t,v,e.stripDiscards,s)
  }
  case class SyncStmt(e: Exp, s: Stmt) extends Stmt {
    def discards = e.discards
    def stripDiscards = SyncStmt(e.stripDiscards,s)
  }
  case class DiscardStmt(ds: List[Stmt], s: Stmt) extends Stmt {
    def discards = ds ::: s.discards
    def stripDiscards = s.stripDiscards
  }

  // It's all expressions from here
  sealed abstract class Exp extends HasDiscard[Exp] with HasDiscards[Exp] {
    def ty: Type
    def item: TypeItem // Faster version of ty.item
    def discard(ds: List[Stmt]) = DiscardExp(ds,this)
  }
  sealed trait NoDiscard extends Exp {
    def discards = Nil
    def stripDiscards = this
  }
  sealed trait StmtExp extends Exp with HasDiscards[StmtExp]

  // Literals
  sealed abstract class Lit extends Exp with NoDiscard
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
  case class ParameterExp(x: ParameterItem) extends Exp with NoDiscard {
    def item = x.item
    def ty = x.ty
  }
  case class LocalVariableExp(x: LocalVariableItem) extends Exp with NoDiscard {
    def item = x.item
    def ty = x.ty
  }
  case class EnumConstantExp(obj: Option[Exp], c: EnumConstantItem) extends Exp {
    def item = c.item
    def ty = c.ty
    def discards = discardsOption(obj)
    def stripDiscards = EnumConstantExp(obj map (_.stripDiscards),c)
  }
  case class StaticFieldExp(obj: Option[Exp], field: StaticFieldItem) extends Exp {
    def item = field.item
    def ty = field.ty
    def discards = discardsOption(obj)
    def stripDiscards = StaticFieldExp(obj map (_.stripDiscards),field)
  }
  case class LocalFieldExp(field: FieldItem) extends Exp with NoDiscard {
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
    def discards = obj.discards
    def stripDiscards = FieldExp(obj.stripDiscards,field)
  }
  case class ThisExp(t: ThisItem) extends Exp with NoDiscard {
    def item = t.item
    def ty = t.inside
  }
  // t is the type super is used in, t.base is the type of this expression
  case class SuperExp(t: ThisItem) extends Exp with NoDiscard {
    def item = t.self.base.item
    def ty = t.self.base
  }
  case class CastExp(ty: Type, e: Exp) extends Exp {
    def item = ty.item
    def discards = e.discards
    def stripDiscards = CastExp(ty,e.stripDiscards)
  }
  sealed abstract class UnaryExp extends Exp {
    def op: UnaryOp
    def e: Exp
    def ty = unaryType(op,e.ty) getOrElse (throw new RuntimeException("type error"))
    def item = ty.item
    def discards = e.discards
  }
  case class ImpExp(op: ImpOp, e: Exp) extends UnaryExp with StmtExp {
    def stripDiscards = ImpExp(op,e.stripDiscards)
  }
  case class NonImpExp(op: NonImpOp, e: Exp) extends UnaryExp {
    def stripDiscards = NonImpExp(op,e.stripDiscards)
  }
  case class BinaryExp(op: BinaryOp, e0: Exp, e1: Exp) extends Exp {
    def ty = binaryType(op,e0.ty,e1.ty) getOrElse (throw new RuntimeException("type error"))
    def item = ty.item
    def discards = e0.discards ::: e1.discards
    def stripDiscards = BinaryExp(op,e0.stripDiscards,e1.stripDiscards)
  }
  case class AssignExp(op: Option[AssignOp], left: Exp, right: Exp) extends StmtExp {
    def item = left.item
    def ty = left.ty
    def discards = left.discards ::: right.discards
    def stripDiscards = AssignExp(op,left.stripDiscards,right.stripDiscards)
  }
  case class ParenExp(e: Exp) extends Exp {
    def item = e.item
    def ty = e.ty
    def discards = e.discards
    def stripDiscards = ParenExp(e.stripDiscards)
  }
  case class ApplyExp(f: Callable, targs: List[RefType], args: List[Exp]) extends StmtExp {
    def item = f.callItem
    def ty = f.callType(targs)
    def discards = f.discards ::: args flatMap (_.discards)
    def stripDiscards = ApplyExp(f.stripDiscards,targs,args map (_.stripDiscards))
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
    def discards = e.discards ::: i.discards
    def stripDiscards = IndexExp(e.stripDiscards,i.stripDiscards)
  }
  case class CondExp(c: Exp, t: Exp, f: Exp, ty: Type) extends Exp {
    def item = ty.item
    def discards = c.discards ::: t.discards ::: f.discards
    def stripDiscards = CondExp(c.stripDiscards,t.stripDiscards,f.stripDiscards,ty)
  }
  case class ArrayExp(t: Type, i: List[Exp]) extends StmtExp { // t is the inner type
    def item = ArrayItem
    def ty = ArrayType(t)
    def discards = i flatMap (_.discards)
    def stripDiscards = ArrayExp(t,i map (_.stripDiscards))
  }
  case class EmptyArrayExp(t: Type, i: List[Exp]) extends StmtExp { // new t[i]
    def item = ArrayItem
    def ty = i.foldLeft(t)((t,i) => ArrayType(t))
    def discards = i flatMap (_.discards)
    def stripDiscards = EmptyArrayExp(t,i map (_.stripDiscards))
  }
  // Evaluate and discard s, then evaluate and return e
  case class DiscardExp(s: List[Stmt], e: Exp) extends Exp {
    def item = e.item
    def ty = e.ty
    def discards = s ::: e.discards
    def stripDiscards = e.stripDiscards
  }

  def typeOf(e: Option[Exp]): Type = e match {
    case None => VoidType
    case Some(e) => e.ty
  }

  // Is an expression definitely side effect free?
  def noEffects(e: Exp): Boolean = e match {
    case _:Lit|_:ParameterExp|_:LocalVariableExp|_:EnumConstantExp|_:StaticFieldExp|_:LocalFieldExp
        |_:ThisExp|_:SuperExp => true
    case _:CastExp|_:AssignExp|_:ApplyExp|_:IndexExp|_:ArrayExp|_:EmptyArrayExp|_:ImpExp|_:DiscardExp => false
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
    case DiscardExp(ds,e) => ds++effects(e)
  }

  def blocked(ss: List[Stmt]): Stmt = ss match {
    case Nil => EmptyStmt
    case List(s) => s
    case ss => BlockStmt(ss)
  }
}
