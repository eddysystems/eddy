package tarski

import ambiguity.Utility._
import tarski.Base._
import tarski.Items._
import tarski.Operators._
import tarski.Types._
import tarski.Scores._
import scala.annotation.tailrec

object Denotations {
  // The equivalent of Any in the denotation world.  The only uniformity is that Den's can have side effects attached.
  sealed abstract class Den extends HasDiscards {
    def strip: Den
  }
  sealed abstract class ExpOrType extends Den with HasDiscard[ExpOrType] {
    def item: TypeItem
  }

  // For use in StaticMethodDen, etc.
  case object NoneDen extends Den {
    def strip = this
    def discards = Nil
  }

  trait HasDiscards {
    def discards: List[Stmt]
  }
  trait HasDiscard[+A] extends HasDiscards {
    def discard(ds: List[Stmt]): A
  }
  case class Above[+A](discards: List[Denotations.Stmt], beneath: A) extends HasDiscard[Above[A]] {
    def strip = Above(Nil,beneath)
    def discard(ds: List[Stmt]) = Above(ds++discards,beneath)
    def map[B](f: A => B): Above[B] = Above(discards,f(beneath))
    def mapA[B](f: A => Scored[B]): Scored[Above[B]] =
      f(beneath) map (Above(discards,_))
    def mapB[B <: HasDiscard[B]](f: A => Scored[B]): Scored[B] = discards match {
      case Nil => f(beneath)
      case ds => f(beneath) map (_.discard(ds))
    }
  }
  def aboves[A](xs: List[Above[A]]): Above[List[A]] = {
    @tailrec def loop(ds: List[List[Stmt]], as: List[A], xs: List[Above[A]]): Above[List[A]] = xs match {
      case Nil => Above(ds.reverse.flatten,as.reverse)
      case Above(d,a)::xs => loop(d::ds,a::as,xs)
    }
    loop(Nil,Nil,xs)
  }
  def discardsOption[A <: HasDiscards](x: Option[A]) = x match {
    case None => Nil
    case Some(x) => x.discards
  }

  // Wrapped types
  case class TypeDen(discards: List[Stmt], beneath: Type) extends ExpOrType with HasDiscard[TypeDen] {
    def item = beneath.item
    def array = TypeDen(discards,ArrayType(beneath))

    def strip = discards match {
      case Nil => this
      case _ => TypeDen(Nil,beneath)
    }
    def discard(ds: List[Stmt]) = ds match {
      case Nil => this
      case ds => TypeDen(ds++discards,beneath)
    }
    def flatMap[A](f: Type => Scored[A]): Scored[Above[A]] = f(beneath) map (Above(discards,_))
  }

  // Callables
  sealed abstract class Callable extends Den with Signature with HasDiscard[Callable] {
    val f: CallableItem
    def tparams: List[TypeVar] = f.tparams
    def params: List[Type] = f.params
    def callItem: TypeItem
    def callType(ts: List[TypeArg]): Type
    def parent: Option[ClassType] // the parent type (for generic substitutions and in the case of NewDen, inference)
    def discard(ds: List[Stmt]) = ds match {
      case Nil => this
      case ds => DiscardCallableDen(ds,this)
    }
    def strip: Callable
  }
  sealed abstract class NonNewCallable extends Callable

  case class MethodDen(x: Exp, override val f: MethodItem) extends NonNewCallable {
    def env(ts: List[TypeArg]) = capture(tparams,ts,parent.get.env)._1
    def callItem = f.retVal.item
    def callType(ts: List[TypeArg]) = f.retVal.substitute(env(ts))
    def parent = Some(x.ty.asInstanceOf[ClassType]) // if we've constructed a MethodDen, with obj, its type must be a Class, basically
    def discards = x.discards
    def strip = MethodDen(x.strip,f)
    override def params = f.params.map( (t:Type) => t.substitute(parent.get.env) )
    // a method is called on an object, which will have a proper type at the time the call happens, so we only need to infer our own type arguments
    def alltparams = tparams
  }
  case class LocalMethodDen(override val f: MethodItem) extends NonNewCallable {
    def env(ts: List[TypeArg]) = capture(tparams,ts,Map.empty)._1 // We're local, and thus have no parent environment
    def callItem = f.retVal.item
    def callType(ts: List[TypeArg]) = f.retVal.substitute(env(ts))
    def parent = None
    def discards = Nil
    def strip = this
    def alltparams = tparams // this is raw, so we never have any relevant parent environment
  }
  case class StaticMethodDen(x: Option[Exp], override val f: MethodItem) extends NonNewCallable {
    def env(ts: List[TypeArg]) = capture(tparams,ts,Map.empty)._1 // Static methods don't use their parent environment
    def callItem = f.retVal.item
    def callType(ts: List[TypeArg]) = f.retVal.substitute(env(ts))
    def parent = None
    def discards = discardsOption(x)
    def strip = StaticMethodDen(x map (_.strip),f)
    def alltparams = tparams // static methods cannot use their parent's type environment
  }
  case class ForwardDen(parent: Option[ClassType], override val f: ConstructorItem) extends NonNewCallable {
    def callItem = VoidItem
    def callType(ts: List[TypeArg]) = VoidType
    def discards = Nil
    def strip = this
    def alltparams = tparams // either this or super -- we cannot add type parameters to those
    override def params = f.params map (_ substitute (if (parent.isDefined) parent.get.env else Map.empty))
  }
  // parent is the parent of the class being created, i.e. in "new A<X>.B<Y>.C(x)", parent is A<X>.B<Y>
  case class NewDen(parent: Option[ClassType], override val f: ConstructorItem) extends Callable {
    def callItem = f.parent
    def callType(ts: List[TypeArg]) = f.parent.generic(ts.take(f.parent.arity),parent match {
      case Some(p) => p
      case None => f.parent.parent.simple
    })
    def discards = Nil
    def strip = this
    // we can infer the type parameters of the class created, and those of the constructor used -- the class parameters go first
    // TODO: this should be recursive to allow inferring new<U> A<T>.B<S>.C<X>(S a, T b, U c, X x)
    def alltparams = f.parent.tparams ++ tparams
  }
  // Evaluate and discard s, then be f
  case class DiscardCallableDen(s: List[Stmt], c: Callable) extends Callable {
    val f = c.f
    def alltparams = c.alltparams
    def callItem = c.callItem
    def callType(ts: List[TypeArg]) = c.callType(ts)
    def parent = c.parent
    def discards = s ::: c.discards
    def strip = c.strip
  }

  type Dims = Int
  type VarDecl = (LocalVariableItem,Dims,Option[Exp]) // name,dims,init

  // Statements
  sealed abstract class Stmt extends HasDiscard[Stmt] {
    def discard(ds: List[Stmt]) = ds match {
      case Nil => this
      case ds => DiscardStmt(ds,this)
    }
    def strip: Stmt
  }
  sealed trait ForInit extends HasDiscards {
    def strip: ForInit
  }
  case object EmptyStmt extends Stmt {
    def discards = Nil
    def strip = this
  }
  case object HoleStmt extends Stmt {
    def discards = Nil
    def strip = this
  }
  case class VarStmt(t: Type, vs: List[VarDecl]) extends Stmt with ForInit {
    def discards = vs flatMap (v => discardsOption(v._3))
    def strip = VarStmt(t,vs map { case (v,n,e) => (v,n,e map (_.strip)) })
  }
  case class ExpStmt(e: StmtExp) extends Stmt {
    def discards = e.discards
    def strip = ExpStmt(e.strip)
  }
  case class BlockStmt(b: List[Stmt]) extends Stmt {
    def discards = b flatMap (_.discards)
    def strip = BlockStmt(b map (_.strip))
  }
  case class AssertStmt(c: Exp, m: Option[Exp]) extends Stmt {
    def discards = m match { case None => c.discards; case Some(m) => m.discards ::: c.discards }
    def strip = AssertStmt(c.strip,m map (_.strip))
  }
  case object BreakStmt extends Stmt { // TODO: optional label
    def discards = Nil
    def strip = this
  }
  case object ContinueStmt extends Stmt { // TODO: optional label
    def discards = Nil
    def strip = this
  }
  case class ReturnStmt(e: Option[Exp]) extends Stmt {
    def discards = e match { case None => Nil; case Some(e) => e.discards }
    def strip = ReturnStmt(e map (_.strip))
  }
  case class ThrowStmt(e: Exp) extends Stmt {
    def discards = e.discards
    def strip = ThrowStmt(e.strip)
  }
  case class IfStmt(c: Exp, t: Stmt) extends Stmt {
    def discards = c.discards
    def strip = IfStmt(c.strip,t)
  }
  case class IfElseStmt(c: Exp, t: Stmt, f: Stmt) extends Stmt {
    def discards = c.discards
    def strip = IfElseStmt(c.strip,t,f)
  }
  case class WhileStmt(c: Exp, s: Stmt) extends Stmt {
    def discards = c.discards
    def strip = WhileStmt(c.strip,s)
  }
  case class DoStmt(s: Stmt, c: Exp) extends Stmt {
    def discards = c.discards
    def strip = DoStmt(s,c.strip)
  }
  case class ForStmt(i: ForInit, c: Option[Exp], u: List[Exp], s: Stmt) extends Stmt {
    def discards = i.discards
    def strip = ForStmt(i.strip,c,u,s)
  }
  case class ForExps(i: List[Exp]) extends ForInit {
    def discards = i flatMap (_.discards)
    def strip = ForExps(i map (_.strip))
  }
  case class ForeachStmt(t: Type, v: LocalVariableItem, e: Exp, s: Stmt) extends Stmt {
    def discards = e.discards
    def strip = ForeachStmt(t,v,e.strip,s)
  }
  case class SyncStmt(e: Exp, s: Stmt) extends Stmt {
    def discards = e.discards
    def strip = SyncStmt(e.strip,s)
  }
  case class DiscardStmt(ds: List[Stmt], s: Stmt) extends Stmt {
    def discards = ds ::: s.discards
    def strip = s.strip
  }

  // It's all expressions from here
  sealed abstract class Exp extends ExpOrType with HasDiscard[Exp] {
    def ty: Type
    def item: TypeItem // Faster version of ty.item
    def discard(ds: List[Stmt]) = ds match {
      case Nil => this
      case ds => DiscardExp(ds,this)
    }
    def strip: Exp
  }
  sealed trait StmtExp extends Exp {
    def strip: StmtExp
  }
  sealed trait NoDiscardExp extends Exp {
    def discards: List[Stmt] = Nil
    def strip = this
  }

  // Literals
  sealed abstract class Lit extends Exp with NoDiscardExp
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
  case class ParameterExp(x: ParameterItem) extends Exp with NoDiscardExp {
    def item = x.item
    def ty = x.ty
  }
  case class LocalVariableExp(x: LocalVariableItem) extends Exp with NoDiscardExp {
    def item = x.item
    def ty = x.ty
  }
  case class EnumConstantExp(x: Option[Exp], c: EnumConstantItem) extends Exp {
    def item = c.item
    def ty = c.ty
    def discards = discardsOption(x)
    def strip = EnumConstantExp(x map (_.strip),c)
  }
  case class StaticFieldExp(x: Option[Exp], field: StaticFieldItem) extends Exp {
    def item = field.item
    def ty = field.ty
    def discards = discardsOption(x)
    def strip = StaticFieldExp(x map (_.strip),field)
  }
  case class LocalFieldExp(field: FieldItem) extends Exp with NoDiscardExp {
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
    def strip = FieldExp(obj.strip,field)
  }
  case class ThisExp(t: ThisItem) extends Exp with NoDiscardExp {
    def item = t.item
    def ty = t.inside
  }
  // t is the type super is used in, t.base is the type of this expression
  case class SuperExp(s: SuperItem) extends Exp with NoDiscardExp {
    def item = s.item
    def ty = s.self
  }
  case class CastExp(ty: Type, e: Exp) extends Exp {
    def item = ty.item
    def discards = e.discards
    def strip = CastExp(ty,e.strip)
  }
  sealed abstract class UnaryExp extends Exp {
    def op: UnaryOp
    def e: Exp
    def ty = unaryType(op,e.ty) getOrElse (throw new RuntimeException("type error"))
    def item = ty.item
    def discards = e.discards
  }
  case class ImpExp(op: ImpOp, e: Exp) extends UnaryExp with StmtExp {
    def strip = ImpExp(op,e.strip)
  }
  case class NonImpExp(op: NonImpOp, e: Exp) extends UnaryExp {
    def strip = NonImpExp(op,e.strip)
  }
  case class BinaryExp(op: BinaryOp, e0: Exp, e1: Exp) extends Exp {
    def ty = binaryType(op,e0.ty,e1.ty) getOrElse (throw new RuntimeException("type error"))
    def item = ty.item
    def discards = e0.discards ::: e1.discards
    def strip = BinaryExp(op,e0.strip,e1.strip)
  }
  case class AssignExp(op: Option[AssignOp], left: Exp, right: Exp) extends StmtExp {
    def item = left.item
    def ty = left.ty
    def discards = left.discards ::: right.discards
    def strip = AssignExp(op,left.strip,right.strip)
  }
  case class ParenExp(e: Exp) extends Exp {
    def item = e.item
    def ty = e.ty
    def discards = e.discards
    def strip = ParenExp(e.strip)
  }
  case class ApplyExp(f: Callable, targs: List[TypeArg], args: List[Exp]) extends StmtExp {
    def item = f.callItem
    def ty = f.callType(targs)
    def discards = f.discards ::: args flatMap (_.discards)
    def strip = ApplyExp(f.strip,targs,args map (_.strip))
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
    def strip = IndexExp(e.strip,i.strip)
  }
  case class CondExp(c: Exp, t: Exp, f: Exp, ty: Type) extends Exp {
    def item = ty.item
    def discards = c.discards ::: t.discards ::: f.discards
    def strip = CondExp(c.strip,t.strip,f.strip,ty)
  }
  case class ArrayExp(t: Type, i: List[Exp]) extends StmtExp { // t is the inner type
    def item = ArrayItem
    def ty = ArrayType(t)
    def discards = i flatMap (_.discards)
    def strip = ArrayExp(t,i map (_.strip))
  }
  case class EmptyArrayExp(t: Type, i: List[Exp]) extends StmtExp { // new t[i]
    def item = ArrayItem
    def ty = i.foldLeft(t)((t,i) => ArrayType(t))
    def discards = i flatMap (_.discards)
    def strip = EmptyArrayExp(t,i map (_.strip))
  }
  // Evaluate and discard s, then evaluate and return e
  case class DiscardExp(s: List[Stmt], e: Exp) extends Exp {
    def item = e.item
    def ty = e.ty
    def discards = s ::: e.discards
    override def strip = e.strip
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
