package tarski

import utility.Utility._
import tarski.Base._
import tarski.Items._
import tarski.Operators._
import tarski.Types._
import tarski.Scores._
import scala.annotation.tailrec
import scala.language.implicitConversions

object Denotations {
  // The equivalent of Any in the denotation world
  sealed trait Den
  sealed trait ParentDen extends Den
  sealed trait ExpOrType extends ParentDen {
    def item: TypeItem
  }
  sealed trait TypeOrCallable extends Den with HasDiscard[TypeOrCallable]
  sealed trait TypeOrPackage extends Den
  sealed trait ExpOrCallable extends Den with HasDiscard[ExpOrCallable]

  trait HasDiscards {
    def discards: List[Stmt]
  }
  trait HasDiscard[+A] extends HasDiscards {
    def discard(ds: List[Stmt]): A
  }
  implicit class DiscardsOption[A <: HasDiscards](val x: Option[A]) extends AnyVal {
    def discards = x match {
      case None => Nil
      case Some(x) => x.discards
    }
  }

  case class Above[+A](discards: List[Denotations.Stmt], beneath: A)
  def aboves[A](xs: List[Above[A]]): Above[List[A]] = {
    @tailrec def loop(ds: List[Stmt], as: List[A], xs: List[Above[A]]): Above[List[A]] = xs match {
      case Nil => Above(ds.reverse,as.reverse)
      case Above(d,a)::xs => loop(revAppend(d,ds),a::as,xs)
    }
    loop(Nil,Nil,xs)
  }

  // Wrapped packages.  Items.Package inherits from this.
  trait PackageDen extends ParentDen with TypeOrPackage {
    def p: Package
    def strip = this
    def discards = Nil
  }

  // Wrapped types
  case class TypeDen(discards: List[Stmt], beneath: Type) extends ExpOrType with TypeOrCallable with TypeOrPackage with HasDiscard[TypeDen] {
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
  sealed abstract class Callable extends ExpOrCallable with TypeOrCallable with Signature with HasDiscard[Callable] {
    def tparams: List[TypeVar]
    def params: List[Type]
    def callItem: TypeItem
    def callType(ts: List[TypeArg]): Type
    def discard(ds: List[Stmt]): Callable
    def strip: Callable
  }
  sealed abstract class NotTypeApply extends Callable {
    def strip: NotTypeApply
    def discard(ds: List[Stmt]): NotTypeApply = ds match {
      case Nil => this
      case ds => DiscardCallableDen(ds,this)
    }
  }
  case class TypeApply(c: NotTypeApply, ts: List[TypeArg], hide: Boolean) extends Callable {
    def tparams = Nil
    lazy val params = {
      // TODO: Map.empty may be wrong here
      implicit val env = capture(c.tparams,ts,Map.empty)._1
      c.params map (_.substitute)
    }
    lazy val result = c.callType(ts)
    def callItem = c.callItem
    def callType(ts2: List[TypeArg]) = ts2 match {
      case Nil => result
      case _ => throw new RuntimeException("TypeApply already has type arguments")
    }
    def strip = TypeApply(c.strip,ts,hide)
    def discards = c.discards
    def discard(ds: List[Stmt]) = ds match {
      case Nil => this
      case ds => TypeApply(c.discard(ds),ts,hide)
    }
  }
  case class MethodDen(x: Option[Exp], f: MethodItem) extends NotTypeApply {
    private lazy val parentEnv: Tenv = x match {
      case _ if f.isStatic => Map.empty
      case None => Map.empty
      case Some(x) => x.ty.asInstanceOf[ClassType].env // x must be a class for MethodDen to make sense
    }
    def tparams = f.tparams
    lazy val params = f.params.map(_.substitute(parentEnv))
    lazy val result = f.retVal.substitute(parentEnv)
    def callItem = f.retVal.item
    def callType(ts: List[TypeArg]) = f.retVal.substitute(capture(tparams,ts,parentEnv)._1)
    def discards = x.discards
    def strip = MethodDen(x map (_.strip),f)
  }
  case class LocalMethodDen(f: MethodItem) extends NotTypeApply {
    def tparams = f.tparams
    def params = f.params
    def result = f.retVal
    def callItem = f.retVal.item
    def callType(ts: List[TypeArg]) = f.retVal.substitute(capture(tparams,ts,Map.empty)._1)
    def discards = Nil
    def strip = this
  }
  case class ForwardDen(parent: Option[ClassType], f: ConstructorItem) extends NotTypeApply {
    def tparams = f.tparams
    def params = {
      implicit val env: Tenv = parent match {
        case Some(p) => p.env
        case None => Map.empty
      }
      f.params map (_.substitute)
    }
    def result = VoidType
    def callItem = VoidItem
    def callType(ts: List[TypeArg]) = VoidType
    def discards = Nil
    def strip = this
  }
  // parent is the parent of the class being created, i.e. in "new A<X>.B<Y>.C(x)", parent is A<X>.B<Y>
  case class NewDen(parent: Option[ClassType], f: ConstructorItem, classArgs: Option[List[TypeArg]] = None) extends NotTypeApply {
    private lazy val env: Tenv = {
      val parentEnv: Tenv = parent match {
        case None => Map.empty
        case Some(c) => c.env
      }
      classArgs match {
        case None => parentEnv
        case Some(ts) => capture(f.parent.tparams,ts,parentEnv)._1
      }
    }
    lazy val tparams = classArgs match {
      case None => f.parent.tparams ++ f.tparams // Try to infer both class and constructor parameters
      case Some(_) => f.tparams // We already have the class type arguments
    }
    lazy val params = f.params map (_.substitute(env))
    lazy val result = f.parent.inside.substitute(env)
    def callItem = f.parent
    def callType(ts: List[TypeArg]) = f.parent.generic(classArgs getOrElse ts.take(f.parent.arity), parent match {
      case Some(p) => p
      case None => f.parent.parent.raw // TODO: check that this is right
    })
    def discards = Nil
    def strip = this
  }
  // Evaluate and discard s, then be f
  case class DiscardCallableDen(s: List[Stmt], c: NotTypeApply) extends NotTypeApply {
    def tparams = c.tparams
    def params = c.params
    def result = c.result
    def callItem = c.callItem
    def callType(ts: List[TypeArg]) = c.callType(ts)
    def discards = s ::: c.discards
    def strip = c.strip
  }

  // Add type arguments to a Callable without checking for correctness
  def uncheckedAddTypeArgs(f: Callable, ts: List[TypeArg], hide: Boolean): Callable = f match {
    case _ if ts.isEmpty => f
    case _:TypeApply => impossible
    case NewDen(p,f,None) => val (ts0,ts1) = ts splitAt f.parent.arity
                             uncheckedAddTypeArgs(NewDen(p,f,Some(ts0)),ts1,hide)
    case f:NotTypeApply => TypeApply(f,ts,hide)
  }

  type Dims = Int
  type VarDecl = (Local,Dims,Option[Exp]) // name,dims,init

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
    def discards = vs flatMap (_._3.discards)
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
  case class BreakStmt(label: Option[Label]) extends Stmt {
    def discards = Nil
    def strip = this
  }
  case class ContinueStmt(label: Option[Label]) extends Stmt {
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
  case class ForeachStmt(t: Type, v: Local, e: Exp, s: Stmt) extends Stmt {
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
  sealed abstract class Exp extends ExpOrType with ExpOrCallable with HasDiscard[Exp] {
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
  case class LocalExp(x: Local) extends Exp with NoDiscardExp {
    def item = x.item
    def ty = x.ty
  }
  case class FieldExp(x: Option[Exp], field: FieldItem) extends Exp {
    def item = field.item
    def ty = if (field.isStatic || x.isEmpty) field.inside else {
      val t = x.get.ty
      val fp = field.parent
      collectOne(supers(t)){
        case t:ClassType if t.item==fp => field.inside.substitute(t.env)
      }.getOrElse(throw new RuntimeException(s"Field $field not found in $t"))
    }
    def discards = x.discards
    def strip = FieldExp(x map (_.strip),field)
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
  case class ApplyExp(f: Callable, args: List[Exp], auto: Boolean) extends StmtExp {
    def item = f.callItem
    def ty = f.callType(Nil)
    def discards = f.discards ::: args flatMap (_.discards)
    def strip = ApplyExp(f.strip,args map (_.strip),auto)
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
    case _:Lit|_:LocalExp|_:ThisExp|_:SuperExp => true
    case _:CastExp|_:AssignExp|_:ApplyExp|_:IndexExp|_:ArrayExp|_:EmptyArrayExp|_:ImpExp|_:DiscardExp => false
    case FieldExp(None,_) => true
    case FieldExp(Some(x),_) => noEffects(x)
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
    case _:Lit|_:LocalExp|_:ThisExp|_:SuperExp => Nil
    case _:CastExp|_:IndexExp => Nil
    case FieldExp(None,_) => Nil
    case FieldExp(Some(x),_) => effects(x)
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
