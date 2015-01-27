package tarski

import tarski.AST.Name
import utility.Utility._
import utility.Locations._
import tarski.Base._
import tarski.Arounds._
import tarski.Items._
import tarski.Operators._
import tarski.Types._
import tarski.Scores._
import tarski.Tokens._
import tarski.Mods._
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
  implicit class DiscardsQualifier[A <: HasDiscards](val x: Option[(A,SRange)]) extends AnyVal {
    def discards = x match {
      case None => Nil
      case Some((x,_)) => x.discards
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
  sealed abstract class Callable extends ExpOrCallable with TypeOrCallable with Signature with HasDiscard[Callable] with HasRange {
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
  case class TypeApply(c: NotTypeApply, ts: List[TypeArg], a: SGroup, hide: Boolean) extends Callable {
    def r = c.r union a.r
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
    def strip = TypeApply(c.strip,ts,a,hide)
    def discards = c.discards
    def discard(ds: List[Stmt]) = ds match {
      case Nil => this
      case ds => TypeApply(c.discard(ds),ts,a,hide)
    }
  }
  case class MethodDen(x: Option[Exp], f: MethodItem, fr: SRange) extends NotTypeApply {
    def r = fr unionR x
    def dot = fr.before
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
    def strip = MethodDen(x map (_.strip),f,fr)
  }
  case class LocalMethodDen(f: MethodItem, fr: SRange) extends NotTypeApply {
    def r = fr
    def tparams = f.tparams
    def params = f.params
    def result = f.retVal
    def callItem = f.retVal.item
    def callType(ts: List[TypeArg]) = f.retVal.substitute(capture(tparams,ts,Map.empty)._1)
    def discards = Nil
    def strip = this
  }
  case class ForwardDen(x: ThisOrSuper, xr: SRange, f: ConstructorItem) extends NotTypeApply {
    def r = xr
    def tparams = f.tparams
    def params = {
      implicit val env: Tenv = x.ty.env
      f.params map (_.substitute)
    }
    def result = VoidType
    def callItem = VoidItem
    def callType(ts: List[TypeArg]) = VoidType
    def discards = Nil
    def strip = this
  }
  // parent is the parent of the class being created, i.e. in "new A<X>.B<Y>.C(x)", parent is A<X>.B<Y>
  case class NewDen(nr: SRange, parent: Option[ClassType], f: ConstructorItem, fr: SRange,
                    classArgs: Option[Grouped[List[TypeArg]]] = None) extends NotTypeApply {
    def r = nr union fr unionR classArgs
    private lazy val env: Tenv = {
      val parentEnv: Tenv = parent match {
        case None => Map.empty
        case Some(c) => c.env
      }
      classArgs match {
        case None => parentEnv
        case Some(ts) => capture(f.parent.tparams,ts.x,parentEnv)._1
      }
    }
    lazy val tparams = classArgs match {
      case None => f.parent.tparams ++ f.tparams // Try to infer both class and constructor parameters
      case Some(_) => f.tparams // We already have the class type arguments
    }
    lazy val params = f.params map (_.substitute(env))
    lazy val result = f.parent.inside.substitute(env)
    def callItem = f.parent
    def callType(ts: List[TypeArg]) = {
      val args = classArgs match {
        case None => ts.take(f.parent.arity)
        case Some(a) => a.x
      }
      val par = parent match {
        case Some(p) => p
        case None => f.parent.parent.raw // TODO: check that this is right
      }
      f.parent.generic(args,par)
    }
    def discards = Nil
    def strip = this
  }
  case class NewArrayDen(nr: SRange, t: Type, tr: SRange, ns: List[Grouped[Exp]], ds: List[SGroup]) extends NotTypeApply {
    def r = nr unionR ns union ds
    lazy val result = arrays(t,ns.size+ds.size)
    def callItem = ArrayItem
    def callType(ts: List[TypeArg]) = { assert(ts.isEmpty); result }
    def tparams = Nil
    def params = if (ns.nonEmpty) Nil
                 else throw new RuntimeException("Should be variadic, but we don't handle that yet")
    def discards = ns.map(_.x.discards).flatten
    def strip = NewArrayDen(nr,t,tr,ns map (_ map (_.strip)),ds)
  }
  // Evaluate and discard s, then be f
  case class DiscardCallableDen(s: List[Stmt], c: NotTypeApply) extends NotTypeApply {
    def r = c.r
    def tparams = c.tparams
    def params = c.params
    def result = c.result
    def callItem = c.callItem
    def callType(ts: List[TypeArg]) = c.callType(ts)
    def discards = s ::: c.discards
    def strip = c.strip
    override def discard(s2: List[Stmt]) = DiscardCallableDen(s2:::s,c)
  }

  // Add type arguments to a Callable without checking for correctness
  def uncheckedAddTypeArgs(f: Callable, ts: List[TypeArg], a: SGroup, hide: Boolean): Callable = f match {
    case _ if ts.isEmpty => f
    case _:TypeApply => impossible
    case NewDen(nr,p,f,fr,None) => val (ts0,ts1) = ts splitAt f.parent.arity
                                   uncheckedAddTypeArgs(NewDen(nr,p,f,fr,Some(Grouped(ts0,a))),ts1,a,hide)
    case f:NotTypeApply => TypeApply(f,ts,a,hide)
  }

  type Dims = List[SGroup]
  case class VarDecl(x: Local, xr: SRange, d: Dims, i: Option[(SRange,Exp)]) extends HasDiscards with HasRange {
    def r = i match { case None => xr; case Some((_,i)) => xr union i.r }
    def discards = mapOrElse(i)(_._2.discards,Nil)
    def strip = VarDecl(x,xr,d,i map {case (e,i) => (e,i.strip)})
  }

  // Statements
  sealed abstract class Stmt extends HasDiscard[Stmt] with HasRange {
    def discard(ds: List[Stmt]) = ds match {
      case Nil => this
      case ds => DiscardStmt(ds,this)
    }
    def strip: Stmt
  }
  sealed trait ForInit extends HasDiscards {
    def strip: ForInit
  }
  case class SemiStmt(s: Stmt, sr: SRange) extends Stmt {
    def r = s.r union sr
    def discards = s.discards
    def strip = SemiStmt(s.strip,sr)
  }
  case class EmptyStmt(r: SRange) extends Stmt {
    def discards = Nil
    def strip = this
  }
  case class HoleStmt(r: SRange) extends Stmt {
    def discards = Nil
    def strip = this
  }
  case class VarStmt(t: Type, tr: SRange, vs: List[VarDecl], m: Mods = Nil) extends Stmt with ForInit {
    def r = tr unionR vs unionR m
    def discards = vs flatMap (_.discards)
    def strip = VarStmt(t,tr,vs map (_.strip),m)
  }
  case class ExpStmt(e: StmtExp) extends Stmt {
    def r = e.r
    def discards = e.discards
    def strip = ExpStmt(e.strip)
  }
  case class BlockStmt(b: List[Stmt], a: SGroup) extends Stmt {
    def r = a.lr
    def discards = b flatMap (_.discards)
    def strip = BlockStmt(b map (_.strip),a)
  }
  case class AssertStmt(ar: SRange, c: Exp, m: Option[(SRange,Exp)]) extends Stmt {
    def r = ar union c.r union m.map(_._2.r)
    def discards = m match { case None => c.discards; case Some((_,m)) => m.discards ::: c.discards }
    def strip = AssertStmt(ar,c.strip,m map {case (r,m) => (r,m.strip)})
  }
  case class BreakStmt(br: SRange, label: Option[Loc[Label]]) extends Stmt {
    def r = br unionR label
    def discards = Nil
    def strip = this
  }
  case class ContinueStmt(cr: SRange, label: Option[Loc[Label]]) extends Stmt {
    def r = cr unionR label
    def discards = Nil
    def strip = this
  }
  case class ReturnStmt(rr: SRange, e: Option[Exp]) extends Stmt {
    def r = rr unionR e
    def discards = e match { case None => Nil; case Some(e) => e.discards }
    def strip = ReturnStmt(rr,e map (_.strip))
  }
  case class ThrowStmt(tr: SRange, e: Exp) extends Stmt {
    def r = tr union e.r
    def discards = e.discards
    def strip = ThrowStmt(tr,e.strip)
  }
  case class IfStmt(ir: SRange, c: Exp, a: SGroup, x: Stmt) extends Stmt {
    def r = ir union x.r
    def discards = c.discards
    def strip = IfStmt(ir,c.strip,a,x)
  }
  case class IfElseStmt(ir: SRange, c: Exp, a: SGroup, x: Stmt, er: SRange, y: Stmt) extends Stmt {
    def r = ir union y.r
    def discards = c.discards
    def strip = IfElseStmt(ir,c.strip,a,x,er,y)
  }
  case class WhileStmt(wr: SRange, c: Exp, a: SGroup, s: Stmt) extends Stmt {
    def r = wr union s.r
    def discards = c.discards
    def strip = WhileStmt(wr,c.strip,a,s)
  }
  case class DoStmt(dr: SRange, s: Stmt, wr: SRange, c: Exp, a: SGroup) extends Stmt {
    def r = dr union a.r
    def discards = c.discards
    def strip = DoStmt(dr,s,wr,c.strip,a)
  }
  case class ForStmt(fr: SRange, i: ForInit, c: Option[Exp], sr: SRange, u: List[Exp], a: SGroup, s: Stmt) extends Stmt {
    def r = fr union s.r
    def discards = i.discards
    def strip = ForStmt(fr,i.strip,c,sr,u,a,s)
  }
  case class ForExps(i: List[Exp], sr: SRange) extends ForInit {
    def discards = i flatMap (_.discards)
    def strip = ForExps(i map (_.strip),sr)
  }
  case class ForeachStmt(fr: SRange, m: Mods, t: Type, tr: SRange, v: Local, vr: SRange, e: Exp, a: SGroup, s: Stmt) extends Stmt {
    def r = fr union s.r
    def discards = e.discards
    def strip = ForeachStmt(fr,m,t,tr,v,vr,e.strip,a,s)
  }
  case class SyncStmt(sr: SRange, e: Exp, a: SGroup, s: Stmt) extends Stmt {
    def r = sr union s.r
    def discards = e.discards
    def strip = SyncStmt(sr,e.strip,a,s)
  }
  case class CatchBlock(m: Mods, tr: SRange, v: Local, vr: SRange, s: BlockStmt) extends HasRange {
    def r = s.r unionR m union tr union vr
  }
  case class TryStmt(tr: SRange, s: BlockStmt, cs: List[CatchBlock], f: Option[(SRange,BlockStmt)]) extends Stmt {
    def r = tr union s.r unionR cs unionR (f map (_._2))
    def discards = Nil
    def strip = this
  }
  case class CommentStmt(c: CommentTok, r: SRange) extends Stmt {
    def discards = Nil
    def strip = this
  }
  case class TokStmt(t: StmtTok, r: SRange) extends Stmt {
    def discards = Nil
    def strip = this
  }
  case class DiscardStmt(ds: List[Stmt], s: Stmt) extends Stmt {
    def r = s.r
    def discards = ds ::: s.discards
    def strip = s.strip
  }

  // It's all expressions from here
  sealed abstract class Exp extends ExpOrType with ExpOrCallable with HasDiscard[Exp] with HasRange {
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
  sealed abstract class Lit extends Exp with NoDiscardExp with HasRange {
    def show: String
  }
  case class ByteLit(b: Byte, show: String, r: SRange) extends Lit {
    def ty = ByteType
    def item = ubByteItem
  }
  case class ShortLit(s: Short, show: String, r: SRange) extends Lit {
    def ty = ShortType
    def item = ubShortItem
  }
  case class IntLit(i: Int, show: String, r: SRange) extends Lit {
    def ty = IntType
    def item = ubIntItem
  }
  case class LongLit(l: Long, show: String, r: SRange) extends Lit {
    def ty = LongType
    def item = ubLongItem
  }
  case class StringLit(s: String, show: String, r: SRange) extends Lit {
    def ty = StringType
    def item = StringItem
  }
  case class FloatLit(f: Float, show: String, r: SRange) extends Lit {
    def ty = FloatType
    def item = ubFloatItem
  }
  case class DoubleLit(d: Double, show: String, r: SRange) extends Lit {
    def ty = DoubleType
    def item = ubDoubleItem
  }
  case class CharLit(c: Char, show: String, r: SRange) extends Lit {
    def ty = CharType
    def item = ubCharItem
  }
  case class BooleanLit(b: Boolean, r: SRange) extends Lit {
    def ty = BooleanType
    def item = ubBooleanItem
    def show = if (b) "true" else "false"
  }
  case class NullLit(r: SRange) extends Lit {
    def ty = NullType
    def item = NullType.item
    def show = "null"
  }

  // Expressions
  case class LocalExp(x: Local, r: SRange) extends Exp with NoDiscardExp {
    def item = x.item
    def ty = x.ty
  }
  case class FieldExp(x: Option[Exp], f: FieldItem, fr: SRange) extends Exp {
    def r = fr unionR x
    def dot = fr.before
    def item = f.item
    def ty = if (f.isStatic || x.isEmpty) f.inside else {
      val t = x.get.ty
      val fp = f.parent
      collectOne(supers(t)){
        case t:ClassType if t.item==fp => f.inside.substitute(t.env)
      }.getOrElse(throw new RuntimeException(s"Field $f not found in $t"))
    }
    def discards = x.discards
    def strip = FieldExp(x map (_.strip),f,fr)
  }
  case class ThisExp(t: ThisItem, r: SRange) extends Exp with NoDiscardExp {
    def item = t.item
    def ty = t.ty
  }
  // t is the type super is used in, t.base is the type of this expression
  case class SuperExp(s: SuperItem, r: SRange) extends Exp with NoDiscardExp {
    def item = s.item
    def ty = s.ty
  }
  case class CastExp(ty: Type, a: SGroup, e: Exp) extends Exp {
    def r = a.l union e.r
    def item = ty.item
    def discards = e.discards
    def strip = CastExp(ty,a,e.strip)
  }
  sealed abstract class UnaryExp extends Exp {
    def op: UnaryOp
    def opr: SRange
    def e: Exp
    def r = opr union e.r
    def ty = unaryType(op,e.ty) getOrElse (throw new RuntimeException("type error"))
    def item = ty.item
    def discards = e.discards
  }
  case class ImpExp(op: ImpOp, opr: SRange, e: Exp) extends UnaryExp with StmtExp {
    def strip = ImpExp(op,opr,e.strip)
  }
  case class NonImpExp(op: NonImpOp, opr: SRange, e: Exp) extends UnaryExp {
    def strip = NonImpExp(op,opr,e.strip)
  }
  case class BinaryExp(op: BinaryOp, opr: SRange, e0: Exp, e1: Exp) extends Exp {
    def r = e0.r union e1.r
    def ty = binaryType(op,e0.ty,e1.ty) getOrElse (throw new RuntimeException("type error"))
    def item = ty.item
    def discards = e0.discards ::: e1.discards
    def strip = BinaryExp(op,opr,e0.strip,e1.strip)
  }
  case class InstanceofExp(e: Exp, ir: SRange, t: Type, tr: SRange) extends Exp {
    def r = e.r union tr
    def ty = BooleanType
    def item = ubBooleanItem
    def discards = e.discards
    def strip = InstanceofExp(e.strip,ir,t,tr)
  }
  case class AssignExp(op: Option[AssignOp], opr: SRange, left: Exp, right: Exp) extends StmtExp {
    def r = left.r union right.r
    def item = left.item
    def ty = left.ty
    def discards = left.discards ::: right.discards
    def strip = AssignExp(op,opr,left.strip,right.strip)
  }
  case class ParenExp(e: Exp, a: SGroup) extends Exp {
    def r = a.lr
    def item = e.item
    def ty = e.ty
    def discards = e.discards
    def strip = ParenExp(e.strip,a)
  }
  case class ApplyExp(f: Callable, args: List[Exp], a: SGroup, auto: Boolean) extends StmtExp {
    def r = f.r union a.r
    def item = f.callItem
    def ty = f.callType(Nil)
    def discards = f.discards ::: args flatMap (_.discards)
    def strip = ApplyExp(f.strip,args map (_.strip),a,auto)
  }
  case class IndexExp(e: Exp, i: Exp, a: SGroup) extends Exp {
    def r = e.r union a.r
    def item = e.ty match {
      case ArrayType(t) => t.item
      case _ => throw new RuntimeException("type error")
    }
    def ty = e.ty match {
      case ArrayType(t) => t
      case _ => throw new RuntimeException("type error")
    }
    def discards = e.discards ::: i.discards
    def strip = IndexExp(e.strip,i.strip,a)
  }
  case class CondExp(c: Exp, qr: SRange, x: Exp, cr: SRange, y: Exp, ty: Type) extends Exp {
    def r = c.r union y.r
    def item = ty.item
    def discards = c.discards ::: x.discards ::: y.discards
    def strip = CondExp(c.strip,qr,x.strip,cr,y.strip,ty)
  }
  case class ArrayExp(t: Type, i: List[Exp], a: SGroup) extends StmtExp { // t is the inner type
    def r = a.lr
    def item = ArrayItem
    def ty = ArrayType(t)
    def discards = i flatMap (_.discards)
    def strip = ArrayExp(t,i map (_.strip),a)
  }
  case class EmptyArrayExp(t: Type, i: List[Grouped[Exp]]) extends StmtExp { // new t[i]
    def r = i.head.r union i.last.r
    def item = ArrayItem
    def ty = i.foldLeft(t)((t,i) => ArrayType(t))
    def discards = i flatMap (_.x.discards)
    def strip = EmptyArrayExp(t,i map (_ map (_.strip)))
  }
  // Evaluate and discard s, then evaluate and return e
  case class DiscardExp(s: List[Stmt], e: Exp) extends Exp {
    def r = e.r
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
    case FieldExp(None,_,_) => true
    case FieldExp(Some(x),_,_) => noEffects(x)
    case NonImpExp(op,_,x) => pure(op) && noEffects(x)
    case InstanceofExp(x,_,_,_) => noEffects(x)
    case BinaryExp(op,_,x,y) => pure(op,x.ty,y.ty) && noEffects(x) && noEffects(y)
    case ParenExp(x,_) => noEffects(x)
    case CondExp(c,_,x,_,y,_) => noEffects(c) && noEffects(x) && noEffects(y)
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
    case CastExp(_,_,x) => effects(x)
    case IndexExp(e,i,_) => effects(e)++effects(i)
    case FieldExp(None,_,_) => Nil
    case FieldExp(Some(x),_,_) => effects(x)
    case NonImpExp(_,_,x) => effects(x)
    case InstanceofExp(x,_,_,_) => effects(x)
    case BinaryExp(_,_,x,y) => effects(x)++effects(y)
    case CondExp(c,qr,x,er,y,_) => (effects(x),effects(y)) match {
      case (Nil,Nil) => Nil
      case (ex,Nil) => List(IfStmt(qr,c,SGroup.approx(c.r),blocked(ex)))
      case (Nil,ey) => List(IfStmt(qr,xor(true,c),SGroup.approx(c.r),blocked(ey)))
      case (ex,ey) => List(IfElseStmt(qr,c,SGroup.approx(c.r),blocked(ex),er,blocked(ey)))
    }
    case ParenExp(x,_) => effects(x)
    case DiscardExp(ds,e) => ds++effects(e)
  }

  def blocked(ss: List[Stmt]): Stmt = ss match {
    case Nil => impossible // EmptyStmt(r)
    case List(s) => s
    case ss => BlockStmt(ss,SGroup.approx(ss.head.r union ss.last.r))
  }

  def needBlock(s: Stmt): Stmt = s match {
    case _:BlockStmt => s
    case TokStmt(t,_) if t.blocked => s
    case _ => BlockStmt(List(s),SGroup.approx(s.r))
  }

  def xor(x: Boolean, y: Exp): Exp =
    if (x) y match {
      case BooleanLit(y,r) => BooleanLit(!y,r)
      case _ => NonImpExp(NotOp,y.r.before,y)
    } else y

  // find all locals declared somewhere in here
  def locals(s: Stmt): List[Local] = s match {
    case BlockStmt(b,_) => b flatMap locals
    case VarStmt(_,_,vs,_) => vs map (_.x)
    case ForStmt(_,i@VarStmt(_,_,_,_),_,_,_,_,fs) => locals(i) ::: locals(fs)
    case ForStmt(_,_,_,_,_,_,fs) => locals(fs)
    case ForeachStmt(_,_,_,_,v,_,_,_,fs) => v :: locals(fs)
    case TryStmt(_,ts,cs,fs) => locals(ts) ::: (cs flatMap { case CatchBlock(_,_,v,_,s) => v :: locals(s) }) ::: fs.toList.flatMap(x => locals(x._2))
    case _ => Nil
  }
}
