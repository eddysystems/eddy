package tarski

import tarski.AST.Name
import utility.Utility._
import utility.Locations._
import tarski.Base._
import tarski.Environment.Env
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
  sealed trait TypeOrCallable extends Den
  sealed trait TypeOrPackage extends Den
  sealed trait ExpOrCallable extends Den

  // Wrapped packages.  Items.Package inherits from this.
  trait PackageDen extends ParentDen with TypeOrPackage {
    def p: Package
  }

  // Wrapped types
  case class TypeDen(beneath: Type) extends ExpOrType with TypeOrCallable with TypeOrPackage {
    def item = beneath.item
    def array = TypeDen(ArrayType(beneath))

    def flatMap[A](f: Type => Scored[A]): Scored[A] = f(beneath)
  }

  // Callables
  sealed abstract class Callable extends ExpOrCallable with TypeOrCallable with Signature with HasRange {
    def tparams: List[TypeVar]
    def params: List[Type]
    def callItem: TypeItem
    def callType(ts: List[TypeArg]): Type
  }
  sealed abstract class NotTypeApply extends Callable
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
  }
  // the full expression could be "parentObj.new<targs> type.Class<classArgs>", which is then a callable.
  // "parentObj." is needed only if Class is an inner class. The targs are not part of NewDen (which is why it's always
  // NotTypeApply, even if there are Some classArgs).
  case class NewDen(nr: SRange, parentObj: Option[Exp], f: ConstructorItem, fr: SRange,
                    classArgs: Option[Grouped[List[TypeArg]]] = None) extends NotTypeApply {
    def r = nr union fr unionR classArgs
    def parent = parentObj map (_.ty.asInstanceOf[ClassType])
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
  }
  case class NewArrayDen(nr: SRange, t: Type, tr: SRange, ns: List[Grouped[Exp]], ds: List[SGroup]) extends NotTypeApply {
    def r = nr unionR ns union ds
    lazy val result = arrays(t,ns.size+ds.size)
    def callItem = ArrayItem
    def callType(ts: List[TypeArg]) = { assert(ts.isEmpty); result }
    def tparams = Nil
    def params = if (ns.nonEmpty) Nil
                 else throw new RuntimeException("Should be variadic, but we don't handle that yet")
  }

  // Add type arguments to a Callable without checking for correctness
  def uncheckedAddTypeArgs(f: Callable, ts: List[TypeArg], a: SGroup, hide: Boolean): Callable = f match {
    case _ if ts.isEmpty => f
    case _:TypeApply => impossible
    case NewDen(nr,p,f,fr,None) => val (ts0,ts1) = ts splitAt f.parent.arity
                                   uncheckedAddTypeArgs(NewDen(nr,p,f,fr,Some(Grouped(ts0,a))),ts1,a,hide)
    case f:NotTypeApply => TypeApply(f,ts,a,hide)
  }

  // Variable declarations.  The env is the environment *before* the declaration.
  type Dims = List[SGroup]
  case class VarDecl(x: Local, xr: SRange, d: Dims, i: Option[(SRange,Exp)], env: Env) extends HasRange {
    def r = i match { case None => xr; case Some((_,i)) => xr union i.r }
  }

  // Statements
  sealed abstract class Stmt extends HasRange {
    def env: Env // The environment *before* the statement
    def envAfter: Env // The environment *after* the statement
    def isBlock: Boolean = false // Are we a block?
    def flatten: List[Stmt] = List(this) // Flatten MultipleStmts
  }
  sealed trait ForInit {
    def env: Env // The environment *before* the for initializer
    def envAfter: Env // The environment *after* the for initializer
  }
  case class SemiStmt(s: Stmt, sr: SRange) extends Stmt {
    def env = s.env
    override def envAfter = s.envAfter
    def r = s.r union sr
  }
  case class EmptyStmt(r: SRange, env: Env) extends Stmt {
    def envAfter = env
  }
  case class HoleStmt(r: SRange, env: Env) extends Stmt {
    def envAfter = env
  }
  case class VarStmt(m: Mods, t: Type, tr: SRange, vs: List[VarDecl], envAfter: Env) extends Stmt with ForInit {
    var env = vs.head.env
    def r = tr unionR vs unionR m
  }
  case class ExpStmt(e: StmtExp, env: Env) extends Stmt {
    def envAfter = env
    def r = e.r
  }
  case class BlockStmt(b: List[Stmt], a: SGroup, env: Env) extends Stmt {
    assert(b forall (!_.isInstanceOf[MultipleStmt]))
    override def isBlock = true
    def envAfter = env
    def r = a.lr
  }
  // Like a block statement, but local variables remain in scope afterwards.  Used by effect discarding.
  case class MultipleStmt(b: List[Stmt]) extends Stmt {
    assert(b forall (!_.isInstanceOf[MultipleStmt]))
    def env = b.head.env
    def envAfter = b.last.envAfter
    def r = b.head.r union b.last.r
    override def flatten = b
  }
  case class AssertStmt(ar: SRange, c: Exp, m: Option[(SRange,Exp)], env: Env) extends Stmt {
    def envAfter = env
    def r = ar union c.r union m.map(_._2.r)
  }
  case class BreakStmt(br: SRange, label: Option[Loc[Label]], env: Env) extends Stmt {
    def envAfter = env
    def r = br unionR label
  }
  case class ContinueStmt(cr: SRange, label: Option[Loc[Label]], env: Env) extends Stmt {
    def envAfter = env
    def r = cr unionR label
  }
  case class ReturnStmt(rr: SRange, e: Option[Exp], env: Env) extends Stmt {
    def envAfter = env
    def r = rr unionR e
  }
  case class ThrowStmt(tr: SRange, e: Exp, env: Env) extends Stmt {
    def envAfter = env
    def r = tr union e.r
  }
  case class IfStmt(ir: SRange, c: Exp, a: SGroup, x: Stmt) extends Stmt {
    def env = x.env
    def envAfter = env
    def r = ir union x.r
  }
  case class IfElseStmt(ir: SRange, c: Exp, a: SGroup, x: Stmt, er: SRange, y: Stmt) extends Stmt {
    assert(!x.isInstanceOf[IfStmt])
    def env = x.env
    def envAfter = env
    def r = ir union y.r
  }
  case class WhileStmt(wr: SRange, c: Exp, a: SGroup, s: Stmt) extends Stmt {
    def env = s.env
    def envAfter = env
    def r = wr union s.r
  }
  case class DoStmt(dr: SRange, s: Stmt, wr: SRange, c: Exp, a: SGroup) extends Stmt {
    def env = s.env
    def envAfter = env
    def r = dr union a.r
  }
  case class ForStmt(fr: SRange, i: ForInit, c: Option[Exp], sr: SRange, u: List[Exp], a: SGroup, s: Stmt) extends Stmt {
    def env = i.env
    def envAfter = env
    def r = fr union s.r
  }
  case class ForExps(i: List[Exp], sr: SRange, env: Env) extends ForInit {
    def envAfter = env
  }
  case class ForeachStmt(fr: SRange, m: Mods, t: Type, tr: SRange, v: Local, vr: SRange,
                         e: Exp, a: SGroup, s: Stmt, env: Env) extends Stmt {
    def envAfter = env
    def r = fr union s.r
  }
  case class SyncStmt(sr: SRange, e: Exp, a: SGroup, s: Stmt) extends Stmt {
    assert(s.isBlock)
    def env = s.env
    def envAfter = env
    def r = sr union s.r
  }
  case class CatchBlock(m: Mods, tr: SRange, v: Local, vr: SRange, a: SGroup, s: Stmt) extends HasRange {
    assert(s.isBlock)
    def r = s.r unionR m union tr union vr
  }
  case class TryStmt(tr: SRange, s: Stmt, cs: List[CatchBlock], f: Option[(SRange,Stmt)]) extends Stmt {
    f foreach {case (_,f) => assert(f.isBlock)}
    def env = s.env
    def envAfter = env
    def r = tr union s.r unionR cs unionR (f map (_._2))
  }
  case class TokStmt(t: StmtTok, r: SRange, env: Env) extends Stmt {
    override def isBlock = t.isBlock
    def envAfter = env
  }
  case class DiscardStmt(ds: List[Stmt], s: Stmt) extends Stmt {
    def env = s.env
    def envAfter = s.env
    def r = s.r
    override def flatten = impossible
  }

  // It's all expressions from here
  sealed abstract class Exp extends ExpOrType with ExpOrCallable with HasRange {
    def ty: Type
    def item: TypeItem // Faster version of ty.item
  }
  sealed trait StmtExp extends Exp

  // Literals
  sealed abstract class Lit extends Exp with HasRange {
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
  case class LocalExp(x: Local, r: SRange) extends Exp {
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
  }
  case class ThisExp(t: ThisItem, r: SRange) extends Exp {
    def item = t.item
    def ty = t.ty
  }
  // t is the type super is used in, t.base is the type of this expression
  case class SuperExp(s: SuperItem, r: SRange) extends Exp {
    def item = s.item
    def ty = s.ty
  }
  case class CastExp(ty: Type, a: SGroup, e: Exp) extends Exp {
    def r = a.l union e.r
    def item = ty.item
  }
  sealed abstract class UnaryExp extends Exp {
    def op: UnaryOp
    def opr: SRange
    def e: Exp
    def r = opr union e.r
    def ty = unaryType(op,e.ty) getOrElse (throw new RuntimeException("type error"))
    def item = ty.item
  }
  case class ImpExp(op: ImpOp, opr: SRange, e: Exp) extends UnaryExp with StmtExp
  case class NonImpExp(op: NonImpOp, opr: SRange, e: Exp) extends UnaryExp
  case class BinaryExp(op: BinaryOp, opr: SRange, e0: Exp, e1: Exp) extends Exp {
    def r = e0.r union e1.r
    def ty = binaryType(op,e0.ty,e1.ty) getOrElse (throw new RuntimeException("type error: " + e0.ty + " " + op + " " + e1.ty))
    def item = ty.item
  }
  case class InstanceofExp(e: Exp, ir: SRange, t: Type, tr: SRange) extends Exp {
    def r = e.r union tr
    def ty = BooleanType
    def item = ubBooleanItem
  }
  case class AssignExp(op: Option[AssignOp], opr: SRange, left: Exp, right: Exp) extends StmtExp {
    def r = left.r union right.r
    def item = left.item
    def ty = left.ty
  }
  case class ParenExp(e: Exp, a: SGroup) extends Exp {
    def r = a.lr
    def item = e.item
    def ty = e.ty
  }
  case class ApplyExp(f: Callable, args: List[Exp], a: SGroup, auto: Boolean) extends StmtExp {
    def r = f.r union a.r
    lazy val item = f.callItem
    lazy val ty = f.callType(Nil)
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
  }
  case class CondExp(c: Exp, qr: SRange, x: Exp, cr: SRange, y: Exp, ty: Type) extends Exp {
    def r = c.r union y.r
    def item = ty.item
  }
  case class ArrayExp(t: Type, i: List[Exp], a: SGroup) extends StmtExp { // t is the inner type
    def r = a.lr
    def item = ArrayItem
    def ty = ArrayType(t)
  }
  case class EmptyArrayExp(t: Type, i: List[Grouped[Exp]]) extends StmtExp { // new t[i]
    def r = i.head.r union i.last.r
    def item = ArrayItem
    def ty = i.foldLeft(t)((t,i) => ArrayType(t))
  }

  def typeOf(e: Option[Exp]): Type = e match {
    case None => VoidType
    case Some(e) => e.ty
  }

  // Is an expression definitely side effect free?
  def noEffects(e: Exp): Boolean = e match {
    case _:Lit|_:LocalExp|_:ThisExp|_:SuperExp => true
    case _:CastExp|_:AssignExp|_:ApplyExp|_:IndexExp|_:ArrayExp|_:EmptyArrayExp|_:ImpExp => false
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
  def effects(e: Exp)(implicit env: Env): List[Stmt] = e match {
    case e:StmtExp => List(ExpStmt(e,env))
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
      case (Nil,ey) => List(IfStmt(qr,not(c),SGroup.approx(c.r),blocked(ey)))
      case (ex,ey) => List(IfElseStmt(qr,c,SGroup.approx(c.r),notIf(blocked(ex)),er,blocked(ey)))
    }
    case ParenExp(x,_) => effects(x)
  }

  def multiple(ss: List[Stmt]): Stmt = ss match {
    case Nil => impossible
    case List(s) => s
    case ss => MultipleStmt(ss flatMap (_.flatten))
  }

  def blocked(s: Stmt): Stmt = blockedHelper(s.flatten)
  def blocked(ss: List[Stmt]): Stmt = blockedHelper(ss flatMap (_.flatten))
  private[this] def blockedHelper(ss: List[Stmt]): Stmt = ss match {
    case Nil => impossible
    case List(s) => s
    case ss => BlockStmt(ss,SGroup.approx(ss.head.r union ss.last.r),ss.head.env)
  }

  // Make sure we're not a bare if
  def notIf(s: Stmt): Stmt = s match {
    case _:IfStmt => needBlock(s)
    case _ => s
  }

  def needBlock(s: Stmt): Stmt =
    if (s.isBlock) s
    else BlockStmt(List(s),SGroup.approx(s.r),s.env)

  def xor(x: Boolean, y: Exp): Exp = if (x) not(y) else y

  def not(e: Exp): Exp = e match {
    case BooleanLit(x,r) => BooleanLit(!x,r)
    case NonImpExp(NotOp,_,x) => x
    case BinaryExp(op:CompareOp,r,x,y) =>
      val flip = op match {
        case EqOp => NeOp
        case NeOp => EqOp
        case LtOp => GeOp
        case GeOp => LtOp
        case GtOp => LeOp
        case LeOp => GtOp
      }
      BinaryExp(flip,r,x,y)
    case _ => NonImpExp(NotOp,e.r.before,e)
  }

  def addSemi(s: Stmt, sr: SRange): Stmt = s match {
    // If we're adding a semi, the desired location overrides an existing one
    case SemiStmt(x,_) => SemiStmt(s,sr)
    // Some statements need no semicolon
    case _:BlockStmt|_:SyncStmt|_:TryStmt => s
    case TokStmt(t,_,_) if t.isBlock => s
    // For if and similar, add a semicolon to the last substatement
    case IfStmt(ir,c,a,x) => IfStmt(ir,c,a,addSemi(x,sr))
    case IfElseStmt(ir,c,a,x,er,y) => IfElseStmt(ir,c,a,x,er,addSemi(y,sr))
    case WhileStmt(wr,c,a,s) => WhileStmt(wr,c,a,addSemi(s,sr))
    case ForStmt(fr,i,c,sr,u,a,s) => ForStmt(fr,i,c,sr,u,a,addSemi(s,sr))
    case ForeachStmt(fr,m,t,tr,v,vr,e,a,s,env) => ForeachStmt(fr,m,t,tr,v,vr,e,a,addSemi(s,sr),env)
    case DiscardStmt(ds,s) => DiscardStmt(ds,addSemi(s,sr))
    case MultipleStmt(b) => MultipleStmt(b.init ::: List(addSemi(b.last,sr)))
    // Otherwise, add a semicolon
    case _:EmptyStmt|_:HoleStmt|_:VarStmt|_:ExpStmt|_:AssertStmt|_:BreakStmt|_:ContinueStmt
        |_:ReturnStmt|_:ThrowStmt|_:DoStmt|_:TokStmt => SemiStmt(s,sr)
  }

  // Find all locals declared somewhere in here
  def locals(s: Stmt): List[Local] = s match {
    case BlockStmt(b,_,_) => b flatMap locals
    case MultipleStmt(b) => b flatMap locals
    case VarStmt(_,_,_,vs,_) => vs map (_.x)
    case ForStmt(_,i:VarStmt,_,_,_,_,fs) => locals(i) ::: locals(fs)
    case ForStmt(_,_,_,_,_,_,fs) => locals(fs)
    case ForeachStmt(_,_,_,_,v,_,_,_,fs,_) => v :: locals(fs)
    case SemiStmt(s,_) => locals(s)
    case TryStmt(_,ts,cs,fs) => (locals(ts) ::: (cs flatMap { case CatchBlock(_,_,v,_,_,s) => v :: locals(s) })
                                            ::: fs.toList.flatMap(x => locals(x._2)))
    case _ => Nil
  }
}
