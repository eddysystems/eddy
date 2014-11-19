package tarski

import tarski.AST._
import tarski.Items._
import tarski.Base._
import tarski.Denotations.{Exp,typeOf}
import tarski.Constants.constantFits
import ambiguity.Utility._

// Properties of types according to the Java spec, without extra intelligence
object Types {
  // Types
  sealed abstract class Type extends scala.Serializable {
    def item: TypeItem
    def supers: List[RefType] // Immediate super classes
    def isSimple: Boolean // Do we depend on any type parameters?
    def isFinal: Boolean
  }
  sealed abstract class LangType extends Type { // Primitive or void
    def item = LangTypeItem(this)
    def supers = Nil
    def isSimple = true
    def isFinal = true
  }
  case object VoidType extends LangType

  // Primitive types
  sealed abstract class PrimType extends LangType
  sealed abstract class NumType extends PrimType
  case object BooleanType extends PrimType // boolean
  case object ByteType    extends NumType  // byte
  case object ShortType   extends NumType  // short
  case object IntType     extends NumType  // int
  case object LongType    extends NumType  // long
  case object FloatType   extends NumType  // float
  case object DoubleType  extends NumType  // double
  case object CharType    extends NumType  // char

  // Parents of classes (either classes or packages)
  // Inherited by PackageItem and ClassType
  // TODO: If the def is A.L, but L is really defined in the base class B of A, actual = B.
  trait Parent {
    def item: ParentItem
    def env: Tenv
    def isRaw: Boolean
    def isSimple: Boolean
  }

  // Reference types
  sealed abstract class RefType extends Type
  sealed abstract class ClassType extends RefType with Parent {
    def item: ClassItem
    def args: List[RefType]
    def parent: Parent
    def base: ClassType = substitute(item.base)(env)
    def implements: List[ClassType] = item.implements map (substitute(_)(env))
    def supers = base :: implements
    def isFinal: Boolean = notImplemented
  }
  case object ObjectType extends ClassType {
    def item = ObjectItem
    def args = Nil
    def parent = JavaLangPkg
    def env = Map.empty
    override def supers = Nil
    override def isFinal = false
    def isRaw = false
    def isSimple = true
  }
  case class SimpleClassType(item: ClassItem, parent: Parent) extends ClassType {
    def args = Nil
    def env = parent.env
    def isRaw = parent.isRaw
    def isSimple = parent.isSimple
  }
  case class RawClassType(item: ClassItem, parent: Parent) extends ClassType {
    def args = Nil
    def env = item.params.foldLeft(parent.env)((env,p) => env+((p,None)))
    def isRaw = true
    def isSimple = parent.isSimple
  }
  case class GenericClassType(item: ClassItem, args: List[RefType], parent: Parent) extends ClassType {
    def env = (item.params,args).zipped.foldLeft(parent.env)((env,p) => env+((p._1,Some(p._2))))
    def isRaw = parent.isRaw
    def isSimple = false
  }
  case object NullType extends RefType {
    def item = NoTypeItem
    def supers = Nil
    def isFinal = true
    def isSimple = true
  }
  case class ErrorType(name: Name) extends RefType {
    def item = NoTypeItem
    def supers = Nil
    def isFinal = false
    def isSimple = true
  }
  case class ParamType(v: TypeParamItem) extends RefType {
    def item = v
    def supers = v.base :: v.implements
    def isFinal = false
    def isSimple = false
  }
  case class IntersectType(ts: Set[RefType]) extends RefType {
    def item = NoTypeItem
    def supers = ts.toList flatMap (_.supers)
    def isFinal = false
    def isSimple = ts forall (_.isSimple)
  }
  case class ArrayType(t: Type) extends RefType {
    def item = ArrayItem
    def supers = CloneableType :: SerializableType :: (t match {
      case t: RefType => t.supers map ArrayType
      case _ => Nil
    })
    def isFinal = t.isFinal
    def isSimple = t.isSimple
  }

  // Type environments
  // None means the type variable is "raw" and therefore unknown.
  type Tenv = Map[TypeParamItem,Option[RefType]]

  // Basic reference types
  def basicType(i: ClassItem) = SimpleClassType(i,JavaLangPkg)
  val BooleanRefType   = basicType(BooleanItem)
  val CharRefType      = basicType(CharacterItem)
  val ByteRefType      = basicType(ByteItem)
  val ShortRefType     = basicType(ShortItem)
  val IntRefType       = basicType(IntegerItem)
  val LongRefType      = basicType(LongItem)
  val FloatRefType     = basicType(FloatItem)
  val DoubleRefType    = basicType(DoubleItem)
  val StringType       = basicType(StringItem)
  val CloneableType    = basicType(CloneableItem)
  val SerializableType = basicType(SerializableItem)

  // Varieties of primitive types (no unboxing logic here)
  def isIntegral(t: PrimType): Boolean = t match {
    case ByteType|ShortType|IntType|LongType|CharType => true
    case _ => false
  }
  def isNumeric(t: PrimType): Boolean = t match {
    case ByteType|ShortType|IntType|LongType|FloatType|DoubleType|CharType => true
    case BooleanType => false
  }
  def isBoolean(t: PrimType): Boolean = t == BooleanType

  // Unary and binary numeric promotion (without unboxing logic)
  def promote(t: PrimType): PrimType = t match {
    case BooleanType => throw new RuntimeException("can't promote a boolean")
    case ByteType|ShortType|CharType => IntType
    case _ => t
  }
  def promote(t0: PrimType, t1: PrimType) = (t0,t1) match {
    case (_,BooleanType)|(BooleanType,_) => throw new RuntimeException("can't promote a boolean")
    case (_,DoubleType) |(DoubleType,_)  => DoubleType
    case (_,FloatType)  |(FloatType,_)   => FloatType
    case (_,LongType)   |(LongType,_)    => LongType
    case _                               => IntType
  }

  // Box from or unbox to a primitive type
  def box(t: PrimType): RefType = t match {
    case BooleanType => BooleanRefType
    case ByteType => ByteRefType
    case ShortType => ShortRefType
    case IntType => IntRefType
    case LongType => LongRefType
    case FloatType => FloatRefType
    case DoubleType => DoubleRefType
    case CharType => CharRefType
  }
  def unbox(t: Type): Option[PrimType] = t match {
    case p: PrimType    => Some(p)
    case BooleanRefType => Some(BooleanType)
    case ByteRefType    => Some(ByteType)
    case ShortRefType   => Some(ShortType)
    case IntRefType     => Some(IntType)
    case LongRefType    => Some(LongType)
    case FloatRefType   => Some(FloatType)
    case DoubleRefType  => Some(DoubleType)
    case CharRefType    => Some(CharType)
    case _              => None
  }

  // Unbox if necessary to get different classes of primitive types
  def toNumeric(t: Type): Option[PrimType] = unbox(t).filter(isNumeric)
  def toIntegral(t: Type): Option[PrimType] = unbox(t).filter(isIntegral)
  def toBoolean(t: Type): Option[PrimType] = unbox(t).filter(isBoolean)
  def isToNumeric(t: Type): Boolean = toNumeric(t).isDefined
  def isToBoolean(t: Type): Boolean = toBoolean(t).isDefined

  // Is a type a reference type?
  def isRef(t: Type): Boolean = t.isInstanceOf[RefType]
  def isObject(t: Type): Boolean = t==ObjectType

  // Types of unary and binary expressions
  def unaryType(op: UnaryOp, t: Type): Option[Type] = op match {
    case PosOp|NegOp|PreIncOp|PreDecOp|PostIncOp|PostDecOp => toNumeric(t) map promote
    case CompOp => toIntegral(t) map promote
    case NotOp => toBoolean(t)
  }
  def binaryType(op: BinaryOp, t0: Type, t1: Type): Option[Type] = op match {
    case AddOp if t0==StringType || t1==StringType => Some(StringType)
    case MulOp|DivOp|ModOp|AddOp|SubOp => for (n0 <- toNumeric(t0); n1 <- toNumeric(t1)) yield promote(n0,n1)
    case LShiftOp|RShiftOp|UnsignedRShiftOp => for (n0 <- toIntegral(t0); _ <- toIntegral(t1)) yield promote(n0)
    case LtOp|GtOp|LeOp|GeOp => for (n0 <- toNumeric(t0); n1 <- toNumeric(t1)) yield BooleanType
    case EqOp|NeOp => ((t0,t1) match {
        case (BooleanType,_) if isToBoolean(t1) => true
        case (_,BooleanType) if isToBoolean(t0) => true
        case (_:PrimType,_) if isToNumeric(t1) => true
        case (_,_:PrimType) if isToNumeric(t0) => true
        case _ => castsTo(t0,t1) || castsTo(t1,t0)
      }) match {
        case true => Some(BooleanType)
        case false => None
      }
    case AndOp|XorOp|XorOp => (unbox(t0),unbox(t1)) match {
      case (Some(BooleanType),Some(BooleanType)) => Some(BooleanType)
      case (Some(i0),Some(i1)) if isIntegral(i0) && isIntegral(i1) => Some(promote(i0,i1))
      case _ => None
    }
    case AndAndOp|OrOrOp => for (b <- toBoolean(t0); _ <- toBoolean(t1)) yield b
  }

  // TODO: Probably eliminate
  def unaryLegal(op: UnaryOp, t: Type) = unaryType(op,t).isDefined
  def binaryLegal(op: BinaryOp, t0: Type, t1: Type) = binaryType(op,t0,t1).isDefined

  // Does a type contain no raw variable?  I.e., is every type variable known?
  def known(v: TypeParamItem)(implicit env: Tenv): Boolean = env.get(v) match {
    case Some(None) => false
    case _ => true
  }
  def known(t: Type)(implicit cenv: Tenv): Boolean = t match {
    case ParamType(v) => known(v)
    case t:ClassType => t.args.forall(known) && known(t.parent)
    case ArrayType(t) => known(t)
    case IntersectType(xs) => xs forall known
    case ObjectType|_:LangType|NullType|_:ErrorType => true
  }
  def known(t: Parent)(implicit env: Tenv): Boolean = t match {
    case _:PackageItem => true
    case t:ClassType => t.args.forall(known) && known(t.parent)
  }

  // Substitute type parameters in a type.
  // Substitution is intentionally *not* recursive.  Each type parameter is substituted once and only once.
  // I believe this will make recursive generic function callers easier to handle.
  def substitute(t: RefType)(implicit env: Tenv): RefType = t match {
    case t:ClassType => substitute(t)
    case ParamType(v) => env.get(v) match {
      case None => t
      case Some(Some(t)) => t
      case Some(None) => throw new RuntimeException("raw variable encountered in substitute")
    }
    case ArrayType(t:RefType) => ArrayType(substitute(t))
    case IntersectType(xs) => IntersectType(xs map substitute)
    case ObjectType|_:ErrorType|NullType|ArrayType(VoidType|_:PrimType) => t
  }
  def substitute(p: Parent)(implicit env: Tenv): Parent = p match {
    case p:PackageItem => p
    case p:ClassType => substitute(p)
  }
  def substitute(t: ClassType)(implicit env: Tenv): ClassType = t match {
    case ObjectType => t
    case SimpleClassType(i,p) => SimpleClassType(i,substitute(p))
    case RawClassType(i,p) => RawClassType(i,substitute(p))
    case GenericClassType(i,a,p) => {
      val ps = substitute(p)
      if (!ps.isRaw && (a forall known)) GenericClassType(i,a map substitute,ps)
      else RawClassType(i,ps)
    }
  }
  def substitute(t: Type)(implicit env: Tenv): Type = t match {
    case t: RefType => substitute(t)
    case _ => t
  }

  // Substitute given tenv as two lists
  def substitute(vs: List[TypeParamItem], ts: List[RefType], t: Type): Type =
    if (vs.isEmpty) t
    else substitute(t)((vs,ts.map(Some(_))).zipped.toMap)

  // Is lo a subtype (or subitem) of hi?
  def isSubtype(lo: Type, hi: Type): Boolean = lo==hi || (lo==NullType || lo.supers.exists(isSubtype(_,hi)))
  def isProperSubtype(lo: Type, hi: Type): Boolean = lo!=hi && (lo==NullType || lo.supers.exists(isSubtype(_,hi)))
  def isSubitem(lo: Type, hi: TypeItem): Boolean = isSubitem(lo.item,hi)
  def isSubitem(lo: TypeItem, hi: TypeItem): Boolean = lo==hi || lo.supers.exists(isSubitem(_,hi))

  // If lo <: hi, extract the type parameters
  def subItemParams(lo: Type, hi: TypeItem): Option[List[RefType]] =
    collectOne(supers(lo)){ case t:ClassType if t.item==hi => t.args }

  // Is a type throwable?
  def isThrowable(t: Type): Boolean = isSubitem(t,ThrowableItem)

  // Is a type iterable or an array?  If so, what does it contain?
  def isIterable(i: Type): Option[Type] = i match {
    case ArrayType(t) => Some(t)
    case _ => subItemParams(i,IterableItem) match {
      case None => None
      case Some(List(t)) => Some(t)
      case _ => throw new RuntimeException("arity mismatch")
    }
  }

  // Widening, narrowing, and widening-and-narrowing primitive conversions: 5.1.2, 5.1.3, 5.1.4
  def widensPrimTo(from: PrimType, to: PrimType): Boolean = (from,to) match {
    case (ByteType,ShortType|IntType|LongType|FloatType|DoubleType)
       | (ShortType|CharType,IntType|LongType|FloatType|DoubleType)
       | (IntType,                   LongType|FloatType|DoubleType)
       | (LongType,                           FloatType|DoubleType)
       | (FloatType,                                    DoubleType) => true
    case _ => false
  }
  def narrowsPrimTo(from: PrimType, to: PrimType): Boolean = (from,to) match {
    case (ShortType, ByteType|CharType)
       | (CharType,  ByteType|ShortType)
       | (IntType,   ByteType|ShortType|CharType)
       | (LongType,  ByteType|ShortType|CharType|IntType)
       | (FloatType, ByteType|ShortType|CharType|IntType|LongType)
       | (DoubleType,ByteType|ShortType|CharType|IntType|LongType|FloatType) => true
    case _ => false
  }
  def widensNarrowsPrimTo(from: PrimType, to: PrimType): Boolean = (from,to) match {
    case (ByteType,CharType) => true
    case _ => false
  }

  // Widening and narrowing reference conversions: 5.1.5, 5.1.6
  def widensRefTo(from: RefType, to: RefType): Boolean = isProperSubtype(from,to)
  def narrowsRefTo(from: RefType, to: RefType): Boolean = isProperSubtype(to,from) || (from!=to && ((from,to) match {
    case (f:ClassType,t:ClassType) => !f.isFinal && !t.isFinal && t.isSimple
    case (ArrayType(f:RefType),ArrayType(t:RefType)) => narrowsRefTo(f,t)
    case _ => false
  }))

  // Boxing and unboxing conversions: 5.1.7, 5.1.8
  def boxesTo(from: Type, to: RefType): Boolean = from match {
    case f: PrimType => box(f)==to
    case NullType => to==NullType
    case _ => false
  }
  def unboxesTo(from: Type, to: PrimType): Boolean = from==box(to)

  // Generic-related conversions: 5.1.9, 5.1.10
  def uncheckedConvertsTo(from: Type, to: Type): Boolean = {
    // TODO
    false
  }
  def captureConvertsTo(from: Type, to: Type): Boolean = {
    // TODO
    false
  }

  // Assignment contexts: 5.2
  // TODO: Handle unchecked conversions
  def assignsTo(e: Exp, to: Type): Boolean =
    typeAssignsTo(typeOf(e),to) || (unbox(to) exists (constantFits(e,_)))
  def assignsTo(e: Option[Exp], to: Type): Boolean = e match {
    case None => to==VoidType
    case Some(e) => assignsTo(e,to)
  }
  def typeAssignsTo(from: Type, to: Type): Boolean = {
    (from,to) match {
      case _ if from==to => true
      case (f: PrimType, t: PrimType) => widensPrimTo(f,t)
      case (f: RefType, t: RefType) => widensRefTo(f,t)
      case (f: PrimType, t: RefType) => widensRefTo(box(f),t)
      case (f: RefType, t: PrimType) => unbox(f) match {
        case Some(fp) => widensPrimTo(fp,t)
        case None => false
      }
    }
  }

  // Invocation contexts: 5.3
  // TODO: Handle unchecked conversions
  def strictInvokeContext(from: Type, to: Type): Boolean = (from,to) match {
    case _ if from==to => true
    case (f: PrimType, t: PrimType) => widensPrimTo(f,t)
    case (f: RefType, t: RefType) => widensRefTo(f,t)
    case _ => false
  }
  def looseInvokeContext(from: Type, to: Type): Boolean = (from,to) match {
    case _ if from==to => true
    case (f: PrimType, t: PrimType) => widensPrimTo(f,t)
    case (f: RefType, t: RefType) => widensRefTo(f,t)
    case (f: PrimType, t: RefType) => widensRefTo(box(f),t)
    case (f: RefType, t: PrimType) => unbox(f) match {
      case Some(fp) => widensPrimTo(fp,t)
      case None => false
    }
  }

  // Whether from can be explicitly cast to to
  def castsTo(from: Type, to: Type): Boolean = from==to || ((from,to) match {
    case (_:ErrorType,_)|(_,_:ErrorType) => false
    case (VoidType,_) => false
    case (_,VoidType) => true
    case (f:PrimType,t:PrimType) => (f==BooleanType)==(t==BooleanType)
    case (f:RefType, t:PrimType) => (unbox(f),t) match {
      case (None,_) => isObject(f)
      case (Some(ByteType),(ByteType|ShortType|IntType|LongType|FloatType|DoubleType))
         | (Some(ShortType),        (ShortType|IntType|LongType|FloatType|DoubleType))
         | (Some(CharType),          (CharType|IntType|LongType|FloatType|DoubleType))
         | (Some(IntType),                    (IntType|LongType|FloatType|DoubleType))
         | (Some(LongType),                           (LongType|FloatType|DoubleType))
         | (Some(FloatType),                                   (FloatType|DoubleType))
         | (Some(DoubleType),                                             DoubleType) => true
      case _ => false
    }
    case (f:PrimType,t:RefType) => isObject(t) || unbox(t)==Some(f)
    case (f:RefType,t:RefType) => isSubtype(f,t) || isSubtype(t,f)
  })

  // All supertypes of a reference type, including self
  def supers(t: RefType): Set[RefType] = {
    def loop(ss: Set[RefType], t: RefType): Set[RefType] = if (ss contains t) ss else t.supers.foldLeft(ss+t)(loop(_,_))
    loop(Set(),t)
  }
  def supers(t: Type): Set[RefType] = t match {
    case t:RefType => supers(t)
    case _ => Set()
  }

  // Least upper bounds: 4.10.4
  // TODO: Handle generics
  def lub(x: RefType, y: RefType): RefType = (x,y) match {
    case (x,NullType) => x
    case (NullType,y) => y
    case (x,y) =>
      val ss = supers(x) & supers(y)
      val mec = ss.filter(t => ss.forall(s => s==t || !isSubtype(s,t)))
      mec.toList match {
        case Nil => throw new RuntimeException("lub should never fail")
        case List(t) => t
        case _ => IntersectType(mec)
      }
  }
  // TODO: Implementing lub with fold is not correct
  def lub(xs: List[RefType]): RefType = xs match {
    case Nil => NullType
    case List(x) => x
    case x::xs => lub(x,lub(xs))
  }

  // Greatest lower bounds: 5.1.10
  def glb(xs: List[RefType]): RefType = xs match {
    case Nil => ObjectType
    case List(x) => x
    case xs => IntersectType(xs.toSet)
  }

  // Combine left and right sides of a conditional expression
  // TODO: Handle poly expressions (admittedly, this doesn't even make sense with this signature)
  def condType(x: Type, y: Type): Type = {
    def pp(x: PrimType, y: PrimType): Type = (x,y) match {
      case (BooleanType,BooleanType) => BooleanType
      case (BooleanType,n:NumType) => lub(BooleanRefType,box(n))
      case (n:NumType,BooleanType) => lub(BooleanRefType,box(n))
      case (x:NumType,y:NumType) => promote(x,y)
    }
    def pr(x: PrimType, y: RefType): Type = (x,unbox(y)) match {
      case (x,None) => lub(box(x),y)
      case (BooleanType,Some(BooleanType)) => BooleanType
      case (BooleanType,Some(y:NumType)) => lub(BooleanRefType,box(y))
      case (x:NumType,Some(BooleanType)) => lub(BooleanRefType,box(x))
      case (x:NumType,Some(y:NumType)) => promote(x,y)
    }
    (x,y) match {
      case (x:ErrorType,_) => x // Propagate error types for simplicity
      case (_,y:ErrorType) => y
      case (VoidType,_)|(_,VoidType) => VoidType
      case (x:PrimType,y:PrimType) => pp(x,y)
      case (x:PrimType,y:RefType) => pr(x,y)
      case (x:RefType,y:PrimType) => pr(y,x)
      case (x:RefType,y:RefType) => lub(x,y)
    }
  }

  // Combine a bunch of types into a single type (for array literal purposes)
  // TODO: This function is a bit strange, since array literals do not exist in Java.
  def condTypes(ts: List[Type]): Type = ts match {
    case Nil => ObjectType // TODO: Doesn't handle zero size primitive type arrays
    case List(x) => x
    case x :: xs => condType(x,condTypes(xs))
  }

  // Is t0 op= t1 valid?
  def assignOpType(op: Option[AssignOp], t0: Type, t1: Type): Option[Type] = op match {
    case Some(op) => binaryType(op,t0,t1) filter (castsTo(_,t0))
    case None => if (typeAssignsTo(t1,t0)) Some(t0) else None
  }

  // Convenience functions for arrays
  def dimensions(t: Type): Int = t match {
    case ArrayType(t) => 1+dimensions(t)
    case _ => 0
  }
  def arrays(t: Type, dims: Int): Type = {
    if (dims == 0) t
    else arrays(ArrayType(t),dims-1)
  }

  // Method resolution: generics and overloads, 15.12.2
  // Given a list of callables, find the most specific one along with its type parameters
  // TODO: Handle explicit type parameters, possibly by prefiltering fs outside of this function
  trait Signature {
    def tparams: List[TypeParamItem]
    def params: List[Type]

    def isGeneric: Boolean = tparams.nonEmpty
    def arity: Int = params.size
  }
  // given argument types ts, which signatures are still usable? ts is allowed to be shorter than f.params,
  // all signatures still possible after matching the prefix are returned.
  def resolveOptions[F <: Signature](fs: List[F], ts: List[Type]): List[(F,List[RefType])] = {
    val n = ts.size
    // TODO: Handle access restrictions (public, private, protected) and scoping
    // TODO: Handle variable arity
    def potentiallyCompatible(f: F): Boolean = f.arity >= n && {
      (f.params,ts).zipped forall {case (p,t) => true}} // TODO: Handle poly expression constraints
    def compatible(f: F, form: Inference.Form, context: (Type,Type) => Boolean): Option[List[RefType]] =
      if (!f.isGeneric)
        if ((f.params.slice(0,ts.size),ts).zipped forall {case (p,t) => context(t,p)}) Some(Nil)
        else None
      else Inference.infer(f.tparams,f.params.slice(0,ts.size),ts)(form)
    def strictCompatible(f: F): Option[List[RefType]] = compatible(f,Inference.strictBounds, strictInvokeContext)
    def looseCompatible (f: F): Option[List[RefType]] = compatible(f,Inference.looseBounds , looseInvokeContext)
    // narrow down candidates
    val potential = fs filter potentiallyCompatible
    val applies = potential flatMap (f => strictCompatible(f).map((f,_)).toList) match {
      case Nil => potential flatMap (f =>  looseCompatible(f).map((f,_)).toList)
      case fs => fs
    }
    applies
  }

  // resolve an overloaded function
  def resolve[F <: Signature](fs: List[F], ts: List[Type]): Option[(F,List[RefType])] = {
    def mostSpecific(fs: List[(F,List[RefType])]): Option[(F,List[RefType])] = fs match {
      case Nil => None
      case List(f) => Some(f)
      case _ => notImplemented // TODO: more inference
    }
    // Pick function
    mostSpecific(resolveOptions(fs, ts))
  }


  // Make sure a type can be written in Java
  def safe(t: Type): Option[Type] = t match {
    case r:RefType => safe(r)
    case _:LangType => Some(t)
  }
  def safe(t: RefType): Option[RefType] = t match {
    case NullType => Some(ObjectType)
    case t:ClassType => safe(t)
    case IntersectType(ts) => allSome(ts map safe) map (IntersectType(_))
    case ArrayType(t) => safe(t) map ArrayType
    case _:ErrorType|_:ParamType => Some(t)
  }
  def safe(t: ClassType): Option[ClassType] = t match {
    case ObjectType => Some(t)
    case SimpleClassType(i,p) => safe(p) map (SimpleClassType(i,_))
    case RawClassType(i,p) => safe(p) map (RawClassType(i,_))
    case GenericClassType(i,a,p) => safe(p) flatMap (p => allSome(a map safe) map (GenericClassType(i,_,p)))
  }
  def safe(t: Parent): Option[Parent] = t match {
    case t:PackageItem => Some(t)
    case t:ClassType => safe(t)
  }
}

