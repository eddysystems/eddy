package tarski

import tarski.AST._
import tarski.Items._
import tarski.Base._
import ambiguity.Utility._

// Properties of types according to the Java spec, without extra intelligence
object Types {
  // Types
  sealed abstract class Type extends scala.Serializable
  sealed trait SimpleType extends Type // Definitely no type variables
  case object VoidType extends Type with SimpleType

  // Primitive types
  sealed abstract class PrimType extends Type with SimpleType
  sealed abstract class NumType extends PrimType
  case object BooleanType extends PrimType // boolean
  case object ByteType    extends NumType  // byte
  case object ShortType   extends NumType  // short
  case object IntType     extends NumType  // int
  case object LongType    extends NumType  // long
  case object FloatType   extends NumType  // float
  case object DoubleType  extends NumType  // double
  case object CharType    extends NumType  // char

  // Reference types
  sealed abstract class RefType extends Type
  sealed abstract class ClassOrObjectType extends RefType
  sealed trait ClassOrInterfaceType extends RefType {
    def d: TypeItem
    def args: List[RefType]
  }
  sealed trait SimpleClassOrInterface extends RefType with SimpleType with ClassOrInterfaceType {
    def d: TypeItem
    def args = Nil
  }
  sealed trait GenericType extends RefType with ClassOrInterfaceType {
    def d: TypeItem
    def args: List[RefType]
  }
  sealed abstract class InterfaceType(val d: InterfaceItem) extends RefType {
    def bases: List[InterfaceType]
  }
  sealed abstract class ClassType(val d: ClassItem) extends ClassOrObjectType {
    def base: ClassOrObjectType
    def implements: List[InterfaceType]
  }
  case object NullType extends RefType with SimpleType
  case object ObjectType extends ClassOrObjectType with SimpleType
  case class ErrorType(name: Name) extends RefType with SimpleType
  case class SimpleInterfaceType(override val d: InterfaceItem) extends InterfaceType(d) with SimpleClassOrInterface {
    def bases = d.bases
  }
  case class GenericInterfaceType(override val d: InterfaceItem, args: List[RefType])
    extends InterfaceType(d) with GenericType {
    implicit def tenv = (d.params,args).zipped.toMap
    def bases = d.bases map substitute
  }
  case class SimpleClassType(override val d: ClassItem) extends ClassType(d) with SimpleClassOrInterface {
    def base = d.base
    def implements = d.implements
  }
  case class GenericClassType(override val d: ClassItem, args: List[RefType]) extends ClassType(d) with GenericType {
    implicit def tenv = (d.params,args).zipped.toMap
    def base = substitute(d.base)
    def implements = d.implements map substitute
  }
  case class ParamType(x: TypeParamItem) extends RefType
  case class IntersectType(ts: Set[RefType]) extends RefType
  case class ArrayType(t: Type) extends RefType
  // TODO: Raw types

  // Basic reference types
  val BooleanRefType = SimpleClassType(BooleanItem)
  val CharRefType    = SimpleClassType(CharacterItem)
  val ByteRefType    = SimpleClassType(ByteItem)
  val ShortRefType   = SimpleClassType(ShortItem)
  val IntRefType     = SimpleClassType(IntegerItem)
  val LongRefType    = SimpleClassType(LongItem)
  val FloatRefType   = SimpleClassType(FloatItem)
  val DoubleRefType  = SimpleClassType(DoubleItem)
  val StringType     = SimpleClassType(StringItem)
  val CloneableType    = SimpleInterfaceType(CloneableItem)
  val SerializableType = SimpleInterfaceType(SerializableItem)

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
    case PosOp()|NegOp()|PreIncOp()|PreDecOp()|PostIncOp()|PostDecOp() => toNumeric(t) map promote
    case CompOp() => toIntegral(t) map promote
    case NotOp() => toBoolean(t)
  }
  def binaryType(op: BinaryOp, t0: Type, t1: Type): Option[Type] = op match {
    case AddOp() if t0==StringType || t1==StringType => Some(StringType)
    case MulOp()|DivOp()|ModOp()|AddOp()|SubOp() => for (n0 <- toNumeric(t0); n1 <- toNumeric(t1)) yield promote(n0,n1)
    case LShiftOp()|RShiftOp()|UnsignedRShiftOp() => for (n0 <- toIntegral(t0); _ <- toIntegral(t1)) yield promote(n0)
    case LtOp()|GtOp()|LeOp()|GeOp() => for (n0 <- toNumeric(t0); n1 <- toNumeric(t1)) yield BooleanType
    case EqOp()|NeOp() => ((t0,t1) match {
        case (BooleanType,_) if isToBoolean(t1) => true
        case (_,BooleanType) if isToBoolean(t0) => true
        case (_:PrimType,_) if isToNumeric(t1) => true
        case (_,_:PrimType) if isToNumeric(t0) => true
        case _ => castsTo(t0,t1) || castsTo(t1,t0)
      }) match {
        case true => Some(BooleanType)
        case false => None
      }
    case AndOp()|XorOp()|XorOp() => (unbox(t0),unbox(t1)) match {
      case (Some(BooleanType),Some(BooleanType)) => Some(BooleanType)
      case (Some(i0),Some(i1)) if isIntegral(i0) && isIntegral(i1) => Some(promote(i0,i1))
      case _ => None
    }
    case AndAndOp()|OrOrOp() => for (b <- toBoolean(t0); _ <- toBoolean(t1)) yield b
  }

  // TODO: Probably eliminate
  def unaryLegal(op: UnaryOp, t: Type) = unaryType(op,t).isDefined
  def binaryLegal(op: BinaryOp, t0: Type, t1: Type) = binaryType(op,t0,t1).isDefined

  // Substitute type parameters in a type.
  // Substitution is intentionally *not* recursive.  Each type parameter is substituted once and only once.
  // I believe this will make recursive generic function callers easier to handle.
  type TEnv = Map[TypeParamItem,RefType]
  def substitute(t: RefType)(implicit tenv: TEnv): RefType = t match {
    case ParamType(v) => tenv.getOrElse(v,t)
    case GenericInterfaceType(d,xs) => GenericInterfaceType(d,xs map substitute)
    case GenericClassType(d,xs) => GenericClassType(d,xs map substitute)
    case ArrayType(t:RefType) => ArrayType(substitute(t))
    case IntersectType(xs) => IntersectType(xs map substitute)
    case _:SimpleType|ArrayType(VoidType|_:PrimType) => t
  }
  def substitute(t: InterfaceType)(implicit tenv: TEnv): InterfaceType = t match {
    case SimpleInterfaceType(_) => t
    case GenericInterfaceType(d,xs) => GenericInterfaceType(d,xs map substitute)
  }
  def substitute(t: ClassOrObjectType)(implicit tenv: TEnv): ClassOrObjectType = t match {
    case ObjectType|SimpleClassType(_) => t
    case GenericClassType(d,xs) => GenericClassType(d,xs map substitute)
  }
  def substituteAny(t: Type)(implicit tenv: TEnv): Type = t match {
    case t: RefType => substitute(t)
    case _ => t
  }

  // Substitute given tenv as two lists
  def substitute(vs: List[TypeParamItem], ts: List[RefType], t: Type): Type =
    if (vs.isEmpty) t
    else substituteAny(t)((vs,ts).zipped.toMap)

  def toType(i: PrimTypeItem): Type = i.t

  // Turn a TypeItem into a type
  // TODO: Handle generics
  def toType(i: TypeItem, ts: List[RefType]): Type = {
    val n = i.arity
    assert(n == ts.size)
    if (n == 0) i match {
      case i: InterfaceItem => SimpleInterfaceType(i)
      case c: ClassItem => SimpleClassType(c)
      case ObjectItem => ObjectType
    } else i match {
      case i: InterfaceItem => GenericInterfaceType(i,ts)
      case c: ClassItem => GenericClassType(c,ts)
      case ObjectItem => throw new RuntimeException("Object has no type parameters")
    }
  }

  // If a type has an associated item, return it
  def toItem(t: RefType): Option[TypeItem] = t match {
    case c: ClassType => Some(c.d)
    case i: InterfaceType => Some(i.d)
    case ObjectType => Some(ObjectItem)
    case ArrayType(_)|ErrorType(_)|IntersectType(_)|ParamType(_)|NullType => None
  }

  // Does a class implement an interface?
  def implements(c: ClassType, i: InterfaceType): Boolean = {
    val ci = c.implements
    (   ci.contains(i)
     || ci.exists(isProperSubtype(_,i))
     || (c.base match { case ObjectType => false; case b: ClassType => implements(b,i) }))
  }

  // Is lo a subtype of hi?
  def isSubtype(lo: Type, hi: Type): Boolean = lo == hi || isProperSubtype(lo,hi)
  def isSubtype(lo: RefType, hi: RefType): Boolean = lo == hi || isProperSubtype(lo,hi)
  def isProperSubtype(lo: Type, hi: Type): Boolean = (lo,hi) match {
    case (lo: RefType, hi: RefType) => isProperSubtype(lo,hi)
    case _ => false // Non-reference types aren't part of inheritance
  }
  def isProperSubtype(lo: RefType, hi: RefType): Boolean = (lo,hi) match {
    case _ if lo==hi => false // Not proper
    case (NullType,_) => true // null can be anything
    case (_,ObjectType) => true // Every ref is Object, even interfaces and enums!
    case (ObjectType,_) => false // Object is not a proper subtype of anything

    // Array types are covariant
    case (ArrayType(l),ArrayType(h)) => isProperSubtype(l,h)
    // Otherwise, arrays are not a subtype of anything (except ObjectType, above), and there can be no subtypes of arrays
    // TODO: Actually, arrays are Cloneable and Serializable
    case (ArrayType(_),_)|(_, ArrayType(_)) => false

    // lo is a proper subtype of hi if its superclass is a subtype of hi, or it implements (a subinterface of) hi
    case (lo:InterfaceType,hi:InterfaceType) => lo.bases exists (isSubtype(_,hi))
    case (lo:ClassType,hi:ClassType) => isSubtype(lo.base,hi)
    case (lo:ClassType,hi:InterfaceType) => implements(lo,hi)
    case (_:InterfaceType,_:ClassType) => false

    // Type variables are subtypes of their bounds, but supertypes only of themselves or other type variables
    case (ParamType(v),_) => isSubtype(v.base,hi) || v.implements.exists(isSubtype(_,hi))
    case (_,ParamType(_)) => false
  }

  // Same as above, but for items (types without their type arguments)
  def isSubitem(lo: Type, hi: TypeItem): Boolean = lo match {
    case NullType => true // null can be anything
    case VoidType|_:PrimType|_:ErrorType => false
    case _:RefType if hi==ObjectItem => true // Every ref is Object, even interfaces and enums
    case ObjectType => false // Object is a subtype only of itself
    case ArrayType(_) => false // TODO: Actually, arrays are Cloneable and Serializable
    case lo:ClassOrInterfaceType => isSubitem(lo.d,hi)
  }
  def isSubitem(lo: TypeItem, hi: TypeItem): Boolean = lo==hi || (lo match {
    case ObjectItem => false // Object is a subitem only of itself
    case lo: InterfaceItem => lo.bases exists (isSubitem(_,hi))
    case lo: ClassItem => isSubitem(lo.base,hi) || lo.implements.exists(isSubitem(_,hi))
  })

  // If lo <: hi, extract the type parameters
  def subItemParams(lo: Type, hi: RefTypeItem): Option[List[RefType]] = lo match {
    case NullType => None // null can be anything, but we don't know what
    case VoidType|_:PrimType|_:ErrorType => None
    case t:RefType => subItemParams(t,hi)
  }
  def subItemParams(lo: RefType, hi: RefTypeItem): Option[List[RefType]] = {
    def any(los: List[RefType]): Option[List[RefType]] = los match {
      case Nil => None
      case lo::los => subItemParams(lo,hi) orElse any(los)
    }
    lo match {
      case ObjectType => None
      case ArrayType(_) => None // TODO: Actually, arrays are Cloneable and Serializable
      case lo:ClassOrInterfaceType if lo.d==hi => Some(lo.args)
      case lo:ClassType => subItemParams(lo.base,hi) orElse any(lo.implements)
      case lo:InterfaceType => any(lo.bases)
    }
  }

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

  // Properties of reference types
  def isFinal(t: ClassType): Boolean =
    throw new RuntimeException("Not implemented")

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
  def narrowsRefTo(from: RefType, to: RefType): Boolean = isProperSubtype(to,from) || (!isSubtype(from,to) && ((from,to) match {
    case (f: ClassType,t: SimpleInterfaceType) if !isFinal(f) => true
    case (f: InterfaceType,t: SimpleClassType) if !isFinal(t) => true
    case (f: InterfaceType,t: SimpleInterfaceType) => true
    case (ArrayType(f: RefType),ArrayType(t: RefType)) => narrowsRefTo(f,t)
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
  // TODO: Handle constant expression narrowing conversions
  def assignsTo(from: Type, to: Type): Boolean = (from,to) match {
    case _ if from==to => true
    case (f: PrimType, t: PrimType) => widensPrimTo(f,t)
    case (f: RefType, t: RefType) => widensRefTo(f,t)
    case (f: PrimType, t: RefType) => widensRefTo(box(f),t)
    case (f: RefType, t: PrimType) => unbox(f) match {
      case Some(fp) => widensPrimTo(fp,t)
      case None => false
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
  def supers(t: RefType): Set[RefType] = t match {
    case NullType => throw new RuntimeException("nulltype has infinitely many supertypes")
    case ObjectType|_:ParamType => Set(t)
    case i: InterfaceType => Set(i,ObjectType) ++ (i.bases map supers).flatten
    case c: ClassType => supers(c.base) ++ (c.implements map supers).flatten + c
    case a: ArrayType => Set(a,CloneableType,SerializableType,ObjectType)
    case e: ErrorType => Set(e,ObjectType)
    case IntersectType(ts) => ts.toSet.flatMap(supers)
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
    case None => if (assignsTo(t1,t0)) Some(t0) else None
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

    def isGeneric: Boolean = !tparams.isEmpty
    def arity: Int = params.size
  }
  def resolve[F <: Signature](fs: List[F], ts: List[Type]): Option[(F,List[RefType])] = {
    val n = ts.size
    // TODO: Handle access restrictions (public, private, protected) and scoping
    // TODO: Handle variable arity
    def potentiallyCompatible(f: F): Boolean = f.arity == n && {
      (f.params,ts).zipped forall {case (p,t) => true}} // TODO: Handle poly expression constraints
    def compatible(f: F, form: Inference.Form, context: (Type,Type) => Boolean): Option[List[RefType]] =
      if (!f.isGeneric)
        if ((f.params,ts).zipped forall {case (p,t) => context(t,p)}) Some(Nil)
        else None
      else Inference.infer(f.tparams,f.params,ts)(form)
    def strictCompatible(f: F): Option[List[RefType]] = compatible(f,Inference.strictBounds,strictInvokeContext)
    def looseCompatible (f: F): Option[List[RefType]] = compatible(f,Inference.looseBounds ,looseInvokeContext)
    def mostSpecific(fs: List[(F,List[RefType])]): Option[(F,List[RefType])] = fs match {
      case Nil => None
      case List(f) => Some(f)
      case _ => notImplemented
    }
    // Pick function
    val potential = fs filter potentiallyCompatible
    val applies = potential flatMap (f => strictCompatible(f).map((f,_)).toList) match {
      case Nil => potential flatMap (f =>  looseCompatible(f).map((f,_)).toList)
      case fs => fs
    }
    mostSpecific(applies)
  }

  // Make sure a type can be written in Java
  def safe(t: Type): Option[Type] = t match {
    case r: RefType => safe(r)
    case VoidType => None
    case _:PrimType => Some(t)
  }

  def safe(t: RefType): Option[RefType] = t match {
    case NullType => Some(ObjectType)
    case ObjectType|ErrorType(_)|SimpleInterfaceType(_)|SimpleClassType(_)|ParamType(_) => Some(t)
    case GenericInterfaceType(d,ts) => {
      val sts = ts map safe
      if (sts exists { x=>x.isDefined })
        None
      else
        Some(GenericInterfaceType(d,sts.map(x => x.get)))
    }
    case GenericClassType(d,ts) => {
      val sts = ts map safe
      if (sts exists { x=>x.isDefined })
        None
      else
        Some(GenericClassType(d,sts.map(x => x.get)))
    }
    case IntersectType(ts) => {
      val sts = ts map safe
      if (sts exists { x=>x.isDefined })
        None
      else
        Some(IntersectType(sts.map(x => x.get)))
    }
    case ArrayType(t) => safe(t) match {
      case Some(t) => Some(ArrayType(t))
      case None => None
    }
  }
}

