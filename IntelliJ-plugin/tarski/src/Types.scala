package tarski

import tarski.AST._
import tarski.Items._
import tarski.Base._
import ambiguity.Utility._

// Properties of types according to the Java spec, without extra intelligence
object Types {
  // Types
  sealed abstract class Type extends scala.Serializable
  case object VoidType extends Type

  // Primitive types
  sealed abstract class PrimType extends Type
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
  sealed abstract class InterfaceType(val d: InterfaceItem) extends RefType {
    def bases: List[InterfaceType]
  }
  sealed abstract class ClassType(val d: ClassItem) extends ClassOrObjectType {
    def base: ClassOrObjectType
    def implements: List[InterfaceType]
  }
  case object NullType extends RefType
  case object ObjectType extends ClassOrObjectType
  case class ErrorType(name: Name) extends RefType
  case class SimpleInterfaceType(override val d: InterfaceItem) extends InterfaceType(d) {
    def bases = d.bases
  }
  case class GenericInterfaceType(override val d: InterfaceItem, a: List[RefType]) extends InterfaceType(d) {
    implicit def tenv = (d.params,a).zipped.toMap
    def bases = d.bases map substitute
  }
  case class SimpleClassType(override val d: ClassItem) extends ClassType(d) {
    def base = d.base
    def implements = d.implements
  }
  case class GenericClassType(override val d: ClassItem, a: List[RefType]) extends ClassType(d) {
    implicit def tenv = (d.params,a).zipped.toMap
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
    case InstanceofOp() => throw new RuntimeException("instanceof is special since the RHS is a type")
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

  // Substitute type parameters in a type
  def substitute(t: RefType)(implicit tenv: Map[TypeParamItem,RefType]): RefType = t match {
    case ParamType(v) => tenv get v getOrElse t
    case i: InterfaceType => substitute(i)
    case c: ClassOrObjectType => substitute(c)
    case ArrayType(x:RefType) => substitute(x)
    case IntersectType(xs) => IntersectType(xs map substitute)
    case NullType|_:ErrorType|ArrayType(VoidType|_:PrimType) => t
  }
  def substitute(t: InterfaceType)(implicit tenv: Map[TypeParamItem,RefType]): InterfaceType = t match {
    case SimpleInterfaceType(_) => t
    case GenericInterfaceType(d,xs) => GenericInterfaceType(d,xs map substitute)
  }
  def substitute(t: ClassOrObjectType)(implicit tenv: Map[TypeParamItem,RefType]): ClassOrObjectType = t match {
    case ObjectType|SimpleClassType(_) => t
    case GenericClassType(d,xs) => GenericClassType(d,xs map substitute)
  }

  // Turn a TypeItem into a type
  // TODO: Handle generics
  def toType(i: TypeItem): Type =
    if (i.params.nonEmpty)
      notImplemented
    else i match {
      case x: InterfaceItem => SimpleInterfaceType(x)
      case x: ClassItem => SimpleClassType(x)
      case ObjectItem => ObjectType
    }

  // if a type has an associated item, return it
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
  def isProperSubtype(lo: Type, hi: Type): Boolean = (lo,hi) match {
    case _ if lo==hi => false // Not proper
    case (NullType,_: RefType) => true // null can be anything
    case (_: RefType, ObjectType) => true // Every ref is Object, even interfaces and enums!

    // Primitive types are not part of inheritance
    case (_,_:PrimType)|(_:PrimType,_) => false

    // Array types are covariant
    case (ArrayType(l), ArrayType(h)) => isProperSubtype(l, h)
    // Otherwise, arrays are not a subtype of anything (except ObjectType, above), and there can be no subtypes of arrays
    // TODO: Actually, arrays are Cloneable and Serializable
    case (ArrayType(_), _)|(_, ArrayType(_)) => false

    // lo is a proper subtype of hi if its superclass is a subtype of hi, or it implements (a subinterface of) hi
    case (lo:InterfaceType, hi:InterfaceType) => lo.bases exists (isSubtype(_,hi))
    case (lo:ClassType, hi:ClassType) => isSubtype(lo.base,hi)
    case (lo:ClassType, hi:InterfaceType) => implements(lo,hi)
    case (_:InterfaceType,_:ClassType) => false

    // leftover RefTypes are not subtypes of anything
    case (_:RefType, _)|(_,_:RefType) => false
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
    case (_:ParamType,_)|(_,_:ParamType) => notImplemented // TODO: Handle type parameters correctly
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
      case (_:ErrorType,_)|(_,_:ErrorType) => notImplemented
      case (_:ParamType,_)|(_,_:ParamType) => notImplemented
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

  def dimensions(t: Type): Int = t match {
    case ArrayType(t) => 1+dimensions(t)
    case _ => 0
  }
}
