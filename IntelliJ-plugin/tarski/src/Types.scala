package tarski

import tarski.AST.{Type => _, ArrayType => _, _}
import tarski.Items._
import tarski.Items.ArrayType
import ambiguity.Utility._

// Properties of types according to the Java spec, without extra intelligence
object Types {

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

  // does this class implement the given interface?
  def implements(cls: ClassType, intf: InterfaceType): Boolean =
    (    cls.implements.contains(intf)
      || cls.implements.exists( isProperSubtype(_, intf) )
      || (cls.base != null && cls.base.isInstanceOf[ClassType] && implements(cls.base.asInstanceOf[ClassType], intf)) )

  // Is lo a subtype of hi?
  def isSubtype(lo: Type, hi: Type): Boolean = lo == hi || isProperSubtype(lo,hi)
  def isProperSubtype(lo: Type, hi: Type): Boolean = (lo,hi) match {
    case _ if lo==hi => false // not proper
    case (NullType,_: RefType) => true // null can be anything
    case (_: RefType, ObjectType) => true // every ref is Object, even interfaces and enums!

    // primitive types are not part of inheritance
    case (_,_:PrimType)|(_:PrimType,_) => false

    // array types are covariant
    case (ArrayType(l), ArrayType(h)) => isProperSubtype(l, h)
    // otherwise, arrays are not a subtype of anything (except ObjectType, above), and there can be no subtypes of arrays
    case (ArrayType(_), _)|(_, ArrayType(_)) => false

    // enums cannot be inherited from, but they inherit from java.lang.Enum (and only that)
    case (_, EnumType(_,_,_)) => false
    case (EnumType(_,_,_), _) => isSubtype(EnumBaseType, hi)

    // lo is a proper subtype of hi if its superclass is a subtype of hi, or it implements (a subinterface of) hi
    case (loi:InterfaceType, hi:InterfaceType) => if (loi.base == null) false else isSubtype(loi.base, hi)
    case (loc:ClassType, _:ClassType) => if (loc.base == null) false else isSubtype(loc.base, hi)
    case (loc:ClassType, hi:InterfaceType) => implements(loc,hi)
    case (_:InterfaceType,_:ClassType) => false

    // leftover RefTypes are not subtypes of anything
    case (_:RefType, _)|(_,_:RefType) => false
  }

  // Properties of reference types
  def isFinal(t: ClassType): Boolean =
    throw new RuntimeException("Not implemented")
  def isParameterized(t: RefType): Boolean = {
    // TODO
    false
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
  def narrowsRefTo(from: RefType, to: RefType): Boolean = isProperSubtype(to,from) || (!isSubtype(from,to) && ((from,to) match {
    case (f: ClassType,t: InterfaceType) if !isFinal(f) && !isParameterized(t) => true
    case (f: InterfaceType,t: ClassType) if !isFinal(t) => true
    case (f: InterfaceType,t: InterfaceType) => true
    // TODO: From Cloneable and java.io.Serializable to any T[]
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
    case (null,_) => false
    case (_,null) => false
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
    case (_:TypeParameterType,_)|(_,_:TypeParameterType) => false // TODO: Handle type parameters correctly
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

  // All supertypes of a reference type
  def supers(t: RefType): Set[RefType] = t match {
    case NullType => throw new RuntimeException("nulltype has infinitely many supertypes")
    case i: InterfaceType => supers(i.base) + i
    case c: ClassType => supers(c.base) ++ (c.implements map supers).flatten + c
    case e: EnumType => Set(e,e.base)
    case a: ArrayType => Set(a,a.base) // TODO: Cloneable, Serializable
    case e: ErrorType => Set(e,e.base)
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
      case (_:TypeParameterType,_)|(_,_:TypeParameterType) => notImplemented
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
}
