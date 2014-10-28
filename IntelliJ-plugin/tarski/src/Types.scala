package tarski

import tarski.AST.{Type => _, _}
import tarski.Items._

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

  // Unbox to a primitive type if possible
  def unbox(t: Type): Option[PrimType] = t match {
    case p: PrimType => Some(p)
    case c: ClassType => c.qualifiedName match {
      case "java.lang.Boolean"   => Some(BooleanType)
      case "java.lang.Byte"      => Some(ByteType)
      case "java.lang.Short"     => Some(ShortType)
      case "java.lang.Integer"   => Some(IntType)
      case "java.lang.Long"      => Some(LongType)
      case "java.lang.Float"     => Some(FloatType)
      case "java.lang.Double"    => Some(DoubleType)
      case "java.lang.Character" => Some(CharType)
    }
    case _ => None
  }

  // Unbox if necessary to get different classes of primitive types
  def toNumeric(t: Type): Option[PrimType] = unbox(t).filter(isNumeric)
  def toIntegral(t: Type): Option[PrimType] = unbox(t).filter(isIntegral)
  def toBoolean(t: Type): Option[PrimType] = unbox(t).filter(isBoolean)

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
        case (BooleanType,_) if toBoolean(t1).isDefined => true
        case (_,BooleanType) if toBoolean(t0).isDefined => true
        case (_:PrimType,_) if toNumeric(t1).isDefined => true
        case (_,_:PrimType) if toNumeric(t0).isDefined => true
        case _ => castableTo(t0,t1) || castableTo(t1,t0)
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

  // Is lo a subtype of hi?
  def isSubtype(lo: Type, hi: Type): Boolean = lo == hi || {
    throw new RuntimeException("Not implemented: subtype " + lo + " < " + hi)
  }

  // Whether from can be implicitly converted to to
  def convertibleTo(from: Type, to: Type): Boolean = from == to || {
    if (from == null || to == null)
      false
    else
      throw new RuntimeException("Not implemented: convertibleTo " + from + " -> " + to)
  }

  // Whether from can be explicitly cast to to
  def castableTo(from: Type, to: Type): Boolean = from==to || ((from,to) match {
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
}
