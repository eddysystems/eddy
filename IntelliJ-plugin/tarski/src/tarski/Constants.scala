package tarski

import tarski.Denotations._
import tarski.Operators._
import tarski.Types._

/**
 * Created by martin on 11.12.14.
 */
object Constants {
  sealed abstract class OptionCon
  sealed abstract class Con extends OptionCon {
    def isStr: Boolean
    def toStr: String
  }
  sealed abstract class PrimCon extends Con {
    def ty: PrimType
    def isStr = false
  }
  sealed abstract class NumCon extends PrimCon {
    // Promotion
    def pro: ProCon
    // Conversions
    def toChar: Char
    def toByte: Byte
    def toShort: Short
    def toInt: Int
    def toLong: Long
    def toFloat: Float
    def toDouble: Double
  }
  sealed trait IntegralCon extends NumCon {
    def pro: ProIntegralCon
  }
  sealed trait SmallCon extends IntegralCon {
    def fitsByte: Boolean
    def fitsShort: Boolean
    def fitsChar: Boolean
  }
  sealed abstract class ProCon extends NumCon { // Promoted numeric types (int and up)
    def neg: ProCon
    def *(y: this.type): ProCon
    def /(y: this.type): ProCon
    def %(y: this.type): ProCon
    def +(y: this.type): ProCon
    def -(y: this.type): ProCon
    def cmp(y: this.type): Int
  }
  sealed trait ProIntegralCon extends ProCon with IntegralCon {
    def comp: ProIntegralCon
    def <<(s: Int): ProIntegralCon
    def >>(s: Int): ProIntegralCon
    def >>>(s: Int): ProIntegralCon
    def &(y: this.type): ProIntegralCon
    def ^(y: this.type): ProIntegralCon
    def |(y: this.type): ProIntegralCon
  }
  case object NotCon extends OptionCon
  case class StringCon(v: String) extends Con {
    def ty = StringType
    def isStr = true
    def toStr = v
  }
  case class BooleanCon(v: Boolean) extends PrimCon {
    def ty = BooleanType
    def toStr = v.toString
  }
  case class CharCon(v: Char) extends IntegralCon with SmallCon {
    def ty = CharType
    def toStr = v.toString
    def pro = IntCon(v.toInt)
    def toChar = v
    def toByte = v.toByte
    def toShort = v.toShort
    def toInt = v.toInt
    def toLong = v.toLong
    def toFloat = v.toFloat
    def toDouble = v.toDouble
    def fitsByte = v==toByte
    def fitsShort = v==toShort
    def fitsChar = true
  }
  case class ByteCon(v: Byte) extends IntegralCon with SmallCon {
    def ty = ByteType
    def toStr = v.toString
    def pro = IntCon(v.toInt)
    def toChar = v.toChar
    def toByte = v
    def toShort = v.toShort
    def toInt = v.toInt
    def toLong = v.toLong
    def toFloat = v.toFloat
    def toDouble = v.toDouble
    def fitsByte = true
    def fitsShort = true
    def fitsChar = true
  }
  case class ShortCon(v: Short) extends IntegralCon with SmallCon {
    def ty = ShortType
    def toStr = v.toString
    def pro = IntCon(v.toInt)
    def toChar = v.toChar
    def toByte = v.toByte
    def toShort = v
    def toInt = v.toInt
    def toLong = v.toLong
    def toFloat = v.toFloat
    def toDouble = v.toDouble
    def fitsByte = v==toByte
    def fitsShort = true
    def fitsChar = true
  }
  case class IntCon(v: Int) extends ProIntegralCon with SmallCon {
    def ty = IntType
    def toStr = v.toString
    def pro = this
    def toChar = v.toChar
    def toByte = v.toByte
    def toShort = v.toShort
    def toInt = v
    def toLong = v.toLong
    def toFloat = v.toFloat
    def toDouble = v.toDouble
    def fitsByte = v==toByte
    def fitsShort = v==toShort
    def fitsChar = v==toChar
    def neg = IntCon(-v)
    def comp = IntCon(~v)
    def *(y: this.type) = IntCon(v*y.v)
    def /(y: this.type) = IntCon(v/y.v)
    def %(y: this.type) = IntCon(v%y.v)
    def +(y: this.type) = IntCon(v+y.v)
    def -(y: this.type) = IntCon(v-y.v)
    def <<(s: Int) = IntCon(v<<s)
    def >>(s: Int) = IntCon(v>>s)
    def >>>(s: Int) = IntCon(v>>>s)
    def &(y: this.type) = IntCon(v&y.v)
    def ^(y: this.type) = IntCon(v^y.v)
    def |(y: this.type) = IntCon(v|y.v)
    def cmp(y: this.type) = v compare y.v
  }
  case class LongCon(v: Long) extends ProIntegralCon {
    def ty = LongType
    def toStr = v.toString
    def pro = this
    def toChar = v.toChar
    def toByte = v.toByte
    def toShort = v.toShort
    def toInt = v.toInt
    def toLong = v
    def toFloat = v.toFloat
    def toDouble = v.toDouble
    def neg = LongCon(-v)
    def comp = LongCon(~v)
    def *(y: this.type) = LongCon(v*y.v)
    def /(y: this.type) = LongCon(v/y.v)
    def %(y: this.type) = LongCon(v%y.v)
    def +(y: this.type) = LongCon(v+y.v)
    def -(y: this.type) = LongCon(v-y.v)
    def <<(s: Int) = LongCon(v<<s)
    def >>(s: Int) = LongCon(v>>s)
    def >>>(s: Int) = LongCon(v>>>s)
    def &(y: this.type) = LongCon(v&y.v)
    def ^(y: this.type) = LongCon(v^y.v)
    def |(y: this.type) = LongCon(v|y.v)
    def cmp(y: this.type) = v compare y.v
  }
  case class FloatCon(v: Float) extends ProCon {
    def ty = FloatType
    def toStr = v.toString
    def pro = this
    def toChar = v.toChar
    def toByte = v.toByte
    def toShort = v.toShort
    def toInt = v.toInt
    def toLong = v.toLong
    def toFloat = v
    def toDouble = v.toDouble
    def neg = FloatCon(-v)
    def *(y: this.type) = FloatCon(v*y.v)
    def /(y: this.type) = FloatCon(v/y.v)
    def %(y: this.type) = FloatCon(v%y.v)
    def +(y: this.type) = FloatCon(v+y.v)
    def -(y: this.type) = FloatCon(v-y.v)
    def cmp(y: this.type) = v compare y.v
  }
  case class DoubleCon(v: Double) extends ProCon {
    def ty = DoubleType
    def toStr = v.toString
    def pro = this
    def toChar = v.toChar
    def toByte = v.toByte
    def toShort = v.toShort
    def toInt = v.toInt
    def toLong = v.toLong
    def toFloat = v.toFloat
    def toDouble = v
    def neg = DoubleCon(-v)
    def *(y: this.type) = DoubleCon(v*y.v)
    def /(y: this.type) = DoubleCon(v/y.v)
    def %(y: this.type) = DoubleCon(v%y.v)
    def +(y: this.type) = DoubleCon(v+y.v)
    def -(y: this.type) = DoubleCon(v-y.v)
    def cmp(y: this.type) = v compare y.v
  }

  private def to(e: Exp, f: Con => OptionCon): OptionCon = toCon(e) match {
    case NotCon => NotCon
    case c:Con => f(c)
  }
  private def toPrim(e: Exp, f: PrimCon => OptionCon): OptionCon = toCon(e) match {
    case c: PrimCon => f(c)
    case _ => NotCon
  }
  private def error: Nothing = throw new RuntimeException("error in constant propagation")

  def promote(x: NumCon, y: NumCon): (ProCon,ProCon) = (x,y) match {
    case _ if x.ty == y.ty => (x.pro,y.pro)
    case (x:DoubleCon,_) => (x,DoubleCon(y.toDouble))
    case (_,y:DoubleCon) => (DoubleCon(x.toDouble),y)
    case (x:FloatCon,_) => (x,FloatCon(y.toFloat))
    case (_,y:FloatCon) => (FloatCon(x.toFloat),y)
    case (x:LongCon,_) => (x,LongCon(y.toLong))
    case (_,y:LongCon) => (LongCon(x.toLong),y)
    case _ => (x.pro,y.pro)
  }

  def toCon(e: Exp): OptionCon = e match {
    // Literals
    case StringLit(v,_) => StringCon(v)
    case BooleanLit(v) => BooleanCon(v)
    case CharLit(v,_) => CharCon(v)
    case ByteLit(v,_) => ByteCon(v)
    case ShortLit(v,_) => ShortCon(v)
    case IntLit(v,_) => IntCon(v)
    case LongLit(v,_) => LongCon(v)
    case FloatLit(v,_) => FloatCon(v)
    case DoubleLit(v,_) => DoubleCon(v)
    case NullLit => NotCon
    // Possibly constant environment values
    case LocalVariableExp(_)|EnumConstantExp(_,_)|StaticFieldExp(_,_) => error // TODO: Handle these
    // Expressions which might be constant
    case CastExp(t,e) => t match {
      case StringType => toCon(e) match {
        case e:StringCon => e
        case _ => NotCon
      }
      case t: PrimType => toPrim(e,e => (t,e) match {
        case _ if t == e.ty => e
        case (BooleanType,_)|(_,BooleanCon(_)) => NotCon
        case (t:NumType,e:NumCon) => t match {
          case ByteType => ByteCon(e.toByte)
          case ShortType => ShortCon(e.toShort)
          case CharType => CharCon(e.toChar)
          case IntType => IntCon(e.toInt)
          case LongType => LongCon(e.toLong)
          case FloatType => FloatCon(e.toFloat)
          case DoubleType => DoubleCon(e.toDouble)
        }
      })
      case _ => NotCon
    }
    case NonImpExp(op,e) => toPrim(e,e => op match {
      case PosOp => e match {
        case e:NumCon => e
        case _ => error
      }
      case NegOp => e match {
        case e:NumCon => e.pro.neg
        case _ => error
      }
      case CompOp => e match {
        case e:IntegralCon => e.pro.comp
        case _ => error
      }
      case NotOp => e match {
        case BooleanCon(v) => BooleanCon(!v)
        case _ => error
      }
    })
    case BinaryExp(op,x,y) => to(x,x => to(y,y => {
      op match {
        case AddOp if x.isStr || y.isStr => StringCon(x.toStr+y.toStr)
        case MulOp|DivOp|ModOp|AddOp|SubOp => (x,y) match {
          case (x:NumCon,y:NumCon) => {
            val (a,bb) = promote(x,y)
            val b = bb.asInstanceOf[a.type]
            op match {
              case MulOp => a*b
              case DivOp => a/b
              case ModOp => a%b
              case AddOp => a+b
              case SubOp => a-b
              case _ => error
            }
          }
          case _ => error
        }
        case LShiftOp|RShiftOp|UnsignedRShiftOp => (x,y) match {
          case (x:IntegralCon,y:IntegralCon) => {
            val n = x.pro
            val s = y.toInt
            op match {
              case LShiftOp => n << s
              case RShiftOp => n >> s
              case UnsignedRShiftOp => n >>> s
              case _ => error
            }
          }
          case _ => error
        }
        case LtOp|GtOp|GeOp|LeOp => BooleanCon({
          val (a,b) = promote(x.asInstanceOf[NumCon],y.asInstanceOf[NumCon])
          val c = a cmp b.asInstanceOf[a.type]
          op match {
            case LtOp => c < 0
            case GtOp => c > 0
            case LeOp => c <= 0
            case GeOp => c >= 0
            case _ => error
          }
        })
        case EqOp|NeOp => BooleanCon((op==NeOp) != ((x,y) match {
          case (BooleanCon(x),BooleanCon(y)) => x == y
          case (x:NumCon,y:NumCon) => { val (a,b) = promote(x,y); a == b }
          case _ => error
        }))
        case AndOp|XorOp|OrOp => (x,y) match {
          case (x:IntegralCon,y:IntegralCon) => {
            val (a,bb) = (x,y) match {
              case (x:LongCon,y) => (x,LongCon(y.toLong))
              case (x,y:LongCon) => (LongCon(x.toLong),y)
              case _ => (x.pro,y.pro)
            }
            val b = bb.asInstanceOf[a.type]
            op match {
              case AndOp => a&b
              case XorOp => a^b
              case OrOp  => a|b
              case _ => error
            }
          }
          case _ => error
        }
        case AndAndOp|OrOrOp => (x,y) match {
          case (BooleanCon(x),BooleanCon(y)) => BooleanCon(if (op==AndAndOp) x && y else x || y)
          case _ => error
        }
      }
    }))
    case CondExp(c,x,y,r) => toPrim(c,c => to(x,x => to(y,y => c match {
      case BooleanCon(c) => r match {
        case StringType => StringCon((if (c) x else y).toStr)
        case BooleanType => if (c) x else y
        case _:NumType => { val (a,b) = promote(x.asInstanceOf[NumCon],y.asInstanceOf[NumCon]); if (c) a else b }
        case _ => error
      }
      case _ => error
    })))
    case ParenExp(e) => toCon(e)
    // Everything else is definitely nonconstant
    case _ => NotCon
  }

  // Is a narrowing conversion safe?
  def constantFits(e: Exp, t: PrimType): Boolean =
    (t==ByteType || t==ShortType || t==CharType) && (toCon(e) match {
      case e: SmallCon => t match {
        case ByteType => e.fitsByte
        case ShortType => e.fitsShort
        case CharType => e.fitsChar
        case _ => false
      }
      case _ => false
    })
}
