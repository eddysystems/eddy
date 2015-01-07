package tarski

import tarski.Pretty._

/**
 * Created by martin on 11.12.14.
 */
object Tokens {

  sealed abstract class Token
  sealed abstract class FixedToken extends Token // Always the same string
  sealed abstract class ToIdentToken extends FixedToken // Converts to IdentTok before parsing

  // Holes in the grammar
  case object HoleTok extends Token

  // Identifiers
  case class IdentTok(name: String) extends Token

  // Whitespace
  sealed abstract class SpaceTok extends Token
  case class WhitespaceTok(content: String) extends SpaceTok
  case class EOLCommentTok(content: String) extends SpaceTok
  case class CCommentTok(content: String) extends SpaceTok

  // Keywords: 3.9
  case object AbstractTok extends FixedToken
  case object AssertTok extends FixedToken
  case object BooleanTok extends FixedToken
  case object BreakTok extends FixedToken
  case object CaseTok extends FixedToken
  case object CatchTok extends FixedToken
  case object ClassTok extends FixedToken
  case object ConstTok extends FixedToken
  case object ContinueTok extends FixedToken
  case object DefaultTok extends FixedToken
  case object DoTok extends FixedToken
  case object ElseTok extends FixedToken
  case object EnumTok extends FixedToken
  case object ExtendsTok extends FixedToken
  case object FinalTok extends FixedToken
  case object FinallyTok extends FixedToken
  case object ForTok extends FixedToken
  case object IfTok extends FixedToken
  case object GotoTok extends FixedToken
  case object ImplementsTok extends FixedToken
  case object ImportTok extends FixedToken
  case object InstanceofTok extends FixedToken
  case object InterfaceTok extends FixedToken
  case object NativeTok extends FixedToken
  case object NewTok extends FixedToken
  case object PackageTok extends FixedToken
  case object PrivateTok extends FixedToken
  case object ProtectedTok extends FixedToken
  case object PublicTok extends FixedToken
  case object ReturnTok extends FixedToken
  case object StaticTok extends FixedToken
  case object StrictfpTok extends FixedToken
  case object SuperTok extends FixedToken
  case object SwitchTok extends FixedToken
  case object SynchronizedTok extends FixedToken
  case object ThisTok extends FixedToken
  case object ThrowTok extends FixedToken
  case object ThrowsTok extends FixedToken
  case object TransientTok extends FixedToken
  case object TryTok extends FixedToken
  case object VolatileTok extends FixedToken
  case object WhileTok extends FixedToken

  // Keywords that we parse as identifiers
  case object ByteTok   extends ToIdentToken
  case object CharTok   extends ToIdentToken
  case object DoubleTok extends ToIdentToken
  case object FloatTok  extends ToIdentToken
  case object IntTok    extends ToIdentToken
  case object LongTok   extends ToIdentToken
  case object NullTok   extends ToIdentToken
  case object ShortTok  extends ToIdentToken
  case object VoidTok   extends ToIdentToken

  // Fake keywords.  These are not actual Java reserved words, but they are used in the grammar.
  sealed abstract class FakeToken extends FixedToken
  case object ThenTok extends FakeToken
  case object UntilTok extends FakeToken
  case object InTok extends FakeToken

  // Literals: 3.10
  case class IntLitTok(v: String) extends Token
  case class LongLitTok(v: String) extends Token
  case class FloatLitTok(v: String) extends Token
  case class DoubleLitTok(v: String) extends Token
  case class BoolLitTok(v: Boolean) extends ToIdentToken
  case class CharLitTok(v: String) extends Token
  case class StringLitTok(v: String) extends Token

  // Separators: 3.11
  case object LParenTok extends FixedToken
  case object RParenTok extends FixedToken
  case object LCurlyTok extends FixedToken
  case object RCurlyTok extends FixedToken
  case object LBrackTok extends FixedToken
  case object RBrackTok extends FixedToken
  case object SemiTok extends FixedToken
  case object CommaTok extends FixedToken
  case object DotTok extends FixedToken
  case object EllipsisTok extends FixedToken
  case object AtTok extends FixedToken
  case object ColonColonTok extends FixedToken

  // Operators: 3.12
  case object EqTok extends FixedToken
  case object GtTok extends FixedToken
  case object LtTok extends FixedToken
  case object NotTok extends FixedToken
  case object CompTok extends FixedToken
  case object QuestionTok extends FixedToken
  case object ColonTok extends FixedToken
  case object ArrowTok extends FixedToken
  case object EqEqTok extends FixedToken
  case object GeTok extends FixedToken
  case object LeTok extends FixedToken
  case object NeTok extends FixedToken
  case object AndAndTok extends FixedToken
  case object OrOrTok extends FixedToken
  case object PlusPlusTok extends FixedToken
  case object MinusMinusTok extends FixedToken
  // Binary
  case object PlusTok extends FixedToken
  case object MinusTok extends FixedToken
  case object MulTok extends FixedToken
  case object DivTok extends FixedToken
  case object AndTok extends FixedToken
  case object OrTok extends FixedToken
  case object XorTok extends FixedToken
  case object ModTok extends FixedToken
  case object LShiftTok extends FixedToken
  case object RShiftTok extends FixedToken
  case object UnsignedRShiftTok extends FixedToken
  // Binary assign
  case object PlusEqTok extends FixedToken
  case object MinusEqTok extends FixedToken
  case object MulEqTok extends FixedToken
  case object DivEqTok extends FixedToken
  case object AndEqTok extends FixedToken
  case object OrEqTok extends FixedToken
  case object XorEqTok extends FixedToken
  case object ModEqTok extends FixedToken
  case object LShiftEqTok extends FixedToken
  case object RShiftEqTok extends FixedToken
  case object UnsignedRShiftEqTok extends FixedToken

  // In fake, we split >> and >>> into GtToks, with separators in between
  sealed abstract class PhantomTok extends Token
  case object RShiftSepTok extends PhantomTok
  case object UnsignedRShiftSepTok extends PhantomTok

  def isSpace(t: Token) = t.isInstanceOf[SpaceTok]

  def show(t: Token): String = t match {
    case HoleTok => ""
    case IdentTok(s) => s
    case WhitespaceTok(s) => s
    case EOLCommentTok(s) => s
    case CCommentTok(s) => s
    // Keywords
    case AbstractTok => "abstract"
    case AssertTok => "assert"
    case BooleanTok => "boolean"
    case BreakTok => "break"
    case ByteTok => "byte"
    case CaseTok => "case"
    case CatchTok => "catch"
    case CharTok => "char"
    case ClassTok => "class"
    case ConstTok => "const"
    case ContinueTok => "continue"
    case DefaultTok => "default"
    case DoTok => "do"
    case DoubleTok => "double"
    case ElseTok => "else"
    case EnumTok => "enum"
    case ExtendsTok => "extends"
    case FinalTok => "final"
    case FinallyTok => "finally"
    case FloatTok => "float"
    case ForTok => "for"
    case IfTok => "if"
    case GotoTok => "goto"
    case ImplementsTok => "implements"
    case ImportTok => "import"
    case InstanceofTok => "instanceof"
    case IntTok => "int"
    case InterfaceTok => "interface"
    case LongTok => "long"
    case NativeTok => "native"
    case NewTok => "new"
    case PackageTok => "package"
    case PrivateTok => "private"
    case ProtectedTok => "protected"
    case PublicTok => "public"
    case ReturnTok => "return"
    case ShortTok => "short"
    case StaticTok => "static"
    case StrictfpTok => "strictfp"
    case SuperTok => "super"
    case SwitchTok => "switch"
    case SynchronizedTok => "synchronized"
    case ThisTok => "this"
    case ThrowTok => "throw"
    case ThrowsTok => "throws"
    case TransientTok => "transient"
    case TryTok => "try"
    case VoidTok => "void"
    case VolatileTok => "volatile"
    case WhileTok => "while"
    // Fake keywords
    case ThenTok => "then"
    case UntilTok => "until"
    case InTok => "in"
    // Literals
    case IntLitTok(v) => v
    case LongLitTok(v) => v
    case FloatLitTok(v) => v
    case DoubleLitTok(v) => v
    case BoolLitTok(true) => "true"
    case BoolLitTok(false) => "false"
    case CharLitTok(v) => v
    case StringLitTok(v) => v
    case NullTok => "null"
    // Separators
    case LParenTok => "("
    case RParenTok => ")"
    case LCurlyTok => "{"
    case RCurlyTok => "}"
    case LBrackTok => "["
    case RBrackTok => "]"
    case SemiTok => ";"
    case CommaTok => ","
    case DotTok => "."
    case EllipsisTok => "..."
    case AtTok => "@"
    case ColonColonTok => "::"
    // Operators
    case EqTok => "="
    case GtTok => ">"
    case LtTok => "<"
    case NotTok => "!"
    case CompTok => "~"
    case QuestionTok => "?"
    case ColonTok => ":"
    case ArrowTok => "->"
    case EqEqTok => "=="
    case GeTok => ">="
    case LeTok => "<="
    case NeTok => "!="
    case AndAndTok => "&&"
    case OrOrTok => "||"
    case PlusPlusTok => "++"
    case MinusMinusTok => "--"
    // Binary
    case PlusTok => "+"
    case MinusTok => "-"
    case MulTok => "*"
    case DivTok => "/"
    case AndTok => "&"
    case OrTok => "|"
    case XorTok => "^"
    case ModTok => "%"
    case LShiftTok => "<<"
    case RShiftTok => ">>"
    case UnsignedRShiftTok => ">>>"
    // Binary assign
    case PlusEqTok => "+="
    case MinusEqTok => "-="
    case MulEqTok => "*="
    case DivEqTok => "/="
    case AndEqTok => "&="
    case OrEqTok => "|="
    case XorEqTok => "^="
    case ModEqTok => "%="
    case LShiftEqTok => "<<="
    case RShiftEqTok => ">>="
    case UnsignedRShiftEqTok => ">>>="
    // Phantoms
    case _:PhantomTok => throw new RuntimeException(s"Shouldn't be printing phantom token $t")
  }

  // Convert to a string, adding whitespace between every token
  def showSep[A](x: A)(implicit p: Pretty[A]): String =
    tokens(x) map show mkString " "

  // Convert to a string, adding as little whitespace as possible
  def show[A](x: A)(implicit p: Pretty[A]): String = {
    def process(ts: List[Token]): String = ts match {
      case Nil => ""
      case x::Nil => show(x)
      case x::(ys@(y::_)) =>
        def safe(x: Token, y: Token) = (x,y) match {
          case (_:WhitespaceTok,_)|(_,_:WhitespaceTok) => true
          case (LParenTok|LBrackTok,_) => true
          case (_,RParenTok|RBrackTok) => true
          case (_:IdentTok,LParenTok|LBrackTok) => true
          case (_:IdentTok|QuestionTok,LtTok|GtTok)|(LtTok|GtTok,_:IdentTok|QuestionTok) => true
          case (GtTok,GtTok|LParenTok) => true
          case (_,DotTok)|(DotTok,_) => true
          case (_,SemiTok) => true
          case _ => false
        }
        show(x) + (if (safe(x,y)) "" else " ") + process(ys)
    }
    process(tokens(x))
  }

  // Prepare a token stream for parsing.
  // 1. Turn matching identifiers into fake keywords.
  // 2. Turn some keywords into identifiers.
  // 3. Split >> and >>> into >'s and separators.
  // 4. Strip whitespace.
  def prepare(ts: List[Token]): List[Token] = ts flatMap {
    case _:SpaceTok => Nil
    case t@IdentTok(s) => List(s match {
      case "then" => ThenTok
      case "until" => UntilTok
      case "in" => InTok
      case _ => t
    })
    case t:ToIdentToken => List(IdentTok(show(t)))
    case RShiftTok => List(GtTok,RShiftSepTok,GtTok)
    case UnsignedRShiftTok => List(GtTok,UnsignedRShiftSepTok,GtTok,UnsignedRShiftSepTok,GtTok)
    case t => List(t)
  }
}
