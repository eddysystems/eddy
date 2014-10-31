package tarski

object Tokens {

  sealed abstract class Token
  sealed abstract class FixedToken extends Token // Always the same string

  // Identifiers
  case class IdentTok(name: String) extends Token

  // Whitespace
  case class WhitespaceTok(content: String) extends Token
  case class EOLCommentTok(content: String) extends Token
  case class CCommentTok(content: String) extends Token

  // Keywords: 3.9
  case class AbstractTok() extends FixedToken
  case class AssertTok() extends FixedToken
  case class BooleanTok() extends FixedToken
  case class BreakTok() extends FixedToken
  case class ByteTok() extends FixedToken
  case class CaseTok() extends FixedToken
  case class CatchTok() extends FixedToken
  case class CharTok() extends FixedToken
  case class ClassTok() extends FixedToken
  case class ConstTok() extends FixedToken
  case class ContinueTok() extends FixedToken
  case class DefaultTok() extends FixedToken
  case class DoTok() extends FixedToken
  case class DoubleTok() extends FixedToken
  case class ElseTok() extends FixedToken
  case class EnumTok() extends FixedToken
  case class ExtendsTok() extends FixedToken
  case class FinalTok() extends FixedToken
  case class FinallyTok() extends FixedToken
  case class FloatTok() extends FixedToken
  case class ForTok() extends FixedToken
  case class IfTok() extends FixedToken
  case class GotoTok() extends FixedToken
  case class ImplementsTok() extends FixedToken
  case class ImportTok() extends FixedToken
  case class InstanceofTok() extends FixedToken
  case class IntTok() extends FixedToken
  case class InterfaceTok() extends FixedToken
  case class LongTok() extends FixedToken
  case class NativeTok() extends FixedToken
  case class NewTok() extends FixedToken
  case class PackageTok() extends FixedToken
  case class PrivateTok() extends FixedToken
  case class ProtectedTok() extends FixedToken
  case class PublicTok() extends FixedToken
  case class ReturnTok() extends FixedToken
  case class ShortTok() extends FixedToken
  case class StaticTok() extends FixedToken
  case class StrictfpTok() extends FixedToken
  case class SuperTok() extends FixedToken
  case class SwitchTok() extends FixedToken
  case class SynchronizedTok() extends FixedToken
  case class ThisTok() extends FixedToken
  case class ThrowTok() extends FixedToken
  case class ThrowsTok() extends FixedToken
  case class TransientTok() extends FixedToken
  case class TryTok() extends FixedToken
  case class VoidTok() extends FixedToken
  case class VolatileTok() extends FixedToken
  case class WhileTok() extends FixedToken

  // Literals: 3.10
  case class IntLitTok(v: String) extends Token
  case class LongLitTok(v: String) extends Token
  case class FloatLitTok(v: String) extends Token
  case class DoubleLitTok(v: String) extends Token
  case class BoolLitTok(v: Boolean) extends Token
  case class CharLitTok(v: String) extends Token
  case class StringLitTok(v: String) extends Token
  case class NullLitTok() extends FixedToken

  // Separators: 3.11
  case class LParenTok() extends FixedToken
  case class RParenTok() extends FixedToken
  case class LCurlyTok() extends FixedToken
  case class RCurlyTok() extends FixedToken
  case class LBrackTok() extends FixedToken
  case class RBrackTok() extends FixedToken
  case class SemiTok() extends FixedToken
  case class CommaTok() extends FixedToken
  case class DotTok() extends FixedToken
  case class EllipsisTok() extends FixedToken
  case class AtTok() extends FixedToken
  case class ColonColonTok() extends FixedToken

  // Operators: 3.12
  case class EqTok() extends FixedToken
  case class GtTok() extends FixedToken
  case class LtTok() extends FixedToken
  case class NotTok() extends FixedToken
  case class CompTok() extends FixedToken
  case class QuestionTok() extends FixedToken
  case class ColonTok() extends FixedToken
  case class ArrowTok() extends FixedToken
  case class EqEqTok() extends FixedToken
  case class GeTok() extends FixedToken
  case class LeTok() extends FixedToken
  case class NeTok() extends FixedToken
  case class AndAndTok() extends FixedToken
  case class OrOrTok() extends FixedToken
  case class PlusPlusTok() extends FixedToken
  case class MinusMinusTok() extends FixedToken
  // Binary
  case class PlusTok() extends FixedToken
  case class MinusTok() extends FixedToken
  case class MulTok() extends FixedToken
  case class DivTok() extends FixedToken
  case class AndTok() extends FixedToken
  case class OrTok() extends FixedToken
  case class XorTok() extends FixedToken
  case class ModTok() extends FixedToken
  case class LShiftTok() extends FixedToken
  case class RShiftTok() extends FixedToken
  case class UnsignedRShiftTok() extends FixedToken
  // Binary assign
  case class PlusEqTok() extends FixedToken
  case class MinusEqTok() extends FixedToken
  case class MulEqTok() extends FixedToken
  case class DivEqTok() extends FixedToken
  case class AndEqTok() extends FixedToken
  case class OrEqTok() extends FixedToken
  case class XorEqTok() extends FixedToken
  case class ModEqTok() extends FixedToken
  case class LShiftEqTok() extends FixedToken
  case class RShiftEqTok() extends FixedToken
  case class UnsignedRShiftEqTok() extends FixedToken

  def isSpace(t: Token) = t match {
    case WhitespaceTok(_)|EOLCommentTok(_)|CCommentTok(_) => true
    case _ => false
  }

  def show(t: Token): String = t match {
    case IdentTok(s) => s
    case WhitespaceTok(s) => s
    case EOLCommentTok(s) => s
    case CCommentTok(s) => s
    // Keywords
    case AbstractTok() => "abstract"
    case AssertTok() => "assert"
    case BooleanTok() => "boolean"
    case BreakTok() => "break"
    case ByteTok() => "byte"
    case CaseTok() => "case"
    case CatchTok() => "catch"
    case CharTok() => "char"
    case ClassTok() => "class"
    case ConstTok() => "const"
    case ContinueTok() => "continue"
    case DefaultTok() => "default"
    case DoTok() => "do"
    case DoubleTok() => "double"
    case ElseTok() => "else"
    case EnumTok() => "enum"
    case ExtendsTok() => "extends"
    case FinalTok() => "final"
    case FinallyTok() => "finally"
    case FloatTok() => "float"
    case ForTok() => "for"
    case IfTok() => "if"
    case GotoTok() => "goto"
    case ImplementsTok() => "implements"
    case ImportTok() => "import"
    case InstanceofTok() => "instanceof"
    case IntTok() => "int"
    case InterfaceTok() => "interface"
    case LongTok() => "long"
    case NativeTok() => "native"
    case NewTok() => "new"
    case PackageTok() => "package"
    case PrivateTok() => "private"
    case ProtectedTok() => "protected"
    case PublicTok() => "public"
    case ReturnTok() => "return"
    case ShortTok() => "short"
    case StaticTok() => "static"
    case StrictfpTok() => "strictfp"
    case SuperTok() => "super"
    case SwitchTok() => "switch"
    case SynchronizedTok() => "synchronized"
    case ThisTok() => "this"
    case ThrowTok() => "throw"
    case ThrowsTok() => "throws"
    case TransientTok() => "transient"
    case TryTok() => "try"
    case VoidTok() => "void"
    case VolatileTok() => "volatile"
    case WhileTok() => "while"
    // Literals
    case IntLitTok(v) => v
    case LongLitTok(v) => v
    case FloatLitTok(v) => v
    case DoubleLitTok(v) => v
    case BoolLitTok(true) => "true"
    case BoolLitTok(false) => "false"
    case CharLitTok(v) => v
    case StringLitTok(v) => v
    case NullLitTok() => "null"
    // Separators
    case LParenTok() => "("
    case RParenTok() => ")"
    case LCurlyTok() => "{"
    case RCurlyTok() => "}"
    case LBrackTok() => "["
    case RBrackTok() => "]"
    case SemiTok() => ";"
    case CommaTok() => ","
    case DotTok() => "."
    case EllipsisTok() => "..."
    case AtTok() => "@"
    case ColonColonTok() => ";"
    // Operators
    case EqTok() => "="
    case GtTok() => ">"
    case LtTok() => "<"
    case NotTok() => "!"
    case CompTok() => "~"
    case QuestionTok() => "?"
    case ColonTok() => ":"
    case ArrowTok() => "->"
    case EqEqTok() => "=="
    case GeTok() => ">="
    case LeTok() => "<="
    case NeTok() => "!="
    case AndAndTok() => "&&"
    case OrOrTok() => "||"
    case PlusPlusTok() => "++"
    case MinusMinusTok() => "--"
    // Binary
    case PlusTok() => "+"
    case MinusTok() => "-"
    case MulTok() => "*"
    case DivTok() => "/"
    case AndTok() => "&"
    case OrTok() => "|"
    case XorTok() => "^"
    case ModTok() => "%"
    case LShiftTok() => "<<"
    case RShiftTok() => ">>"
    case UnsignedRShiftTok() => ">>>"
    // Binary assign
    case PlusEqTok() => "+="
    case MinusEqTok() => "-="
    case MulEqTok() => "*="
    case DivEqTok() => "/="
    case AndEqTok() => "&="
    case OrEqTok() => "|="
    case XorEqTok() => "^="
    case ModEqTok() => "%="
    case LShiftEqTok() => "<<="
    case RShiftEqTok() => ">>="
    case UnsignedRShiftEqTok() => ">>>="
  }

  def show(ts: List[Token]): String =
    ts map show mkString " "
}
