package tarski

object Tokens {

  sealed abstract class Token

  case class IdentTok(name: String) extends Token

  case class WhiteSpaceTok() extends Token
  case class EOLCommentTok(content: String) extends Token
  case class CCommentTok(content: String) extends Token

  // Keywords: 3.9
  case class AbstractTok() extends Token
  case class AssertTok() extends Token
  case class BooleanTok() extends Token
  case class BreakTok() extends Token
  case class ByteTok() extends Token
  case class CaseTok() extends Token
  case class CatchTok() extends Token
  case class CharTok() extends Token
  case class ClassTok() extends Token
  case class ConstTok() extends Token
  case class ContinueTok() extends Token
  case class DefaultTok() extends Token
  case class DoTok() extends Token
  case class DoubleTok() extends Token
  case class ElseTok() extends Token
  case class EnumTok() extends Token
  case class ExtendsTok() extends Token
  case class FinalTok() extends Token
  case class FinallyTok() extends Token
  case class FloatTok() extends Token
  case class ForTok() extends Token
  case class IfTok() extends Token
  case class GotoTok() extends Token
  case class ImplementsTok() extends Token
  case class ImportTok() extends Token
  case class InstanceofTok() extends Token
  case class IntTok() extends Token
  case class InterfaceTok() extends Token
  case class LongTok() extends Token
  case class NativeTok() extends Token
  case class NewTok() extends Token
  case class PackageTok() extends Token
  case class PrivateTok() extends Token
  case class ProtectedTok() extends Token
  case class PublicTok() extends Token
  case class ReturnTok() extends Token
  case class ShortTok() extends Token
  case class StaticTok() extends Token
  case class StrictfpTok() extends Token
  case class SuperTok() extends Token
  case class SwitchTok() extends Token
  case class SynchronizedTok() extends Token
  case class ThisTok() extends Token
  case class ThrowTok() extends Token
  case class ThrowsTok() extends Token
  case class TransientTok() extends Token
  case class TryTok() extends Token
  case class VoidTok() extends Token
  case class VolatileTok() extends Token
  case class WhileTok() extends Token

  // Literals: 3.10
  case class IntLitTok(v: String) extends Token
  case class LongLitTok(v: String) extends Token
  case class FloatLitTok(v: String) extends Token
  case class DoubleLitTok(v: String) extends Token
  case class BoolLitTok(v: Boolean) extends Token
  case class CharLitTok(v: String) extends Token
  case class StringLitTok(v: String) extends Token
  case class NullLitTok() extends Token

  // Separators: 3.11
  case class LParenTok() extends Token
  case class RParenTok() extends Token
  case class LCurlyTok() extends Token
  case class RCurlyTok() extends Token
  case class LBrackTok() extends Token
  case class RBrackTok() extends Token
  case class SemiTok() extends Token
  case class CommaTok() extends Token
  case class DotTok() extends Token
  case class EllipsisTok() extends Token
  case class AtTok() extends Token
  case class ColonColonTok() extends Token

  // Operators: 3.12
  case class EqTok() extends Token
  case class GtTok() extends Token
  case class LtTok() extends Token
  case class NotTok() extends Token
  case class CompTok() extends Token
  case class QuestionTok() extends Token
  case class ColonTok() extends Token
  case class ArrowTok() extends Token
  case class EqEqTok() extends Token
  case class GeTok() extends Token
  case class LeTok() extends Token
  case class NeTok() extends Token
  case class AndAndTok() extends Token
  case class OrOrTok() extends Token
  case class PlusPlusTok() extends Token
  case class MinusMinusTok() extends Token
  // Binary
  case class PlusTok() extends Token
  case class MinusTok() extends Token
  case class MulTok() extends Token
  case class DivTok() extends Token
  case class AndTok() extends Token
  case class OrTok() extends Token
  case class XorTok() extends Token
  case class ModTok() extends Token
  case class LShiftTok() extends Token
  case class RShiftTok() extends Token
  case class UnsignedRShiftTok() extends Token
  // Binary assign
  case class PlusEqTok() extends Token
  case class MinusEqTok() extends Token
  case class MulEqTok() extends Token
  case class DivEqTok() extends Token
  case class AndEqTok() extends Token
  case class OrEqTok() extends Token
  case class XorEqTok() extends Token
  case class ModEqTok() extends Token
  case class LShiftEqTok() extends Token
  case class RShiftEqTok() extends Token
  case class UnsignedRShiftEqTok() extends Token

  def isSpace(t: Token) = t match {
    case WhiteSpaceTok()  => true
    case EOLCommentTok(_) => true
    case CCommentTok(_)   => true
    case _                => false
  }
}
