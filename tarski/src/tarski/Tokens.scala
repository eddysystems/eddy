package tarski

import tarski.Pretty._
import tarski.Scores._
import utility.Locations._

import scala.annotation.tailrec

object Tokens {

  sealed abstract class Token {
    def show(implicit f: ShowFlags): String
  }
  sealed abstract class SimpleToken extends Token {
    def s: String
    final def show(implicit f: ShowFlags) = s
  }
  sealed abstract class FixedToken(val s: String) extends SimpleToken // Always the same string
  sealed abstract class ToIdentToken extends SimpleToken // Converts to IdentTok before parsing

  // Flags controlling show for fancy tokens
  case class ShowFlags(abbreviate: Boolean, probs: Boolean)
  val abbrevShowFlags = ShowFlags(abbreviate=true,probs=false)
  val fullShowFlags = ShowFlags(abbreviate=false,probs=false)

  // Holes in the grammar
  case object HoleTok extends SimpleToken {
    def s = ""
  }

  // Identifiers
  case class IdentTok(name: String) extends SimpleToken {
    def s = name
  }

  // Whitespace
  sealed abstract class SpaceTok extends Token {
    def s: String
    def show(implicit f: ShowFlags) = if (f.abbreviate) " " else s
  }
  sealed abstract class CommentTok extends SpaceTok
  case class WhitespaceTok(s: String) extends SpaceTok
  case class EOLCommentTok(s: String) extends CommentTok
  case class CCommentTok(s: String) extends CommentTok

  // Keywords: 3.9
  case object AbstractTok extends FixedToken("abstract")
  case object AssertTok extends FixedToken("assert")
  case object BooleanTok extends FixedToken("boolean")
  case object BreakTok extends FixedToken("break")
  case object CaseTok extends FixedToken("case")
  case object CatchTok extends FixedToken("catch")
  case object ClassTok extends FixedToken("class")
  case object ConstTok extends FixedToken("const")
  case object ContinueTok extends FixedToken("continue")
  case object DefaultTok extends FixedToken("default")
  case object DoTok extends FixedToken("do")
  case object ElseTok extends FixedToken("else")
  case object EnumTok extends FixedToken("enum")
  case object ExtendsTok extends FixedToken("extends")
  case object FinalTok extends FixedToken("final")
  case object FinallyTok extends FixedToken("finally")
  case object ForTok extends FixedToken("for")
  case object IfTok extends FixedToken("if")
  case object GotoTok extends FixedToken("goto")
  case object ImplementsTok extends FixedToken("implements")
  case object ImportTok extends FixedToken("import")
  case object InstanceofTok extends FixedToken("instanceof")
  case object InterfaceTok extends FixedToken("interface")
  case object NativeTok extends FixedToken("native")
  case object NewTok extends FixedToken("new")
  case object PackageTok extends FixedToken("package")
  case object PrivateTok extends FixedToken("private")
  case object ProtectedTok extends FixedToken("protected")
  case object PublicTok extends FixedToken("public")
  case object ReturnTok extends FixedToken("return")
  case object StaticTok extends FixedToken("static")
  case object StrictfpTok extends FixedToken("strictfp")
  case object SuperTok extends FixedToken("super")
  case object SwitchTok extends FixedToken("switch")
  case object SynchronizedTok extends FixedToken("synchronized")
  case object ThisTok extends FixedToken("this")
  case object ThrowTok extends FixedToken("throw")
  case object ThrowsTok extends FixedToken("throws")
  case object TransientTok extends FixedToken("transient")
  case object TryTok extends FixedToken("try")
  case object VolatileTok extends FixedToken("volatile")
  case object WhileTok extends FixedToken("while")

  // Keywords that we parse as identifiers
  case object ByteTok   extends ToIdentToken { def s = "byte" }
  case object CharTok   extends ToIdentToken { def s = "char" }
  case object DoubleTok extends ToIdentToken { def s = "double" }
  case object FloatTok  extends ToIdentToken { def s = "float" }
  case object IntTok    extends ToIdentToken { def s = "int" }
  case object LongTok   extends ToIdentToken { def s = "long" }
  case object NullTok   extends ToIdentToken { def s = "null" }
  case object ShortTok  extends ToIdentToken { def s = "short" }
  case object VoidTok   extends ToIdentToken { def s = "void" }

  // Fake keywords.  These are not actual Java reserved words, but they are used in the grammar.
  sealed abstract class FakeToken(show: String) extends FixedToken(show)
  case object ThenTok extends FakeToken("then")
  case object UntilTok extends FakeToken("until")
  case object InTok extends FakeToken("in")

  // Literals: 3.10
  case class IntLitTok(s: String) extends SimpleToken
  case class LongLitTok(s: String) extends SimpleToken
  case class FloatLitTok(s: String) extends SimpleToken
  case class DoubleLitTok(s: String) extends SimpleToken
  case class BoolLitTok(v: Boolean) extends ToIdentToken { def s = if (v) "true" else "false" }
  case class CharLitTok(s: String) extends SimpleToken
  case class StringLitTok(s: String) extends SimpleToken

  // Separators: 3.11
  case object LParenTok extends FixedToken("(")
  case object RParenTok extends FixedToken(")")
  case object LCurlyTok extends FixedToken("{")
  case object RCurlyTok extends FixedToken("}")
  case object LBrackTok extends FixedToken("[")
  case object RBrackTok extends FixedToken("]")
  case object SemiTok extends FixedToken(";")
  case object CommaTok extends FixedToken(",")
  case object DotTok extends FixedToken(".")
  case object EllipsisTok extends FixedToken("...")
  case object AtTok extends FixedToken("@")
  case object ColonColonTok extends FixedToken("::")

  // Operators: 3.12
  case object EqTok extends FixedToken("=")
  case object GtTok extends FixedToken(">")
  case object LtTok extends FixedToken("<")
  case object NotTok extends FixedToken("!")
  case object CompTok extends FixedToken("~")
  case object QuestionTok extends FixedToken("?")
  case object ColonTok extends FixedToken(":")
  case object ArrowTok extends FixedToken("->")
  case object EqEqTok extends FixedToken("==")
  case object GeTok extends FixedToken(">=")
  case object LeTok extends FixedToken("<=")
  case object NeTok extends FixedToken("!=")
  case object AndAndTok extends FixedToken("&&")
  case object OrOrTok extends FixedToken("||")
  case object PlusPlusTok extends FixedToken("++")
  case object MinusMinusTok extends FixedToken("--")
  // Binary
  case object PlusTok extends FixedToken("+")
  case object MinusTok extends FixedToken("-")
  case object MulTok extends FixedToken("*")
  case object DivTok extends FixedToken("/")
  case object AndTok extends FixedToken("&")
  case object OrTok extends FixedToken("|")
  case object XorTok extends FixedToken("^")
  case object ModTok extends FixedToken("%")
  case object LShiftTok extends FixedToken("<<")
  case object RShiftTok extends FixedToken(">>")
  case object UnsignedRShiftTok extends FixedToken(">>>")
  // Binary assign
  case object PlusEqTok extends FixedToken("+=")
  case object MinusEqTok extends FixedToken("-=")
  case object MulEqTok extends FixedToken("*=")
  case object DivEqTok extends FixedToken("/=")
  case object AndEqTok extends FixedToken("&=")
  case object OrEqTok extends FixedToken("|=")
  case object XorEqTok extends FixedToken("^=")
  case object ModEqTok extends FixedToken("%=")
  case object LShiftEqTok extends FixedToken("<<=")
  case object RShiftEqTok extends FixedToken(">>=")
  case object UnsignedRShiftEqTok extends FixedToken(">>>=")

  // In fake, we split >> and >>> into GtToks, with separators in between
  sealed abstract class PhantomTok extends SimpleToken {
    def s = throw new RuntimeException(s"Shouldn't be printing phantom token $this")
  }
  case object RShiftSepTok extends PhantomTok
  case object UnsignedRShiftSepTok extends PhantomTok

  // Uninterpreted statements and code blocks
  abstract class StmtTok extends Token {
    def blocked: Boolean
    def show(implicit f: ShowFlags): String
  }

  def isSpace(t: Token) = t.isInstanceOf[SpaceTok]

  // Convert to a string, adding whitespace between every token
  def showSep[A](x: A)(implicit p: Pretty[A], f: ShowFlags): String =
    tokens(x) map (_.show) mkString " "

  // Convert to a string, adding as little whitespace as possible
  def show[A](x: A)(implicit p: Pretty[A], f: ShowFlags): String = {
    def process(ts: List[Token]): String = ts match {
      case Nil => ""
      case x::Nil => x.show
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
        x.show + (if (safe(x,y)) "" else " ") + process(ys)
    }
    process(tokens(x))
  }

  // Prepare a token stream for parsing.
  // 1. Turn matching identifiers into fake keywords.
  // 2. Turn some keywords into identifiers.
  // 3. Split >> and >>> into >'s and separators.
  // 4. Strip whitespace.
  // 5. Separate out comments to be added back at the end.
  def prepare(ts: List[Located[Token]]): Scored[(List[Located[Token]],Option[EOLCommentTok])] = {
    def expand(ts: List[Located[Token]]): List[Located[Token]] = ts flatMap { case Located(t,r) => (t match {
      case _:SpaceTok => Nil
      case t@IdentTok(s) => List(s match {
        case "then" => ThenTok
        case "until" => UntilTok
        case "in" => InTok
        case _ => t
      })
      case t:ToIdentToken => List(IdentTok(t.s))
      case RShiftTok => List(GtTok,RShiftSepTok,GtTok)
      case UnsignedRShiftTok => List(GtTok,UnsignedRShiftSepTok,GtTok,UnsignedRShiftSepTok,GtTok)
      case t => List(t)
    }) map (Located(_,r))}
    def interior(ts: List[Located[Token]]): Scored[List[Located[Token]]] =
      if (ts exists (_.x.isInstanceOf[CommentTok]))
        fail(s"Token stream contains interior comments:\n  ${ts.reverse mkString "\n  "}")
      else known(expand(ts.reverse))
    ts.reverse match {
      case (Located(c:EOLCommentTok,_)) :: s => interior(s) map ((_,Some(c)))
      case s => interior(s) map ((_,None))
    }
  }
}
