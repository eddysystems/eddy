package tarski

import tarski.Pretty._
import utility.Locations._
import utility.Utility._

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
  case class ShowFlags(abbreviate: Boolean, valid: Boolean)
  val abbrevShowFlags = ShowFlags(abbreviate=true,valid=false)
  val sentinelShowFlags = ShowFlags(abbreviate=true,valid=true)
  val fullShowFlags = ShowFlags(abbreviate=false,valid=true)

  // Sentinel registry for use with ShowFlags
  object ShowFlags {
    private[this] var sentinels: List[(String,String)] = Nil
    def registerSentinel(sentinel: String, value: String): Unit = sentinels = (sentinel,value)::sentinels
    def replaceSentinels(s: String): String = sentinels.foldLeft(s)((s,p) => s.replaceAllLiterally(p._1,p._2))
  }

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
  case class WhitespaceTok(s: String) extends SpaceTok {
    override def toString = s"WhitespaceTok(${escape(s)})"
  }
  case class EOLCommentTok(s: String) extends CommentTok
  case class CCommentTok(s: String) extends CommentTok

  // Non-ASCII character outside identifiers or literals.  Stripped before parsing.
  case class IllegalTok(s: String) extends SimpleToken

  // Keywords: 3.9
  case object AbstractTok extends FixedToken("abstract")
  case object AssertTok extends FixedToken("assert")
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
  case object BooleanTok extends ToIdentToken { def s = "boolean" }
  case object ByteTok    extends ToIdentToken { def s = "byte" }
  case object CharTok    extends ToIdentToken { def s = "char" }
  case object DoubleTok  extends ToIdentToken { def s = "double" }
  case object FloatTok   extends ToIdentToken { def s = "float" }
  case object IntTok     extends ToIdentToken { def s = "int" }
  case object LongTok    extends ToIdentToken { def s = "long" }
  case object NullTok    extends ToIdentToken { def s = "null" }
  case object ShortTok   extends ToIdentToken { def s = "short" }
  case object VoidTok    extends ToIdentToken { def s = "void" }

  // Fake keywords.  These are not actual Java reserved words, but they are used in the grammar.
  sealed trait FakeToken extends Token
  case object ThenTok extends FixedToken("then") with FakeToken
  case object UntilTok extends FixedToken("until") with FakeToken
  case class ElifTok(s: String) extends SimpleToken with FakeToken
  case object InTok extends FixedToken("in") with FakeToken

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
    def isBlock: Boolean
    def show(implicit f: ShowFlags): String
  }

  // Convert to a string, adding whitespace between every token
  def showSep[A](x: A)(implicit p: Pretty[A], f: ShowFlags): String =
    tokens(x) map (_.x.show) mkString " "

  def print(ts: List[Token])(implicit f: ShowFlags): String = ts match {
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
        case (_,SemiTok|CommaTok) => true
        case _ => false
      }
      val rest = print(ys)
      if (f.abbreviate && isSpace(x) && isSpace(y)) rest
      else x.show + (if (safe(x,y)) rest else " "+rest)
  }

  // Convert to a string, adding as little whitespace as possible
  def show[A](x: A)(implicit p: Pretty[A], f: ShowFlags): String =
    print(tokens(x) map (_.x))

  // Prepare a token stream for parsing.
  // 0. Drop whitespace and illegal tokens.
  // 1. Turn matching identifiers into fake keywords.
  // 2. Turn some keywords into identifiers.
  // 3. Split >> and >>> into >'s and separators.
  def prepare(ts: List[Loc[Token]]): List[Loc[Token]] = ts flatMap { case Loc(t,r) => (t match {
    case _:SpaceTok|_:IllegalTok => Nil
    case t@IdentTok(s) => List(s match {
      case "then" => ThenTok
      case "until" => UntilTok
      case "elif"|"elsif"|"elseif" => ElifTok(s)
      case "in" => InTok
      case _ => t
    })
    case t:ToIdentToken => List(IdentTok(t.s))
    case RShiftTok => List(GtTok,RShiftSepTok,GtTok)
    case UnsignedRShiftTok => List(GtTok,UnsignedRShiftSepTok,GtTok,UnsignedRShiftSepTok,GtTok)
    case t => List(t)
  }) map (Loc(_,r))}

  // Check for spaces
  def isSpace(t: Token): Boolean = t.isInstanceOf[SpaceTok]
  def isSpace(t: Loc[Token]): Boolean = isSpace(t.x)

  // Extract spaces from a token stream
  def spaces(ts: List[Loc[Token]]): List[Loc[SpaceTok]] = ts collect { case Loc(s:SpaceTok,r) => Loc(s,r) }

  // Interleave whitespace back into a token stream in the right places
  def insertSpaces(ts: List[Loc[Token]], sp: List[Loc[SpaceTok]]): List[Loc[Token]] = {
    // Replace any SRange.empty occurrences with previous.after or next.before, in that order
    val tsa = ts.filterNot(t => isSpace(t) && t.r.size==0).toArray;
    {
      var default = SRange.unknown
      val is = (0 until tsa.length).toList
      for (i <- is ::: is.reverse) {
        if (tsa(i).r == SRange.empty) tsa(i) = Loc(tsa(i).x,default)
        else default = tsa(i).r
      }
    }
    // Place each space after the last token that is before it
    val spots = tsa.map(List(_)) ++ Array(Nil)
    for (s <- sp.reverse) {
      @tailrec def loop(i: Int): Unit =
        if (i == 0 || tsa(i-1).r.hi <= s.r.lo) spots(i) = s :: spots(i)
        else loop(i-1)
      loop(tsa.length)
    }
    // Make sure there are newlines after line comments
    def newlines(ts: List[Loc[Token]]): List[Loc[Token]] = ts match {
      case (c@Loc(_:EOLCommentTok,_))::(w@Loc(wt:WhitespaceTok,_))::ts if  wt.s.contains('\n') => c :: w :: newlines(ts)
      case (c@Loc(_:EOLCommentTok,r))::ts if ts.nonEmpty => c :: Loc(WhitespaceTok("\n"),r.after) :: newlines(ts)
      case t::ts => t :: newlines(ts)
      case Nil => Nil
    }
    newlines(spots.toList.flatten)
  }
}
