package tarski

import java.util.regex.Pattern
import utility.Locations._
import utility.Utility._
import tarski.Tokens._
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Lexer {
  private val (pattern,factories): (Regex,List[(Int,String => Token)]) = {
    // Fixed tokens
    val fixed: List[SimpleToken] = List(
      // Keywords
      AbstractTok,AssertTok,BooleanTok,BreakTok,ByteTok,CaseTok,CatchTok,CharTok,ClassTok,ConstTok,ContinueTok,
      DefaultTok,DoTok,DoubleTok,ElseTok,EnumTok,ExtendsTok,FinalTok,FinallyTok,FloatTok,ForTok,IfTok,GotoTok,
      ImplementsTok,ImportTok,InstanceofTok,IntTok,InterfaceTok,LongTok,NativeTok,NewTok,PackageTok,PrivateTok,
      ProtectedTok,PublicTok,ReturnTok,ShortTok,StaticTok,StrictfpTok,SuperTok,SwitchTok,SynchronizedTok,ThisTok,
      ThrowTok,ThrowsTok,TransientTok,TryTok,VoidTok,VolatileTok,WhileTok,
      // Literals and separators
      NullTok,BoolLitTok(true),BoolLitTok(false),LParenTok,RParenTok,LCurlyTok,RCurlyTok,LBrackTok,
      RBrackTok,SemiTok,CommaTok,DotTok,EllipsisTok,AtTok,ColonColonTok,
      // Operators
      EqTok,GtTok,LtTok,NotTok,CompTok,QuestionTok,ColonTok,ArrowTok,EqEqTok,GeTok,LeTok,NeTok,AndAndTok,OrOrTok,
      PlusPlusTok,MinusMinusTok,PlusTok,MinusTok,MulTok,DivTok,AndTok,OrTok,XorTok,ModTok,LShiftTok,RShiftTok,
      UnsignedRShiftTok,PlusEqTok,MinusEqTok,MulEqTok,DivEqTok,AndEqTok,OrEqTok,XorEqTok,ModEqTok,LShiftEqTok,
      RShiftEqTok,UnsignedRShiftEqTok)
    val fixedMap = fixed.map(t => (t.s,t)).toMap

    // Regular expressions to factories
    val subs = List(
      // Digits
      "$ds"  -> """(?:\d(?:|[_\d]*\d))""",
      "$bs"  -> """(?:[01](?:|[_01]*[01]))""",
      "$hs"  -> """(?:[\da-fA-F](?:|[_\da-fA-F]*[\da-fA-F]))""",
      "$int" -> """(?:\d|[1-9]_*$ds|0[xX]$hs|0[0-7_]*[0-7]|0[bB]$bs)""",
      // Float support
      "$e"   -> """(?:[eE][+-]?$ds)""",
      "$p"   -> """(?:[pP][+-]?$ds)""",
      "$ff"  -> """[fF]""",
      "$of"  -> """[fF]""",
      "$fd"  -> """[dD]""",
      "$od"  -> """[dD]?""",
      // Escape sequences
      "$esc" -> """(?:\\[btnfr"'\\]|\\[0-7]+)""")
    val float = """$ds\.$ds?$e?$of|\.$ds$e?$of|$ds(?:$e$of|$e?$ff)|0[xX](?:$hs\.|$hs?\.$hs)$p$of"""
    def close(s: String) =
      if (s.count('('==_) == s.count(')'==_)) s
      else throw new RuntimeException("unclosed: "+escape(s))
    val patterns: List[(String,String => Token)] = List(
      """//[^\n]*""" -> EOLCommentTok,
      fixedMap.keys.map(Pattern.quote).mkString("|") -> fixedMap,
      """\s+"""    -> WhitespaceTok, // 3.6
      """[a-zA-Z$][\w$]*""" -> IdentTok, // 3.8
      """$int""" -> IntLitTok, // 3.10.1
      """$int[lL]""" -> LongLitTok, // 3.10.1
      float -> FloatLitTok, // 3.10.2
      float.replaceAllLiterally("$ff","$fd").replaceAllLiterally("$of","$od") -> DoubleLitTok, // 3.10.2
      """'(?:[^'\\]|$esc)'""" -> CharLitTok, // 3.10.4
      """"(?:[^"\\]|$esc)*"""" -> StringLitTok) // 3.10.5

    // Build master pattern and factory map
    def sub(s: String) = subs.foldRight(s) {(p,s) => close(close(s).replaceAllLiterally(p._1,close(p._2)))}
    // Require end of string to ensure longest match
    val pattern = ("^(?:"+patterns.map(p => "("+sub(p._1)+")").mkString("|")+")$").r
    val factories = patterns.zipWithIndex.map {case ((p,f),i) => (i+1,f)}
    (pattern,factories)
  }

  def lex(input: String): List[Located[Token]] = {
    // TODO: Handle comments
    if ("""/\*""".r.findFirstIn(input).isDefined)
      throw new RuntimeException("Not implemented: multiline comments, input "+escape(input))
    def loop(lo: Int, s: String, ts: List[Located[Token]]): List[Located[Token]] = if (s.isEmpty) ts else {
      def longest(n: Int, best: Option[Match]): Option[Match] = {
        if (n > s.length) best
        else longest(n+1,pattern.findPrefixMatchOf(s.substring(0,n)) orElse best)
      }
      longest(1,None) match {
        case None => throw new RuntimeException(
          "Scan failed, column "+(input.length-s.length+1)+": "+escape(input)+", "+escape(s)+", "+ts.reverse)
        case Some(m) => (for ((i,f) <- factories; g = m.group(i); if g != null) yield (g.length,f(g))) match {
          case List((n,t)) => loop(lo+n,s.substring(n),Located(t,SRange(lo,lo+n))::ts)
          case ts => throw new RuntimeException("string "+escape(s)+" matches as ambiguous token list "+ts)
        }
      }
    }
    loop(0,input,Nil).reverse
  }
}
