package tarski

import java.util.regex.Pattern

import Tokens._
import ambiguity.Utility._

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Lexer {
  private val (pattern,factories): (Regex,List[(Int,String => Token)]) = {
    // Fixed tokens
    val fixed: List[() => Token] = List(
      // Keywords
      AbstractTok,AssertTok,BooleanTok,BreakTok,ByteTok,CaseTok,CatchTok,CharTok,ClassTok,ConstTok,ContinueTok,
      DefaultTok,DoTok,DoubleTok,ElseTok,EnumTok,ExtendsTok,FinalTok,FinallyTok,FloatTok,ForTok,IfTok,GotoTok,
      ImplementsTok,ImportTok,InstanceofTok,IntTok,InterfaceTok,LongTok,NativeTok,NewTok,PackageTok,PrivateTok,
      ProtectedTok,PublicTok,ReturnTok,ShortTok,StaticTok,StrictfpTok,SuperTok,SwitchTok,SynchronizedTok,ThisTok,
      ThrowTok,ThrowsTok,TransientTok,TryTok,VoidTok,VolatileTok,WhileTok,
      // Literals and separators
      NullLitTok,() => BoolLitTok(true),() => BoolLitTok(false),LParenTok,RParenTok,LCurlyTok,RCurlyTok,LBrackTok,
      RBrackTok,SemiTok,CommaTok,DotTok,EllipsisTok,AtTok,ColonColonTok,
      // Operators
      EqTok,GtTok,LtTok,NotTok,CompTok,QuestionTok,ColonTok,ArrowTok,EqEqTok,GeTok,LeTok,NeTok,AndAndTok,OrOrTok,
      PlusPlusTok,MinusMinusTok,PlusTok,MinusTok,MulTok,DivTok,AndTok,OrTok,XorTok,ModTok,LShiftTok,RShiftTok,
      UnsignedRShiftTok,PlusEqTok,MinusEqTok,MulEqTok,DivEqTok,AndEqTok,OrEqTok,XorEqTok,ModEqTok,LShiftEqTok,
      RShiftEqTok,UnsignedRShiftEqTok)
    val fixedMap = fixed.map(t => (show(t()),t())).toMap

    // Regular expressions to factories
    val subs = List(
      // Digits
      "$ds" -> """(?:\d(?:|[_\d]*\d))""",
      "$bs" -> """(?:[01](?:|[_01]*[01]))""",
      "$hs" -> """(?:[\da-fA-F](?:|[_\da-fA-F]*[\da-fA-F]))""",
      // Float support
      "$e"  -> """(?:[eE][+-]?$ds)""",
      "$p"  -> """(?:[pP][+-]?$ds)""",
      "$f"  -> """[fFdD]""",
      // Escape sequences
      "$esc" -> """(?:\\[btnfr"'\\]|\\[0-7]+)""")
    def close(s: String) =
      if (s.count('('==_) == s.count(')'==_)) s
      else throw new RuntimeException("unclosed: "+escape(s))
    val patterns: List[(String,String => Token)] = List(
      fixedMap.keys.map(Pattern.quote).mkString("|") -> fixedMap,
      """\s+"""    -> WhitespaceTok, // 3.6
      """[a-zA-Z$][\w$]*""" -> IdentTok, // 3.8
      """(?:\d|[1-9]_*$ds|0[xX]$hs|0[0-7_]*[0-7]|0[bB]$bs)[lL]?""" -> IntLitTok, // 3.10.1, TODO: LongLitTok
      """$ds\.$ds?$e?$f?|\.$ds$e?$f?|$ds(?:$e$f?|$e?$f)|0[xX](?:$hs\.|$hs?\.$hs)$p$f?""" -> FloatLitTok, // 3.10.2, TODO: DoubleLitTok
      """'(?:[^'\\]|$esc)'""" -> CharLitTok, // 3.10.4
      """"(?:[^"\\]|$esc)*"""" -> StringLitTok) // 3.10.5

    // Build master pattern and factory map
    def sub(s: String) = subs.foldRight(s) {(p,s) => close(close(s).replaceAllLiterally(p._1,close(p._2)))}
    // Require end of string to ensure longest match
    val pattern = ("^(?:"+patterns.map(p => "("+sub(p._1)+")").mkString("|")+")$").r
    val factories = patterns.zipWithIndex.map {case ((p,f),i) => (i+1,f)}
    (pattern,factories)
  }

  def lex(input: String): List[Token] = {
    // TODO: Handle comments
    if ("""//|/\*""".r.findFirstIn(input).isDefined)
      throw new RuntimeException("Not implemented: comments, input "+escape(input))
    def loop(s: String, ts: List[Token]): List[Token] = if (s.isEmpty) ts else {
      def longest(n: Int, best: Option[Match]): Option[Match] = {
        if (n > s.length) best
        else longest(n+1,pattern.findPrefixMatchOf(s.substring(0,n)) orElse best)
      }
      longest(1,None) match {
        case None => throw new RuntimeException(
          "Scan failed, column "+(input.length-s.length+1)+": "+escape(input)+", "+escape(s)+", "+ts.reverse)
        case Some(m) => (for ((i,f) <- factories; g = m.group(i); if g != null) yield (s.substring(g.length),f(g))) match {
          case List((r,t)) => loop(r,t::ts)
          case ts => throw new RuntimeException("string "+escape(s)+" matches as ambiguous token list "+ts)
        }
      }
    }
    loop(input,Nil).reverse
  }
}
