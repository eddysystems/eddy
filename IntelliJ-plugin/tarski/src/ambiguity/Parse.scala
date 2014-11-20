package ambiguity
import ambiguity.Grammar._
import ambiguity.Utility._
import scala.collection.mutable

object Parse {
  // Code generation combinators
  type Code = List[String]
  def indent(c: Code) = c.map("  "+_)
  def block(start: String, body: Code, brace: Boolean = true): Code = {
    if (brace)
      start+" {" :: indent(body) ::: List("}")
    else
      start :: indent(body)
  }
  def function(name: String, params: List[(String,String)], result: String, body: Code) = {
    val p = params.map{case (n,t) => s"$n: $t"}.mkString(", ")
    block(s"def $name($p): $result =",body)
  }

  def parseGen(G: Grammar): Code = {
    val toks = (for ((n,ps) <- G.prods; (p,a) <- ps; t <- p if !G.types.contains(t)) yield t).toSet
    val nons = sortNons(G)
    def act(action: Action, xs: List[String]): String = {
      val xa = xs.toArray
      """\$(\d+)""".r.replaceAllIn(action, m => {
        val i = m.group(1).toInt-1
        if (0<=i && i<xs.length)
          xs(i)
        else
          throw new RuntimeException(s"bad action: '$action', xs $xs")
      })
    }
    def P(s: Symbol): String = s"P_$s"
    def kind(s: Symbol) = if (G.isToken(s)) "t" else if (G.nullable(s)) "n" else "s"
    val debug = true
    def ifNull(s: Symbol): Option[String] = if (G.nullable(s)) Some(s"P_$s((lo,lo)).head") else None
    var f =
      function("parse", List(("input",s"List[${G.token}]")), s"List[${G.types(G.start)}]",
            "type R = (Int,Int)"
        ::  "import scala.collection.mutable"
        ::  "val debug = false"
        ::  ""
        ::  "// Functions for matching tokens"
        ::  "val array = input.toArray"
        ::  "def tok(i: Int) = array(i)"
        ::  toks.toList.map {t => s"def P_$t(i: Int) = array(i) match { case t: $t => List(t); case _ => Nil }"}
        ::: ""
        ::  "// Allocate one sparse array per nonterminal"
        ::  nons.toList.map(n => s"val P_$n = mutable.Map[R,List[${G.types(n)}]]()")
        ::: ""
        ::  "// Parse bottom up for each nonterminal"
        ::  "val n = input.length"
        ::  "// Parse null productions"
        ::  block("for (lo <- 0 to n)",
              for (non <- nons)
                yield s"P_$non((lo,lo)) = List(" + G.prods(non).flatMap {case (p,a) =>
                  allSome(p map ifNull) map (act(a,_))
                }.mkString(",") + ")"
            )
        ::: "// Parse nonnull productions"
        ::  block("for (lo <- n to 0 by -1; hi <- lo+1 to n)",
                  "if (debug) println(\"\\nparsing: \"+array.slice(lo,hi).mkString(\" \"))"
              ::  "def d[A](non: String, p: mutable.Map[R,List[A]]) = if (debug) {"
              ::  "  val c = p((lo,hi)); val n = c.size"
              ::  "  if (n>0) println(s\"  $non $n = \"+c.mkString(\" \"))"
              ::  "}"
              ::  "def t[A,C](p: Int => List[A])(f: A => C) = if (lo+1==hi) p(lo).map(f) else Nil"
              ::  "def n[A,C](p: R   => List[A])(f: A => C) = p(lo,hi).map(f)"
              ::  "def s[A,C](p: R   => List[A])(f: A => C) = if (lo<hi) p(lo,hi).map(f) else Nil"
              ::  "def tt[A,B,C](pa: Int => List[A], pb: Int => List[B])(f: (A,B) => C) = if (lo+2==hi) for (a <- pa(lo); b <- pb(lo+1)) yield f(a,b) else Nil"
              ::  "def tn[A,B,C](pa: Int => List[A], pb: R   => List[B])(f: (A,B) => C) = if (lo<hi) for (a <- pa(lo); b <- pb((lo+1,hi))) yield f(a,b) else Nil"
              ::  "def ts[A,B,C](pa: Int => List[A], pb: R   => List[B])(f: (A,B) => C) = if (lo+1<hi) for (a <- pa(lo); b <- pb((lo+1,hi))) yield f(a,b) else Nil"
              ::  "def nt[A,B,C](pa: R   => List[A], pb: Int => List[B])(f: (A,B) => C) = if (lo<hi) for (b <- pb(hi-1); a <- pa((lo,hi-1))) yield f(a,b) else Nil"
              ::  "def st[A,B,C](pa: R   => List[A], pb: Int => List[B])(f: (A,B) => C) = if (lo+1<hi) for (b <- pb(hi-1); a <- pa((lo,hi-1))) yield f(a,b) else Nil"
              ::  "def nn[A,B,C](pa: R   => List[A], pb: R   => List[B])(f: (A,B) => C) = (for (m <- lo to hi; a <- pa((lo,m)); b <- pb((m,hi))) yield f(a,b)).toList"
              ::  "def sn[A,B,C](pa: R   => List[A], pb: R   => List[B])(f: (A,B) => C) = (for (m <- lo+1 to hi; a <- pa((lo,m)); b <- pb((m,hi))) yield f(a,b)).toList"
              ::  "def ns[A,B,C](pa: R   => List[A], pb: R   => List[B])(f: (A,B) => C) = (for (m <- lo to hi-1; a <- pa((lo,m)); b <- pb((m,hi))) yield f(a,b)).toList"
              ::  "def ss[A,B,C](pa: R   => List[A], pb: R   => List[B])(f: (A,B) => C) = (for (m <- lo+1 to hi-1; a <- pa((lo,m)); b <- pb((m,hi))) yield f(a,b)).toList"
              ::  nons.map(non => {
                    val p = s"P_$non((lo,hi)) = " + (G.prods(non).flatMap {case (p,a) => p match {
                        case Nil => None
                        case List(s) => Some(s"${kind(s)}(P_$s)(x => ${act(a,List("x"))})")
                        case List(s0,s1) => Some(s"${kind(s0)}${kind(s1)}(P_$s0,P_$s1)((x,y) => ${act(a,List("x","y"))})")
                        case _ => throw new RuntimeException("nonbinarized grammar")
                      }} mkString(" ::: "))
                    val d = s"""d("$non",P_$non)"""
                    s"$p; $d"}))
        ::: ""
        ::  "// All done!"
        ::  s"P_${G.start}((0,n))"
        ::  Nil
      )
    var c = (
          "// Autogenerated by ambiguity.  DO NOT EDIT!"
      ::  G.preamble
      ::: ""
      ::  block(s"object ${G.name}",f))
    c
  }
}