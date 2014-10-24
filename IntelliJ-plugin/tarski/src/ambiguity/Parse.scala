package ambiguity
import ambiguity.Grammar._
import ambiguity.Utility._
import scala.collection.mutable

object Parse {
  /*
  def parse(G: Grammar, action: String => Action, input: Array[Tok]): List[Result] = {
    val nons = G.prods.keys.toList

    // Parse using dumb bottom up dynamic programming
    // parses(lo,hi,non): [value]
    val parses = mutable.Map[(Int,Int,Symbol),List[Any]]()
    val n = input.length
    // Fill in tokens
    for (lo <- 0 until n) {
      val (t,v) = input(lo)
      parses((lo,lo+1,t)) = List(v)
    }
    // Fill in nonterminals
    for (len <- 0 to n;
         lo <- 0 to n;
         non <- nons;
         (prod,act) <- G.prods(non)) {
      val hi = lo+len
      val a = action(act)
      parses((lo,hi,non)) = prod match {
        case Nil =>
          if (len==0)
            List(a(Nil))
          else Nil
        case List(s) =>
          parses((lo,hi,s)).map(x => a(List(x)))
        case List(s0,s1) =>
          (for (mid <- lo to hi;
               p0 <- parses((lo,mid,s0));
               p1 <- parses((mid,hi,s1)))
            yield a(List(p0,p1))).toList
      }
    }

    // Extract results
    parses((0,n,G.start))
  }
  */

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
    val p = params.map{case (n,t) => s"$n: $t"}
    block(s"def $name($params): $result",body)
  }

  def parseGen(name: String, G: Grammar): Code = {
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
    function(name, List(("input",s"Seq[${G.token}]")), G.types(G.start),
          "// Functions for matching tokens"
      ::  "type R = (Int,Int)"
      ::  "val array = input.toArray"
      ::  "def tok(r: R: if (r._2-r._1==1) Some(array[r._1]) else None"
      ::  toks.toList.map {t => s"def P_$t(r: R) = tok(r) match { case Some(t: $t) => List(t); case _ => Nil }"}
      ::: ""
      ::  "// Allocate one sparse array per nonterminal"
      ::  nons.toList.map(n => s"val P_$n = Map[R,List[${G.types(n)}]]()")
      ::: ""
      ::  "// Parse bottom up for each nonterminal"
      ::  "val n = input.length"
      ::  "// Parse null productions"
      ::  block("for (lo <- 0 to n)",
            for (non <- nons)
              yield s"P_$non(lo,lo) = List(" + G.prods(non).flatMap {case (p,a) => p match {
                case Nil => Some(act(a,Nil))
                case _ => None
              }}.mkString(",") + ")"
          )
      ::: "// Parse nonnull productions"
      ::  block("for (lo <- n to 0 by -1; hi <- lo to n)",
               "def two[A,B](pa: R => List[A], pb: R => List[A], f: (A,B) => C): List[C] ="
            :: "  for (m <- lo to hi; a <- pa(lo,m); b <- pb(m,hi)) yield f(a,b)"
            :: (for (non <- nons)
                 yield s"P_$non(lo,hi) = " + (G.prods(non).flatMap {case (p,a) => p match {
                   case Nil => None
                   case List(s) => Some(s"P_$s(lo,hi).map(${act(a,List("_"))})".replaceAllLiterally(".map(_)",""))
                   case List(s0,s1) => Some(s"two(P_$s0,P_$s1,${act(a,List("_1","_2"))})")
                   case _ => throw new RuntimeException("nonbinarized grammar")
                 }} mkString(" ::: "))))
      ::: ""
      ::  "// All done!"
      ::  s"P_${G.start}(0,n)"
      ::  Nil
    )
  }
}