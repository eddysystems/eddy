/* Parse: Parser generator for ambiguous grammars
 *
 * Given a possibly ambiguous Grammar, Parse generates Java code which parses
 * the grammar and runs all actions.  Ambiguity is isolated where possible
 * using embedded Scored instances.
 */

package ambiguity

import ambiguity.Grammar._
import utility.Utility._

object Parse {
  // Code generation combinators
  type Code = List[String]
  def indent(c: Code) = c.map(s => if (s.isEmpty) "" else "  "+s)

  def block(start: String, body: Code, brace: Boolean = true, alwaysBrace: Boolean = false): Code = {
    if (brace && (alwaysBrace || body.size > 1))
      start+(if (start.isEmpty) "{" else " {") :: indent(body) ::: List("}")
    else
      start :: indent(body)
  }

  def blocked(body: Code): Code =
    if (body.size <= 1) body else block("",body)

  def ifs(conds: List[String], body: Code): Code =
    if (conds.isEmpty) blocked(body) else block(s"if (${conds mkString " && "})",body)

  def function(name: String, params: List[(String,Type)], result: String, body: Code): Code =
    block(s"def $name(${params map {case (n,t) => s"$n: $t"} mkString ", "}): $result =",body)

  def method(ret: String, name: String, params: List[(String,Type)], body: Code): Code =
    block(s"$ret${if (ret=="") "" else " "}$name(${params map {case (v,t) => s"$t $v"} mkString ", "})",
      body,alwaysBrace=true)

  def separate(cs: List[Code]): Code =
   intersperse(List(""),cs).flatten

  private def actionName(s: Symbol, p: Prod)(implicit G: Grammar): String = {
    val i = G.prods(s).toList.indexOf(p)
    s"$s$i"
  }

  // Convert a Scala type to Java
  sealed abstract class Mode
  case object Cast extends Mode
  case object Box extends Mode
  case object Unbox extends Mode
  def jty(s: Symbol, mode: Mode)(implicit G: Grammar): String = {
    def basic(t: String, mode: Mode) = {
      def prim(b: String, p: String) = mode match {
        case Cast|Box => b
        case Unbox => p
      }
      t.replaceAllLiterally("[","<")
       .replaceAllLiterally("]",">")
       .replaceAll("""\bInt\b""",prim("Integer","int"))
       .replaceAll("""\bBoolean\b""",prim("java.lang.Boolean","boolean"))
    }
    def tup(ts: List[List[Char]]): List[Char] = {
      val T = s"Tuple${ts.size}"
      (mode match {
        case Cast => T
        case Box|Unbox => s"$T<${ts.reverse map (t => basic(t.reverse mkString "",Box)) mkString ","}>"
      }).toList
    }
    def fix(t: List[Char]): List[Char] = t match {
      case Nil => Nil
      case '[' :: _ if mode == Cast => Nil // Make all casts unchecked
      case '(' :: t =>
        val (ts,r) = tuple(Nil,Nil,t)
        tup(ts) ::: fix(r)
      case  c :: t => c :: fix(t)
    }
    def tuple(ws: List[List[Char]], w: List[Char], t: List[Char]): (List[List[Char]],List[Char]) = t match {
      case Nil => impossible
      case ')' :: t => (w::ws,t)
      case ',' :: t => tuple(w::ws,Nil,t)
      case '(' :: t =>
        val (ts,r) = tuple(Nil,Nil,t)
        tuple(ws,tup(ts).reverse:::w,r)
      case c :: t => tuple(ws,c::w,t)
    }
    basic(fix(G.ty(s).toList) mkString "",mode)
  }

  // Which arguments does this action need?
  def needsI(a: Action, i: Int, r: Boolean): Boolean =
    ("""\$"""+(i+1)+(if (r) "r" else "")+"""(?!\d)""").r.findFirstMatchIn(a).nonEmpty
  def needsR(a: Action): Boolean =
    """\$r\b""".r.findFirstMatchIn(a).nonEmpty

  // Actions are Scala
  def actionGen(implicit G: Grammar): Code = {
    def action(s: Symbol, p: Prod): Code = p match { case (ss,a) =>
      val vs = ss.zipWithIndex map {case (s,i) => if (G.isSimple(s)) "?" else s"x${i+1}"}
      val vsr = ss.zipWithIndex map {case (s,i) => if (!G.isToken(s)) "?" else s"x${i+1}r"}
      def args(vs: List[String], r: Boolean): List[String] = (vs,ss).zipped.toList.zipWithIndex collect {
        case ((v,s),i) if (if (r) G.isToken(s) else !G.isSimple(s)) && needsI(a,i,r) => if (r) s"$v: Range" else s"$v: ${G.ty(s)}"
      }
      val ps = (args(vs,r=false) ::: args(vsr,r=true) ::: (if (needsR(a)) List("r: Range") else Nil)) mkString ", "
      val act = """\$((\d+)(r?)|r\b)""".r.replaceAllIn(a, m => {
        val g = m.group(1)
        if (m.group(1) == "r") "r" else {
          val i = m.group(2).toInt-1
          val v = if (m.group(3)=="r") vsr else vs
          if (0<=i && i<v.length && v(i)!="?") v(i)
          else throw new RuntimeException(s"Action references bad variable ${m.group(0)}:"
            +s"\n  $s -> ${ss mkString " "}\n  a = $a\n  vs = ${vs mkString " "}\n  vsr = ${vsr mkString " "}")
        }
      })
      List(s"def ${actionName(s,p)}($ps): ${G.ty(s)} = $act")
    }
    val actions = G.prods.toList flatMap { case (s,ps) => if (G.isSimple(s)) Nil else ps.toList map (action(s,_)) }
    val ranges = List("type Range = Long",
                      "@inline private implicit def convert(r: Range): SRange = new SRange(r)",
                      "")
    val extra = List("import utility.Locations._",
                     "import scala.language.implicitConversions")
    ("// Autogenerated by ambiguity.  DO NOT EDIT!" :: G.preamble ::: extra ::: ""
      :: block(s"object ${G.name}Actions",ranges ::: actions.flatten))
  }

  // Convert a preamble line to Java
  def javaPreamble(p: String): String = p.replaceAllLiterally("_","*")+";"

  def parseGen(G: Grammar, nop: Boolean = false): Code = {
    implicit val _G = G
    val toks = (for ((n,ps) <- G.prods; (p,a) <- ps; t <- p if G.isToken(t)) yield t).toSet
    val nons = sortNons(G)
    assert(toks.size < 255,s"toks.size = ${toks.size}")
    val idBits = 8
    val posBits = 12
    assert(idBits+2*posBits==32)
    val valueBits = 32
    def lo(i: Int) = if (i == 0) "lo" else s"lo+$i"
    def j(i: Int) = if (i == 0) "j" else s"j+$i"
    def hi(i: Int) = if (i == 0) "hi" else s"hi-$i"
    val debug = false

    // Preamble
    lazy val extra = "// Internal imports" :: (List(
      "scala.collection.immutable.*",
      "gnu.trove.TIntLongHashMap",
      "java.util.ArrayList",
      "scala.Tuple2",
      "scala.Tuple3",
      "scala.Tuple4",
      "scala.Option",
      "tarski.Scores.*",
      "utility.Interrupts",
      "static utility.Locations.*",
      "static tarski.ParseEddyActions.*"
    ) map (i => s"import $i;"))
    lazy val preamble = "// Autogenerated by ambiguity: DO NOT EDIT!" :: (G.preamble map javaPreamble)

    // Helper class
    lazy val parser = block(s"private static final class Parser",
      separate(List(fields,init,toplevel,ids,types,nulls,nonnulls) ::: (nons map nonnull)))

    lazy val fields = List(
      s"${G.token}[] input;",
      s"long[] ranges;",
      s"byte[] type;",
      s"ArrayList<Object> values = new ArrayList<Object>();",
      s"TIntLongHashMap slices = new TIntLongHashMap(); // (non,lo,hi) => (start,size)",
      s"static long vMask = (1L<<$valueBits)-1;"
    ) map (f => s"private final $f")

    lazy val init: Code = "// Convert input and allocate working memory" ::
      method("","Parser",List("_input" -> s"List<Loc<${G.token}>>"),List(
        s"final int n = _input.size();",
        s"assert n+1<(1<<$posBits);",
        s"input = new ${G.token}[n];",
        s"ranges = new long[n];",
        s"for (int i=0;i<n;i++) {",
        s"  final Loc<${G.token}> tr = _input.head();",
        s"  input[i] = tr.x();",
        s"  ranges[i] = tr.raw();",
        s"  _input = (List<Loc<${G.token}>>)_input.tail();",
        s"}",
        s"type = new byte[n];"
      ))

    lazy val toplevel: Code = "// The entire parse" :: {
      val ty = jty(G.start,Box)
      method(s"List<$ty>","toplevel",Nil,{
        s"// Parse" ::
        s"types();" ::
        s"nulls();" ::
        s"nonnulls();" ::
        s"" ::
        s"// All done!" ::
        s"final long s = slices.get(${slice(G.start,"0","input.length")});" ::
        ("List<"+ty+"> xs = (List)Nil$.MODULE$;") ::
        loop(G.start,"k","s",v => List(
          "xs = $colon$colon$.MODULE$.<"+ty+">apply("+(v ::: List("xs")).mkString(",")+");")) :::
        "return xs;" :: Nil
      })
    }

    def id(s: Symbol) = s"i$s"
    lazy val ids = "// Symbol ids" :: ((toks.toList.zipWithIndex++nons.zipWithIndex) map {case (s,i) =>
      s"private static final int ${id(s)} = $i;"
    })

    lazy val types: Code = "// Determine token types" ::
      method("private void","types",Nil,
        "final int n = input.length;" ::
        block("for (int i=0;i<n;i++)", s"final ${G.token} t = input[i];" :: "type[i] = (byte)(" ::
          (toks.toList.zipWithIndex map {case (t,i) =>
            s"  ${if (i==0) " " else ":"} t instanceof $t${if (G.isSimple(t)) "$" else ""} ? ${id(t)}"
          }) ::: List("  : 255);")
        )
      )

    def slice(n: Symbol, lo: String, hi: String): String = {
      val slo = if (lo == "0") "" else s"|$lo<<$posBits"
      val shi = if (hi == "0") "" else s"|$hi"
      s"${id(n)}<<${2*posBits}$slo$shi"
    }

    def act(n: Symbol, prod: Prod, args: List[String], locs: List[String], range: Option[String]): String = {
      assert(!G.isSimple(n))
      val (ss,a) = prod
      val needs = ss.zipWithIndex collect { case (s,i) if !G.isSimple(s) => needsI(a,i,r=false) }
      val needsL = ss.zipWithIndex collect { case (s,i) if G.isToken(s) => needsI(a,i,r=true) }
      assert(args.size == needs.size)
      assert(locs.size == needsL.size, s"locs size mismatch:\n  n = $n\n  prod = $prod\n"
                                      +s"  args = ${args mkString ", "}\n  locs = ${locs mkString ", "}")
      val as = (args,needs).zipped.toList collect { case (x,true) => x }
      val ls = (locs,needsL).zipped.toList collect { case (x,true) => x }
      val r = if (!needsR(prod._2)) Nil else range match {
        case Some(r) => List(r)
        case None => throw new RuntimeException(s"Bad action: $n -> ${prod._1 mkString " "} : ${prod._2}, no range available")
      }
      s"${actionName(n,prod)}(${(as ++ ls ++ r) mkString ","})"
    }

    def cast(s: Symbol, v: String): String =
      s"(${jty(s,Cast)})$v"

    lazy val nulls: Code = "// Parse null productions" ::
      method("private void","nulls",Nil,
        s"final int n = input.length;" ::
        s"long slice;" ::
        s"int next = values.size();" ::
        (nons collect {case n if G.nullable(n) => {
          def ifNull(s: Symbol): Option[Option[String]] = if (!G.nullable(s)) None else Some(
            if (G.isSimple(s)) None
            else Some(cast(s,s"values.get((int)(slices.get(${slice(s,"0","0")})>>$valueBits))")))
          val make: Code =
            if (G.isSimple(n)) List(s"// $n is simple")
            else G.prods(n).toList.flatMap {case prod@(p,_) => allSome(p map ifNull) map (vs => {
              val comment = s"$n -> ${if (prod._1.isEmpty) "\"\"" else prod._1 mkString " "} : ${prod._2}"
              s"values.add(${act(n,prod,vs.flatten,Nil,None)}); // $comment"
            })}
          val (s,set): (String,Code) =
            if (G.isSimple(n)) ("1",Nil)
            else ("slice",List(s"slice = (long)next<<$valueBits|${make.size}; next += ${make.size};"))
          val fill: Code = List(s"for (int i=0;i<=n;i++) slices.put(${slice(n,"i","i")},$s);")
          make ::: set ::: fill
        }}).flatten
      )

    def loop(n: Symbol, k: String, s: String, body: List[String] => Code): Code =
      if (G.isSimple(n)) { assert(!G.isToken(n)); body(Nil) }
      else block(s"for (int $k=0;$k<($s&vMask);$k++)",
             body(List(cast(n,s"values.get((int)($s>>$valueBits)+$k)"))))

    lazy val nonnulls: Code = "// Parse nonnull productions" ::
      method("private void","nonnulls",Nil,
        "final int n = input.length;" ::
        block("for (int lo=n;lo>=0;lo--) for (int hi=lo+1;hi<=n;hi++)",
          "if (Interrupts.pending != 0) Interrupts.checkInterrupts();" ::
          "final long range = unionHelper(ranges[lo],ranges[hi-1]);" ::
          nons.map(n => s"$n(lo,hi,range);")))

    def nonnull(n: Symbol): Code =
      method("private void",n,List("lo" -> "final int","hi" -> "final int", "range" -> "final long"),{
        val r = Some("range")
        def check(ti: (Symbol,String)): String = {
          val (t,i) = ti
          s"type[$i]==${id(t)}"
        }
        def checks(tis: List[(Symbol,String)]) = tis map check
        def get(t: Symbol, i: String) = cast(t,s"input[$i]")
        def gets(tis: List[(Symbol,String)]): List[String] =
          tis collect { case (t,i) if !G.isSimple(t) => get(t,i) }
        def cached(tis: List[(Symbol,String)], body: List[String] => Code): Code = {
          val simple = tis filter {case (t,_) => !G.isSimple(t)}
          if (simple.isEmpty) body(Nil) else {
            val (vs,cs) = (simple.zipWithIndex map {case ((t,i),j) =>
              (s"x$j",s"${jty(t,Unbox)} x$j = ${get(t,i)};")}).unzip
            (cs mkString "; ") :: body(vs)
          }
        }
        def add(prod: Prod, vs: List[List[String]], rs: List[List[String]]): Code =
          if (G.isSimple(n)) List("found = true;")
          else {
            val a = act(n,prod,vs.flatten,rs.flatten,r)
            if (G.isNullable(prod._2)) List(s"final ${jty(n,Box)} x = $a;",
                                            s"if (x != null) values.add(x);")
            else List(s"values.add($a);")
          }
        def parse(prod: Prod, d: Divide): Code = {
          val range = {
            val lo = prod._1.map(G.minSize).sum
            val hi = allSome(prod._1.map(G.maxSize)) map (_.sum)
            hi match {
              case None => s"hi-lo>=$lo"
              case Some(hi) if lo==hi => s"hi-lo==$lo"
              case Some(hi) => s"$lo<=hi-lo && hi-lo<=$hi"
            }
          }
          def is[A](xs: List[(A,String)]*): List[List[String]] = xs.toList map (_ map (x => s"ranges[${x._2}]"))
          d match {
            case (Nil,Nil) => Nil
            case (t0,Nil) =>
              val ti0 = t0.zipWithIndex map {case (t,i) => (t,lo(i))}
              ifs(range :: checks(ti0),add(prod,List(gets(ti0)),is(ti0)))
            case (t0,List((n1,t2))) =>
              val ti0 = t0.zipWithIndex map {case (t,i) => (t,lo(i))}
              val ti2 = t2.zipWithIndex map {case (t,i) => (t,s"hi-${t2.size-i}")}
              ifs(range :: checks(ti0) ::: checks(ti2), {
                s"final long s1 = slices.get(${slice(n1,lo(t0.size),hi(t2.size))});" ::
                block(s"if (s1 != 0)", cached(ti0++ti2,v02 => {
                  val (v0,v2) = v02.splitAt(t0.size)
                  loop(n1,"k","s1",v1 => add(prod,List(v0,v1,v2),is(ti0,ti2)))
                }))
              })
            case (t0,List((n1,t2),(n3,t4))) =>
              val ti0 = t0.zipWithIndex map {case (t,i) => (t,lo(i))}
              val ti4 = t4.zipWithIndex map {case (t,i) => (t,s"hi-${t4.size-i}")}
              ifs(range :: checks(ti0) ::: checks(ti4), cached(ti0++ti4,v04 => {
                val (v0,v4) = v04.splitAt(t0.size)
                block(s"for (int j=${lo(t0.size+G.minSize(n1))};j<=${hi(t2.size+G.minSize(n3)+t4.size)};j++)", {
                  val ti2 = t2.zipWithIndex map {case (t,i) => (t,j(i))}
                  ifs(checks(ti2),{
                    List(s"final long s1 = slices.get(${slice(n1,lo(t0.size),"j")}); if (s1 == 0) continue;",
                         s"final long s3 = slices.get(${slice(n3,j(t2.size),hi(t4.size))}); if (s3 == 0) continue;") :::
                    cached(ti2,v2 => loop(n1,"k1","s1",v1 => loop(n3,"k3","s3",v3 =>
                      add(prod,List(v0,v1,v2,v3,v4),is(ti0,ti2,ti4)))))
                  })
                })
              }))
            case _ => impossible
          }
        }
        val ps = G.prods(n).toList flatMap {case prod@(ss,_) => parse(prod,divide(G,ss))}
        val (start,finish) = {
          val s = slice(n,"lo","hi")
          val dump = if (debug) "System.out.println(lo+\":\"+hi+\" "+n+"\"); " else ""
          if (G.isSimple(n))
            ("boolean found = false;",
             List(s"if (found) { ${dump}slices.put($s,1); }"))
          else {
            val ty = jty(n,Box)
            val start = "final int prev = values.size();"
            val slice = s"if (count != 0) { ${dump}slices.put($s,(long)prev<<$valueBits|count); }"
            if (!G.isScored(ty)) (start,List(
              "final int count = values.size()-prev;",
              slice))
            else (start,
              "int size = values.size();" ::
              "int count = size-prev;" ::
              ifs(List("count > 1"),List(
                s"Scored<$ty> s = (Scored)Empty$$.MODULE$$;",
                s"for (int i=0;i<count;i++)",
                s"  s = new Best<$ty>(Pr.parse(),($ty)values.remove(--size),s);",
                s"values.add(new Scored$ty(s,range));",
                s"count = 1;")) :::
              List(slice))
          }
        }
        start :: ps ::: finish
      })

    lazy val parse: Code = "// Parse a token stream" ::
      method(s"public static List<${jty(G.start,Box)}>","parse",List("input" -> s"List<Loc<${G.token}>>"),List(
        if (nop) "return null;"
        else s"return new Parser(input).toplevel();"
      ))

    lazy val module: Code =
      block(s"class ${G.name}",
        if (nop) parse
        else separate(List(parse,parser)))

    // All together now
    separate(List(preamble,extra,module))
  }
}