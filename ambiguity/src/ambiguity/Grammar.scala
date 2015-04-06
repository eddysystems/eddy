package ambiguity

import utility.Utility
import Utility._
import scala.collection.mutable
import scala.util.matching.Regex
import scala.math.min

object Grammar {

  type Symbol = String
  type Action = String
  type Type = String
  type Prod = (List[Symbol],Action)

  case class Grammar(name: String,
                     preamble: List[String],
                     start: Symbol,
                     token: Type,
                     simple: Regex, // Is a token simple?
                     scored: Regex, // Does a type have a Scored case?
                     types: Map[Symbol,Type],
                     prods: Map[Symbol,Set[Prod]]) {

    def isToken(s: Symbol) = !types.contains(s)
    def isSimple(t: Symbol) = if (isToken(t)) simple.findFirstIn(t).isDefined else types(t) == "Unit"
    def isScored(ty: Type) = scored.findFirstIn(ty).isDefined
    def isNullable(a: Action) = """\bnull\b""".r.findFirstIn(a).isDefined // TODO: This is a hack

    val nullable: Set[Symbol] = {
      lazy val f: Symbol => Boolean = fixpoint(false, s =>
        prods.contains(s) && prods(s).exists(_._1.forall(f)))
      prods.keySet.filter(f)
    }

    lazy val minSize: Symbol => Int = fixpoint(0, s =>
      if (isToken(s)) 1 else prods(s).map({case (ss,_) => (ss map minSize).sum}).min
    )
    val maxSize: Symbol => Option[Int] = {
      val cap = 32
      lazy val loop: Symbol => Int = fixpoint(0, s =>
        if (isToken(s)) 1 else min(cap,prods(s).map({case (ss,_) => (ss map loop).sum}).max))
      def f(s: Symbol) = {
        val n = loop(s)
        if (n >= cap) None else Some(n)
      }
      f
    }

    def ty(s: Symbol): Type =
      if (isSimple(s)) throw new RuntimeException(s"Simple token $s has no useful type")
      else if (isToken(s)) s
      else types(s)

    def modify(types: Map[Symbol,Type],
               prods: Map[Symbol,Set[Prod]]) =
      Grammar(name,preamble,start,token,simple,scored,types,prods)
  }

  // Split a list of symbols by terminals (t) and nonterminals (n)
  type Divide = (List[Symbol],List[(Symbol,List[Symbol])])
  def divide(G: Grammar, ss: List[Symbol]): Divide = {
    val (t,r) = ss span G.isToken
    (t,r match {
      case Nil => Nil
      case n::ss => { val (a,b) = divide(G,ss); (n,a)::b }
    })
  }

  def check(G: Grammar, generic: Boolean = false) = {
    // Nonterminal lists must match
    val typeOnly = G.types.keySet -- G.prods.keySet
    if (typeOnly.size > 0)
      throw new RuntimeException(s"nonterminals $typeOnly have types but not productions")
    val prodOnly = G.prods.keySet -- G.types.keySet
    if (prodOnly.size > 0)
      throw new RuntimeException(s"nonterminals $prodOnly have productions but not types")

    // Hack: nonterminals must not end in Tok
    def isTok(s: Symbol) = s.endsWith("Tok") && s.forall(_.isLetterOrDigit)
    for (s <- G.types.keys)
      if (isTok(s))
        throw new RuntimeException(s"nonterminal $s looks like a token")
    if (!generic)
      for ((s,ps) <- G.prods; (p,a) <- ps; t <- p)
        if (!isTok(t) && !G.types.contains(t))
          throw new RuntimeException(s"production $s -> ${p.mkString(" ")} contains weird symbol $t")

    // Check productions
    def sym(s: Symbol, m: => String) = {
      if (s.isEmpty)
        throw new RuntimeException(s"$m: empty symbol")
      if (!s.forall(c => c.isLetterOrDigit || c=='_' || (generic && "[]".contains(c))))
        throw new RuntimeException(s"$m: invalid symbol $s")
    }
    val empty = Symbol("")
    for ((s,ps) <- G.prods) {
      sym(s,"grammar")
      for ((p,a) <- ps) {
        p foreach {sym(_,s"bad production $s -> $p")}
        def count(c: Char) = a.count(c==_)
        if (   count('(') != count(')')
            || count('[') != count(']')
            || count('{') != count('}'))
          throw new RuntimeException(s"production $s -> $p { $a } has mismatched something")
      }
    }

    // Check cycles
    if (!generic)
      sortNons(G)
  }

  def complete(G: Grammar): Grammar = {
    // Find the generic symbols
    val pat = """^(\w+)\[([\w\[\]]+)\]$""".r
    val generics = (for ((n,ps) <- G.prods if n.contains('[')) yield {
      n match {
        case pat(g,v) => (g,v)
        case _ => throw new RuntimeException(s"bad generic nonterminal: $n")
      }
    }).toMap

    // Close the start
    val types = mutable.Map[Symbol,Type]()
    val prods = mutable.Map[Symbol,Set[Prod]]()
    def ty(s: Symbol): Type = types.getOrElse(s,G.ty(s))
    def close(s: Symbol): Symbol = s match {
      case pat(g,to) =>
        val t = close(to)
        if (G.isSimple(t)) throw new RuntimeException(s"Can't close on simple type: s $s, g $g, to $to, t $t")
        val n = s"${g}_$t"
        if (!types.contains(n)) {
          val v = generics(g)
          def sub(u: Symbol, rep: String) = s"\\b$v\\b".r.replaceAllIn(u,rep)
          var ss = s"$g[$v]"
          types(n) = sub(G.types(ss),ty(t))
          prods(n) = G.prods(ss).map {case (p,a) => (p.map(u => close(sub(u,t))),a)}
        }
        n
      case _ if !G.types.contains(s) || types.contains(s) =>
        s
      case _ =>
        types(s) = G.types(s)
        prods(s) = G.prods(s).map {case (p,a) => (p.map(close),a)}
        s
    }
    close(G.start)
    G.modify(types.toMap,prods.toMap)
  }

  def binarize(G: Grammar): Grammar = {
    def sym(ss: List[Symbol]) = ss mkString "__"
    def split(indent: String, n: Symbol, t: Type, p: List[Symbol], a: Action): (List[(Symbol,Type)],List[(Symbol,Prod)]) = {
      val d = divide(G,p)
      if (d._2.size <= 2)
        (List((n,t)),List((n,(p,a))))
      else {
        // Chop p in half as equitably as possible
        val (p0,p1): (List[Symbol],List[Symbol]) = {
          val (t0,s) = d
          val (s1,s2) = s splitAt (s.size/2)
          val s10 = s1.init
          val (n11,t12) = s1.last
          val (t120,t121) = t12 splitAt (t12.size/2)
          def f(ss: List[(Symbol,List[Symbol])]) = ss flatMap (s => s._1 :: s._2)
          val p0 = t0:::f(s10):::n11::t120
          val p1 = t121:::f(s2)
          assert(p0++p1==p)
          (p0,p1)
        }
        val k = p0.size
        def sub(ii: Int, p: List[(Symbol,Boolean)]): (Symbol,(Int,Boolean)=>String,(List[(Symbol,Type)],List[(Symbol,Prod)])) = p match {
          case List((x,_)) => (x,(i,r)=>if (r) "r" else "",(Nil,Nil))
          case _ =>
            val n = sym(p map (_._1))
            def tup(nil: String, ss: List[String]): String = ss match {
              case Nil => nil
              case List(s) => s
              case _ => s"(${ss mkString ","})"
            }
            val single = p.count(_._2)==1
            val t = tup("Unit",p collect {case (s,true) => if (G.isSimple(s)) "Long"
                                                           else { val t = G.ty(s); if (G.isToken(s)) s"Loc[$t]" else t }})
            val a = tup("()",p.zipWithIndex collect {case ((s,true),i) =>
              val x = "$"+(i+1)
              if (G.isSimple(s)) x+"r"
              else if (G.isToken(s)) s"Loc($x,${x}r)"
              else x
            })
            def fj(j: Int, r: Boolean) = p(j)._1 match {
              case s if !r && G.isSimple(s) => throw new RuntimeException(s"Bad action: s $s, a $a, jr $j$r")
              case _ if single => ""
              case s => val k = "._"+(1+(p take j count (_._2)))
                        if (r && !G.isSimple(s)) k+".r" else k
            }
            (n,fj,split(indent+"  ",n,t,p map (_._1),a))
        }
        def used(di: Int)(si: (Symbol,Int)): (Symbol,Boolean) =
          (si._1,("""\$"""+(si._2+di)+"""(?:r\b|(?!\d))""").r.findFirstIn(a).nonEmpty)
        val (n0,f0,(t0,r0)) = sub(0,p0.zipWithIndex map used(1))
        val (n1,f1,(t1,r1)) = sub(1,p1.zipWithIndex map used(1+p0.size))
        def convert(m: scala.util.matching.Regex.Match): String = {
          val i = m.group(1).toInt
          val r = m.group(2).nonEmpty
          if (i-1 < k) "\\$1"+f0(i-1,r) else "\\$2"+f1(i-k-1,r)
        }
        val a1 = """\$(\d+)(r?)""".r.replaceAllIn(a,convert(_))
        val a2 = """\bLoc\((\$\d+\._\d+),\1\.r\)""".r.replaceAllIn(a1,m => "\\"+m.group(1)) // Terrible hack: Loc($2._3,$2._3.r) -> $2._3
        val r = (n,(List(n0,n1),a2))
        ((n,t)::t0:::t1,r::r0:::r1)
      }
    }
    val info = for ((n,ps) <- G.prods.toList; (p,a) <- ps) yield split("",n,G.types(n),p,a)
    val types = info.map(_._1).flatten.toMap
    val prods = toMapSet(info.map(_._2).flatten)
    G.modify(types,prods)
  }

  // Sort nonterminals: s0 before s1 if s1 =>* s0
  def sortNons(G: Grammar): List[Symbol] = {
    def generates(s: Symbol) = {
      def singles(prod: List[Symbol]): Set[Symbol] = prod match {
        case Nil => Set.empty
        case s :: ss => {
          val s0: Set[Symbol] = if (G.types.contains(s) && ss.forall(G.nullable.contains)) Set(s) else Set.empty
          val s1              = if (G.nullable(s)) singles(ss) else Set.empty
          s0 ++ s1
        }
      }
      G.prods(s).flatMap(pa => singles(pa._1)).toSet
    }
    val nons = G.types.keySet.toList
    val from: Map[Symbol,Set[Symbol]] = toMapSet(for (s <- nons; t <- generates(s)) yield (t,s))
    val degree = mutable.Map() ++ nons.map(s => (s,generates(s).size))
    var work = nons.filter(s => degree(s)==0)
    var sorted: List[Symbol] = Nil
    doWhile(work match {
      case Nil => false
      case s :: ss => {
        work = ss
        sorted = s :: sorted
        for (f <- from get s; t <- f) {
          val d = degree(t)
          if (d==1)
            work = t :: work
          else
            degree(t) = d-1
        }
        true
      }
    })
    if (sorted.length != nons.length) {
      def cycles(bad: List[Symbol], s: Symbol): Unit = {
        if (bad.contains(s))
          throw new RuntimeException(s"Grammar cycle: ${bad.reverse.mkString(" -> ")} -> $s")
        val b = s :: bad
        for (g <- generates(s))
          cycles(b,g)
      }
      for (n <- G.types.keys)
        cycles(Nil,n)
      throw new RuntimeException(s"Cycle not found: nons ${nons.length}, sorted ${sorted.length}")
    }
    sorted.reverse
  }

  def read(file: String): Grammar = {
    var name: Option[Symbol] = None
    var preamble: List[String] = Nil
    var start: Option[Symbol] = None
    var token: Option[Type] = None
    var simple: Option[String] = None
    var scored: Option[String] = None
    var scope: Option[Symbol] = None
    val types = mutable.Map[Symbol,Type]()
    val prods = mutable.Map[Symbol,Set[Prod]]()
    val aliases = mutable.Map[Symbol,Symbol]()

    // Split "prod { action }" into prod,action
    def splitProd(line: String, words: List[String]): Prod = words.reverse match {
      case "}" :: ws => {
        def search(depth: Int, words: List[String], action: List[String]): Prod =
          words match {
            case Nil => throw new RuntimeException("mismatched braces")
            case w :: ws => {
              val d = depth + (w match {
                case "}" => 1
                case "{" => -1
                case _ => 0
              })
              if (d == 0)
                (ws.reverse,action.mkString(" "))
              else
                search(d,ws,w::action)
            }
          }
        search(1,ws,Nil)
      }
      case _ => throw new RuntimeException(s"production line must end with }: $line")
    }

    // Parse file line by line
    for (line <- file.split('\n')) {
      val words : List[String] = splitWhitespace(line.replaceAll("//.*",""))
      words match {
        case Nil => ()
        case _ if !line(0).isWhitespace =>
          scope = None
          words match {
            case s :: (ty@(_::_)) =>
              val t = ty.mkString(" ")
              s match {
                case "name" => name = Some(t)
                case "preamble" => preamble = t :: preamble
                case "start" => start = Some(t)
                case "token" => token = Some(t)
                case "simple" => simple = Some(t)
                case "scored" => scored = Some(t)
                case "alias" => ty match {
                  case List(from,to) => aliases(from) = to
                  case _ => throw new RuntimeException(s"bad alias: '$line'")
                }
                case _ =>
                  types(s) = t
                  scope = Some(s)
              }
            case _ => throw new RuntimeException(s"bad line: '$line'")
          }
        case _ =>
          scope match {
            case None => throw new RuntimeException("indented line before scope")
            case Some(s) =>
              val (p,action) = splitProd(line,words)
              val prod = p match {
                case List("\"\"") => Nil
                case w => w
              }
              prods(s) = prods.getOrElse(s,Set()) + ((prod,action))
          }
      }
    }

    def unpack[A](a: Option[A], e: String) = a match {
      case None => throw new RuntimeException(e)
      case Some(s) => s
    }

    // Remove aliases from a production
    def unaliasSym(s: Symbol): Symbol = aliases.foldLeft(s)((s,av) => {
      val (a,v) = av
      if (s==a) v
      else s.replaceAllLiterally(s"[$a]",s"[$v]")
    })
    def unaliasProd(p: Prod): Prod = (p._1 map unaliasSym,p._2)

    // Nearly done!
    Grammar(unpack(name,"missing name"),
            preamble.reverse,
            unpack(start,"at least one nonterminal required"),
            unpack(token,"token type required"),
            unpack(simple,"simple required").r,
            unpack(scored,"scored required").r,
            types.toMap,
            prods.toMap.mapValues(_ map unaliasProd))
  }
}