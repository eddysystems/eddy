package ambiguity
import ambiguity.Utility._
import scala.collection.mutable

object Grammar {

  type Symbol = String
  type Action = String
  type Type = String
  type Prod = (List[Symbol],Action)

  case class Grammar(name: String,
                     preamble: List[String],
                     start: Symbol,
                     token: Type,
                     types: Map[Symbol,Type],
                     prods: Map[Symbol,List[Prod]]) {
    val nullable: Set[Symbol] = {
      val f = fixpoint(false, (f: Symbol => Boolean, s: Symbol) =>
        prods.contains(s) && prods(s).exists(_._1.forall(f)))
      prods.keySet.filter(f)
    }

    def isToken(s: Symbol) = !types.contains(s)
    def ty(s: Symbol): Type = if (isToken(s)) s else types(s)

    def modify(types: Map[Symbol,Type],
               prods: Map[Symbol,List[Prod]]) =
      Grammar(name,preamble,start,token,types,prods)
  }

  def check(G: Grammar, generic: Boolean = false) = {
    // Nonterminal lists must match
    val typeOnly = G.types.keySet -- G.prods.keySet
    if (typeOnly.size > 0)
      throw new RuntimeException(s"nonterminals $typeOnly have types but not productions")
    val prodOnly = G.prods.keySet -- G.types.keySet
    if (prodOnly.size > 0)
      throw new RuntimeException(s"nonterminals $prodOnly have productions but not types")

    // Hack: types must not contain TypeArgs
    for ((s,t) <- G.types)
      if (t.contains("TypeArgs"))
        throw new RuntimeException(s"type $s: $t looks bad")

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
        def count(c: Char) = a.count(c==)
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
    val pat = """(\w+)\[(\w+)\]""".r
    val generics = (for ((n,ps) <- G.prods if n.contains('[')) yield {
      n match {
        case pat(g,v) => (g,v)
        case _ => throw new RuntimeException(s"bad generic nonterminal: $n")
      }
    }).toMap

    // Close the start
    val types = mutable.Map[Symbol,Type]()
    val prods = mutable.Map[Symbol,List[Prod]]()
    def close(s: Symbol): Symbol = s match {
      case pat(g,t) =>
        var n = s"${g}_$t"
        if (!types.contains(n)) {
          val v = generics(g)
          def sub(u: Symbol, rep: String) = s"\\b$v\\b".r.replaceAllIn(u,rep)
          var ss = s"$g[$v]"
          types(n) = sub(G.types(ss),G.ty(t))
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
    def split(n: Symbol, t: Type, p: List[Symbol], a: Action): (List[(Symbol,Type)],List[(Symbol,Prod)]) = {
      val k = p.length
      if (k <= 2)
        (List((n,t)),List((n,(p,a))))
      else {
        val (p0, p1) = p splitAt k/2
        def sub(p: List[Symbol]): (Symbol,Int=>String,(List[(Symbol,Type)],List[(Symbol,Prod)])) = p match {
          case List(x) => (x,(i=>""),(Nil,Nil))
          case _ =>
            var n = sym(p)
            var t = "(" + p.map(G.ty).mkString(",") + ")"
            var i = p.length
            var a = "(" + (1 to i).map("$"+_).mkString(",") + ")"
            (n,(j=>s"._$j"),split(n,t,p,a))
        }
        val (n0,f0,(t0,r0)) = sub(p0)
        val (n1,f1,(t1,r1)) = sub(p1)
        def convert(m: scala.util.matching.Regex.Match): String = {
          val i = m.group(1).toInt
          if (i-1 < k/2) "\\$1"+f0(i) else "\\$2"+f1(i-k/2)
        }
        val r = (n,(List(n0,n1),"""\$(\d+)""".r.replaceAllIn(a,convert(_))))
        ((n,t)::t0:::t1,r::r0:::r1)
      }
    }
    val info = for ((n,ps) <- G.prods.toList; (p,a) <- ps) yield split(n,G.types(n),p,a)
    val types = info.map(_._1).flatten.toMap
    val prods = toMapList(info.map(_._2).flatten)
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
          throw new RuntimeException(s"grammar cycle: ${bad.reverse.mkString(" -> ")} -> $s")
        val b = s :: bad
        for (g <- generates(s))
          cycles(b,g)
      }
      for (n <- G.types.keys)
        cycles(Nil,n)
      throw new RuntimeException(s"unfound cycle: nons ${nons.length}, sorted ${sorted.length}")
    }
    sorted
  }

  def read(file: String): Grammar = {
    var name: Option[Symbol] = None
    var preamble: List[String] = Nil
    var start: Option[Symbol] = None
    var token: Option[Type] = None
    var scope: Option[Symbol] = None
    val types = mutable.Map[Symbol,Type]()
    val prods = mutable.Map[Symbol,List[(List[Symbol],Action)]]()

    // Split "prod { action }" into prod,action
    def splitProd(words: List[String]): Prod = words.reverse match {
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
      case _ => throw new RuntimeException("production line must end with }")
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
              val (p,action) = splitProd(words)
              val prod = p match {
                case List("\"\"") => Nil
                case w => w
              }
              prods(s) = (prod,action) :: (prods get s getOrElse Nil)
          }
      }
    }

    def unpack[A](a: Option[A], e: String) = a match {
      case None => throw new RuntimeException(e)
      case Some(s) => s
    }

    // Nearly done!
    Grammar(unpack(name,"missing name"),
            preamble.reverse,
            unpack(start,"at least one nonterminal required"),
            unpack(token,"token type required"),
            types.toMap,
            prods.toMap mapValues {_.reverse})
  }
}