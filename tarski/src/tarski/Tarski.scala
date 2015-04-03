package tarski

import tarski.Denotations.Stmt
import tarski.Environment.{LazyEnv, PlaceInfo, Env}
import tarski.Items.Item
import tarski.Levels.LangLevel
import tarski.Scores._
import tarski.JavaScores._
import tarski.Semantics._
import tarski.Tokens._
import tarski.Tries.{Queriable, Trie}
import utility.Interrupts
import utility.Locations._
import utility.JavaUtils.isDebug
import scala.annotation.tailrec
import scala.collection.JavaConverters._

object Tarski {

  def makeTrie(jvalues: java.util.Collection[Item]): Trie[Item] =
    Trie(jvalues.asScala)

  def makeTrie(jvalues: Array[Item]): Trie[Item] =
    Trie(jvalues)

  def environment(trie0: Queriable[Item], trie1: Queriable[Item], byItem: ValueByItemQuery,
                  imports: ImportTrie, scope: java.util.Map[Item,Integer], place: PlaceInfo, level: Int): Env =
    new LazyEnv(trie0, trie1, QueriableItemList.empty, byItem, LangLevel(level),
                imports, scope.asScala.toMap.mapValues(_.intValue), place)

  def print(is: Iterable[Alt[Item]]): Unit = {
    is foreach { case Alt(p,i) =>
      println(s"  $p => " + i)
    }
  }

  // (show(s),s.toString,fullFormat,abbrevFormat) for a list of statements s.
  // Once we convert Stmt to ShowStmts, Env can be discarded.
  case class ShowStmts(ss: List[Stmt], show: String, den: String, full: String, abbrev: String, fullTokens: List[Loc[Token]]) {
    // Compare ignoring locations and whitespace
    def similar(ts: List[Loc[Token]]): Boolean = {
      def strip(ts: List[Loc[Token]]): List[Token] = ts collect { case Loc(t,_) if !isSpace(t) && t != HoleTok => t }
      strip(ts) == strip(fullTokens)
    }
    def similar(ts: JList[Loc[Token]]): Boolean = similar(ts.asScala.toList)
  }

  // Java and Scala result types
  type JList[A] = java.util.List[A]
  type  Results =  List[Alt[ShowStmts]]
  type JResults = JList[Alt[ShowStmts]]

  abstract class Take {
    // Accept some results, returning a probability cutoff for continuing searches.  To stop, return 1.
    def take(rs: JResults): Double
  }

  // Feed results to a take instance until it's satisfied
  def fixTake(tokens: java.util.List[Loc[Token]], env: Env,
              format: (String,ShowFlags) => String, take: Take): Unit = {
    val toks = tokens.asScala.toList
    val r = fix(toks)(env)
    val sp = spaces(toks)

    println("input: " + Tokens.print(toks map (_.x))(abbrevShowFlags))

    // Take elements until we have enough, merging duplicates and adding their probabilities if found
    def mergeTake(s: Scored[ShowStmts], m: Map[String,Alt[ShowStmts]]): Unit = {
      // Check interrupts (as the probabilities decline, we hardly ever do env lookups)
      if (Interrupts.pending != 0) Interrupts.checkInterrupts()
      val cutoff = take.take((m.values.toList sortBy (-_.p)).asJava)
      @tailrec def loop(s: Scored[ShowStmts]): Unit =
        if (s.p <= cutoff)
          println(s"stopping search: p ${s.p} <= cutoff $cutoff")
        else s match {
          case s:LazyScored[ShowStmts] => loop(s.force(cutoff))
          case _:EmptyOrBad => ()
          case Best(p,b,s) =>
            val a = b.abbrev
            println(s"found in stream: $p: $a")
            mergeTake(s, if (m contains a) m else m + ((a,Alt(p,b))))
        }
      loop(s)
    }

    val sc = r map (ss => {
      val abbrev   = show(ss)(Pretty.prettyStmts,abbrevShowFlags).trim
      val sentinel = show(ss)(Pretty.prettyStmts,sentinelShowFlags).trim
      val tokens   = insertSpaces(Pretty.tokens(ss)(Pretty.prettyStmts),sp)
      val full     = Tokens.print(tokens map (_.x))(fullShowFlags)
      ShowStmts(ss=ss,show=abbrev,den=ss.toString,
        full=format(full,fullShowFlags),
        abbrev=ShowFlags.replaceSentinels(format(sentinel,sentinelShowFlags)).replaceAll("""\s+"""," "),
        fullTokens=tokens)
    })
    // Complain if there's an error
    if (trackErrors) sc.strict match {
      case e:EmptyOrBad => println("fixJava failed:\n"+e.error.prefixed("  error: "))
      case _:Best[_] => ()
    }
    mergeTake(sc, Map.empty)
  }

  def fix(tokens: List[Loc[Token]])(implicit env: Env): Scored[List[Stmt]] = {
    val asts = Mismatch.repair(prepare(tokens)) flatMap (ts => {
      val asts = ParseEddy.parse(ts)
      if (isDebug) {
        println(s"asts ${asts.size} = ")
        for (a <- asts) {
          implicit val f = abbrevShowFlags
          println(s"  ${show(a)}")
          println(s"    $a")
        }
      }
      uniform(Pr.parse,asts,"Parse failed")
    })
    asts flatMap (denoteStmts(_)(env)) flatMap Expand.expandStmts map (_ map Simplify.simplify)
  }
}
