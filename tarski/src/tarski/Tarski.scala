package tarski

import java.util

import tarski.Denotations.{CommentStmt, Stmt}
import tarski.Environment.{ThreeEnv, PlaceInfo, Env}
import tarski.Items.{Value, TypeItem, Item, Package}
import tarski.Scores._
import tarski.JavaScores._
import tarski.Semantics._
import tarski.Tokens._
import tarski.Tries.{LazyTrie, DTrie, Trie}
import utility.Locations._
import scala.collection.JavaConverters._

object Tarski {

  def makeTrie(jvalues: java.util.Collection[Item]): Trie[Item] = {
    Trie(jvalues.asScala)
  }

  def makeTrie(jvalues: Array[Item]): Trie[Item] = {
    Trie(jvalues)
  }

  def makeDTrie(jvalues: java.util.Collection[Item]): DTrie[Item] = {
    DTrie(jvalues.asScala)
  }

  def environment(sTrie: LazyTrie[Item], dTrie: DTrie[Item], vTrie: Trie[Item],
                  dByItem: java.util.Map[TypeItem,Array[Value]],
                  vByItem: java.util.Map[TypeItem,Array[Value]],
                  scope: java.util.Map[Item,Integer], place: PlaceInfo): Env = {
    println("environment with " + dTrie.values.length + " local items, " + vTrie.values.length + " scope items taken at " + place)
    new ThreeEnv(sTrie, dTrie, vTrie, dByItem, vByItem, scope.asScala.toMap.mapValues(_.intValue), place)
  }

  def print(is: Iterable[Alt[Item]]): Unit = {
    is foreach { case Alt(p,i) =>
      println(s"  $p => " + i)
    }
  }

  // (show(s),s.toString,fullFormat,abbrevFormat) for a statement s.  Once we convert Stmt to ShowStmt, Env can be discarded.
  case class ShowStmt(show: String, den: String, full: String, abbrev: String)

  // Java and Scala result types
  type JList[A] = java.util.List[A]
  type  Results =  List[Alt[ List[ShowStmt]]]
  type JResults = JList[Alt[JList[ShowStmt]]]

  abstract class Take {
    // Accept some results, returning true if we've had enough.
    def take(rs: JResults): Boolean
  }

  // Feed results to a take instance until it's satisfied
  def fixTake(tokens: java.util.List[Located[Token]], env: Env,
              format: (Stmt,String,ShowFlags) => String, take: Take): Unit = {
    val toks = tokens.asScala.toList
    val r = fix(toks)(env)

    //println(s"fix found: empty ${r.isEmpty}, single ${r.isSingle}, best ${r.best}")

    // Take elements until we have enough, merging duplicates and adding their probabilities if found
    def mergeTake(s: Stream[Alt[List[ShowStmt]]])(m: Map[List[String],Alt[List[ShowStmt]]]): Unit = {
      val rs = (m.toList map {case (_,Alt(p,b)) => Alt(p,b.toList.asJava)} sortBy (-_.p)).asJava
      if (!take.take(rs) && s.nonEmpty) {
        val Alt(p,b) = s.head
        val a = b map (_.abbrev)
        println(s"$p: $a")
        mergeTake(s.tail)(m + ((a,m get a match {
          case None => Alt(p,b)
          case Some(Alt(q,c)) => Alt(pmax(q,p),c) // Use the List[ShowStmt] from the higher probability alternative, use maximum probability of any alternative
        })))
      }
    }

    val sc = r.map { case (env,s) => {
      val sh = s map (s => {
        val abbrev   = show(s)(Pretty.prettyStmt(_)(env),abbrevShowFlags).trim
        val sentinel = show(s)(Pretty.prettyStmt(_)(env),sentinelShowFlags).trim
        val full     = show(s)(Pretty.prettyStmt(_)(env),fullShowFlags)
        ShowStmt(show=abbrev,den=s.toString,
          full=format(s,full,fullShowFlags),
          abbrev=ShowFlags.replaceSentinels(format(s,sentinel,sentinelShowFlags)).replaceAll("""\s+"""," "))
      })
      println(s"$s => ${sh map (_.show)}")
      sh
    }}
    // Complain if there's an error
    if (trackErrors) sc.strict match {
      case e:EmptyOrBad => println("fixJava failed:\n"+e.error.prefixed("error: "))
      case _:Best[_] => ()
    }
    mergeTake(sc.stream)(Map.empty)
  }

  def fix(tokens: List[Located[Token]])(implicit env: Env): Scored[(Env,List[Stmt])] = {
    prepare(tokens) flatMap { case (ts,c) =>
      val asts = Mismatch.repair(ts) flatMap (ts => {
        val asts = ParseEddy.parse(ts)
        for (a <- asts; n = asts.count(a==_); if n > 1)
          throw new RuntimeException(s"AST duplicated $n times: $a")
        uniform(Pr.parse,asts,"Parse failed")
      })
      asts flatMap (denoteStmts(_)(env)) map {case (env,ss) => (env,ss ::: c.toList.map(CommentStmt))}
    }
  }
}
