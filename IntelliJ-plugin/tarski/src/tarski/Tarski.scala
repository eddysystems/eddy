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

  // (show(s),s.toString) for a statement s.  Once we convert Stmt to ShowStmt, Env can be discarded.
  case class ShowStmt(show: String, den: String, comment: Option[String])
  object ShowStmt {
    def apply(s: Stmt)(implicit env: Pretty.Scope): ShowStmt =
      new ShowStmt(show(s),s.toString,s match {
        case CommentStmt(c) => Some(c.content)
        case _ => None
      })
  }

  // Java and Scala result types
  type JList[A] = java.util.List[A]
  type  Results =  List[Alt[ List[ShowStmt]]]
  type JResults = JList[Alt[JList[ShowStmt]]]

  abstract class Enough {
    def enough(rs: JResults): Boolean
  }

  def fixJava(tokens: java.util.List[Located[Token]], env: Env, enough: Enough): JResults = {
    val toks = tokens.asScala.toList
    val r = fix(toks)(env)

    //println(s"fix found: empty ${r.isEmpty}, single ${r.isSingle}, best ${r.best}")

    // Take elements until we have enough, merging duplicates and adding their probabilities if found
    def mergeTake(s: Stream[Alt[List[ShowStmt]]])(m: Map[List[String],Alt[List[ShowStmt]]]): JResults = {
      val rs = (m.toList map {case (_,Alt(p,b)) => Alt(p,b.toList.asJava)} sortBy (-_.p)).asJava
      if (s.isEmpty || enough.enough(rs)) rs
      else {
        val Alt(p,b) = s.head
        val a = b map (_.show)
        mergeTake(s.tail)(m + ((a,m get a match {
          case None => Alt(p,b)
          case Some(Alt(q,c)) => Alt(padd(q,p),c) // Use the List[ShowStmt] from the higher probability alternative
        })))
      }
    }

    r.map { case (env,s) => {
      implicit val e = env
      println(s"$s => ${s.map(show(_))}")
      s map (ShowStmt(_))
    }}.all match {
      case Left(error) =>
        if (trackErrors) println("fixJava failed:\n"+error.prefixed("error: "))
        new java.util.ArrayList[Alt[JList[ShowStmt]]]
      case Right(all) =>
        val rs = mergeTake(all)(Map.empty)
        rs.asScala foreach {case Alt(p,r) => println(s"$p: $r")}
        rs
    }
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
