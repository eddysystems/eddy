package tarski

import tarski.Denotations.Stmt
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

  type Result = java.util.List[String]
  abstract class Enough {
    def enough(m: Map[Result,Prob]): Boolean
  }

  def fixJava(tokens: java.util.List[Located[Token]], env: Env, enough: Enough): java.util.List[Alt[Result]] = {
    val toks = tokens.asScala.toList
    val r = fix(toks)(env)

    //println(s"fix found: empty ${r.isEmpty}, single ${r.isSingle}, best ${r.best}")

    // Take elements until we have enough, merging duplicates and adding their probabilities if found
    def mergeTake(s: Stream[Alt[Result]])(m: Map[Result,Prob]): List[Alt[Result]] =
      if (s.isEmpty || enough.enough(m)) {
        m.toList map {case (a,p) => Alt(p,a)} sortBy (-_.p)
      } else {
        val Alt(p,a) = s.head
        mergeTake(s.tail)(m+((a,padd(p,m.getOrElse(a,pzero)))))
      }

    (r.map { case (env,s) => {
        implicit val e = env;
        println(s"$s => ${s.map(show(_))}")
        s.map(show(_)).asJava
    }}.all match {
      case Left(error) => Nil
      case Right(all) => {
        val rs = mergeTake(all)(Map.empty)
        rs foreach {case Alt(p,r) => println(s"$p: $r")}
        rs
      }
    }).asJava
  }

  def fix(tokens: List[Located[Token]])(implicit env: Env): Scored[(Env,List[Stmt])] = {
    val asts = Mismatch.repair(prepare(tokens)) flatMap (ts => {
      val asts = ParseEddy.parse(ts)
      for (a <- asts; n = asts.count(a==_); if n > 1)
        throw new RuntimeException(s"AST duplicated $n times: $a")
      uniform(Pr.parse,asts,"Parse failed")
    })
    asts flatMap (denoteStmts(_)(env))
  }
}
