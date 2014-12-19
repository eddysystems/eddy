package tarski

import tarski.Denotations.Stmt
import tarski.Environment.Env
import tarski.Items.{Item, PackageItem}
import tarski.Pretty._
import tarski.Scores._
import tarski.JavaScores._
import tarski.Semantics._
import tarski.Tokens._

import scala.collection.JavaConverters._

object Tarski {

  def environment(jvalues: java.util.Collection[Item]): Env = {
    val vs = jvalues.asScala.toArray // Copy because jvalues may change during addObjects due to lazy conversion
    val all = vs++Base.extraEnv.allItems
    Env(all,Map.empty)
  }

  def addEnvironment(env: Env, values: Array[Item], scope: java.util.Map[Item,Integer]): Env =
    env.extend(values, scope.asScala.toMap.mapValues(_.intValue))

  def localPkg(): PackageItem = Base.LocalPkg

  def print(is: Iterable[Alt[Item]]): Unit = {
    is.foreach { case Alt(p,i) =>
      println(s"  $p => " + i.print)
    }
  }

  type Result = java.util.List[String]
  abstract class Enough {
    def enough(m: Map[Result,Prob]): Boolean
  }

  def fixJava(tokens: java.util.List[Token], env: Env, enough: Enough): java.util.List[Alt[Result]] = {
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

  def prettyJava(ss: java.util.List[Stmt], e: Env): String = {
    implicit val env = e
    show(tokens(ss.asScala.toList))
  }

  def pretty(s: Stmt)(implicit env: Env): String =
    show(tokens(s))

  def pretty(ss: java.util.List[Stmt])(implicit env: Env): String =
    show(tokens(ss.asScala.toList))

  def fix(tokens: List[Token])(implicit env: Env): Scored[(Env,List[Stmt])] = {
    val asts = Mismatch.repair(prepare(tokens)) flatMap (ts => {
      val asts = ParseEddy.parse(ts)
      for (a <- asts; n = asts.count(a==_); if n > 1)
        throw new RuntimeException(s"AST duplicated $n times: $a")
      uniform(Pr.parse,asts,"Parse failed")
    })
    asts flatMap (denoteStmts(_)(env))
  }
}
