package tarski

import Environment._
import Tokens._
import Items._
import Scores._
import Denotations._
import Semantics._
import Pretty._

import scala.collection.JavaConverters._

object Tarski {

  def environment(jvalues: java.util.Collection[Item]): Env =
    Base.extraEnv.addObjects(jvalues.asScala,Map.empty)

  def add_environment(env: Env, values: java.util.Collection[Item], inScope: java.util.Map[Item,Integer]): Env =
    env.addObjects(values.asScala.toList, inScope.asScala.toMap.mapValues(_.intValue))

  def localPkg(): PackageItem = Base.LocalPkg

  def baseLookupJava(i: Item): Item = i.qualifiedName match {
    case None => null
    case Some(q) => Base.baseQualifiedNames.getOrElse(q,null)
  }

  def fixJava(tokens: java.util.List[Token], env: Env): java.util.List[Alt[java.util.List[Stmt]]] = {
    val limit = 4 // Report at most this many alternatives
    val toks = tokens.asScala.toList
    val r = fix(toks)(env)

    // Take up to limit elements, merging duplicates and adding their probabilities if found
    def mergeTake[A](s: Stream[Alt[A]])(m: Map[A,Prob]): List[Alt[A]] =
      if (m.size == limit || s.isEmpty)
        m.toList map {case (a,p) => Alt(p,a)} sortBy (-_.p)
      else {
        val Alt(p,a) = s.head
        mergeTake(s.tail)(m+((a,p+m.getOrElse(a,0.0))))
      }

    (r.map(_._2.asJava).all match {
      case Left(error) => Nil
      case Right(all) => mergeTake(all)(Map.empty)
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
    val clean = tokens.filterNot(isSpace).map(fake)
    val asts = Mismatch.repair(clean) flatMap (ts => {
      val asts = ParseEddy.parse(ts)
      for (a <- asts; n = asts.count(a==_); if n > 1)
        throw new RuntimeException(s"AST duplicated $n times: $a")
      uniform(1,asts,"Parse failed")
    })
    asts flatMap (denoteStmts(_)(env))
  }
}