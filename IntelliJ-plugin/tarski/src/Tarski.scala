package tarski

import Environment._
import Tokens._
import Items._
import Scores._
import Denotations._
import Semantics._
import Pretty._

import ambiguity.Utility._

import scala.collection.JavaConverters._

object Tarski {

  def environment(jvalues: java.util.Collection[Item]): Env = {
    println("making environment from " + jvalues.size + " items")
    val things = addItemsToMapList(Base.baseEnv.things, jvalues.asScala)
    println("  merged things")
    Env(new Trie.CompactTrie(jvalues.asScala, (x:Item) => x.name ), things, Base.baseEnv.inScope, Base.baseEnv.place, false, false, Nil)
  }

  def add_environment(env: Env, values: java.util.Collection[Item], inScope: java.util.Map[Item,Integer]): Env = {
    env.addObjects(values.asScala.toList, inScope.asScala.toMap.mapValues(_.intValue))
  }

  def localPkg(): PackageItem = Base.LocalPkg

  def fixJava(tokens: java.util.List[Token], env: Env): java.util.List[Alt[java.util.List[Stmt]]] = {
    val toks = tokens.asScala.toList
    val r = fix(toks)(env)
    // TODO: Propagate error messages into Java
    ((r map {case (e,ss) => ss.asJava}).all match {
      case Left(error) => Nil
      case Right(all) => all
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
    println("parsing " + show(clean))
    val asts = ParseEddy.parse(clean)
    if (asts.isEmpty)
      println("  no asts")

    // Check for duplicates
    val uasts = asts.toSet
    val bads = for (a <- uasts; n = asts.count(a==_); if n > 1) yield
      println(s"  $n copied ast: $a")
    if (!bads.isEmpty)
      throw new RuntimeException("duplicated ast")

    // Determine meaning(s)
    multiple(uasts.toList map (Alt(Prob(1),_)), "Parse failed") flatMap { root => {
      println("  ast: " + show(Pretty.tokens(root)))
      //println("  ast: " + root)
      println("  meanings: ")
      val ds = denoteStmts(root)(env)
      ds.all match {
        case Left(e) => println(e.prefixed("    error: "))
        case Right(all) =>
          for (Alt(s,(e,d)) <- all)
            println(s"    $s: \t${show(d)} \t($d)")
      }
      ds
    }}
  }
}