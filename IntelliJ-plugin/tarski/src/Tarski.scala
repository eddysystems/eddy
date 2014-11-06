package tarski

import Environment._
import Tokens._
import Items._
import tarski.Scores._
import tarski.Denotations._
import tarski.Semantics._
import tarski.Pretty._

import scala.collection.JavaConverters._

object Tarski {
  def environment(values: java.util.Collection[NamedItem], inScope: java.util.Map[NamedItem,Integer], place: NamedItem): Env = {
    Base.baseEnv.move(place).addObjects(values.asScala.toList, inScope.asScala.toMap.mapValues(_.intValue))
  }

  def localPkg(): PackageItem = Base.LocalPkg

  def fixJava(tokens: java.util.List[Token], env: Env): java.util.List[(Score,java.util.List[Stmt])] = {
    val toks = tokens.asScala.toList
    val r = fix(toks)(env)
    // TODO: Propagate error messages into Java
    ((r map {case (e,ss) => ss.asJava}).all match {
      case Left(error) => Nil
      case Right(all) => all
    }).asJava
  }

  def pretty(ss: java.util.List[Stmt]): String =
    show(tokens(ss.asScala.toList))

  def fix(tokens: List[Token])(implicit env: Env): Scored[(Env,List[Stmt])] = {
    println("parsing " + show(tokens))
    val asts = ParseEddy.parse(tokens.filterNot(isSpace))
    if (asts.isEmpty)
      println("  no asts")

    // Check for duplicates
    val uasts = asts.toSet
    val bads = for (a <- uasts; n = asts.count(a==_); if n > 1) yield
      println(s"  $n copied ast: $a")
    if (!bads.isEmpty)
      throw new RuntimeException("duplicated ast")

    // Determine meaning(s)
    simple(uasts.toList,"Parse failed") flatMap { root => {
      println("  ast: " + show(Pretty.tokens(root)))
      //println("  ast: " + root)
      println("  meanings: ")
      val ds = denoteStmts(root)(env)
      ds.all match {
        case Left(e) => println(e.prefixed("    error: "))
        case Right(all) =>
          for ((s,(e,d)) <- all)
            println(s"    $s: $d")
      }
      ds
    }}
  }
}