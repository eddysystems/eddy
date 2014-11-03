package tarski

import java.io.{ObjectInputStream, FileInputStream}

import Environment._
import Tokens.{Token,isSpace}
import Items._
import ambiguity.ParseEddy
import tarski.Scores._
import tarski.Denotations._
import tarski.Semantics._

import scala.collection.JavaConverters._

object Tarski {
  def environment(types: java.util.Collection[NamedItem], values: java.util.Collection[NamedItem]): Env = {
    baseEnvironment.addObjects(types.asScala.toList++values.asScala.toList)
  }

  def fixJava(tokens: java.util.List[Token], env: Env): java.util.List[(Score,java.util.List[StmtDen])] = {
    val toks = tokens.asScala.toList
    val r = fix(toks)(env)
    (r map {case (e,ss) => ss.asJava}).c.asJava
  }

  def fix(tokens: List[Token])(implicit env: Env): Scored[(Env,List[StmtDen])] = {
    var results: Scored[(Env,List[StmtDen])] = fail
    println("parsing " + tokens)
    val asts = ParseEddy.parse(tokens.filterNot(isSpace))
    println("  " + asts)
    for (root <- asts) {
      println("  ast: " + root)
      println("  meanings: ")
      val ds = denoteStmts(root)(env)
      for ((s,(e,d)) <- ds.c)
        println(s"    $s: $d")
      results ++= ds
    }
    results
  }
}