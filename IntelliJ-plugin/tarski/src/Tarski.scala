package tarski

import java.io.{ObjectInputStream, FileInputStream}

import Environment._
import Tokens.{Token,isSpace}
import Items._
import ambiguity.ParseEddy
import java.util.ArrayList
import tarski.Scores._
import tarski.Denotations._
import tarski.Semantics._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

object Tarski {
  def environment(types: java.util.Collection[NamedItem], values: java.util.Collection[NamedItem]): JavaEnvironment = {
    baseEnvironment.addObjects(types.asScala.toList++values.asScala.toList)
  }

  def fix(tokens: java.util.List[Token], env: JavaEnvironment): java.util.List[(AST.Stmt, java.util.List[(Score,StmtDen)])] = {
    val results: java.util.List[(AST.Stmt, java.util.List[(Score,StmtDen)])] = new java.util.ArrayList[(AST.Stmt,java.util.List[(Score,StmtDen)])]()
    val toks = tokens.asScala.toList.filterNot(isSpace)
    println("line " + toks)
    for (root <- ParseEddy.parse(toks)) {
      println("  ast: " + root)
      println("  meanings: ")
      val ds = denote(root)(env)
      results.add((root,ds.c.asJava))
      for ((s,d) <- ds.c)
        println(s"    $s: $d")
    }
    results
  }
}