package tarski

import java.io.{ObjectInputStream, FileInputStream}

import Environment._
import Tokens.{Token,isSpace}
import Items._
import ambiguity.ParseEddy
import com.intellij.util.SmartList
import tarski.Semantics.{Score, denotationScores}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

object Tarski {
  def environment(types: java.util.Collection[NamedItem], values: java.util.Collection[NamedItem]): JavaEnvironment = {
    baseEnvironment.addObjects(types.asScala.toList++values.asScala.toList)
  }

  def fix(tokens: java.util.List[Token], env: JavaEnvironment): java.util.List[(AST.Stmt, java.util.List[(java.util.Map[AST.Node,Items.EnvItem],Score)])] = {
    val results: java.util.List[(AST.Stmt, java.util.List[(java.util.Map[AST.Node,Items.EnvItem],Score)])] = new SmartList[(AST.Stmt, java.util.List[(java.util.Map[AST.Node,Items.EnvItem],Score)])]()
    val toks = tokens.asScala.toList.filterNot(isSpace)
    println("line " + toks)
    for ( root <- ParseEddy.parse(toks) ) {
      println("  ast: " + root)
      println("  meanings: ")
      val ds = denotationScores(root, env)
      results.add((root, ListBuffer(ds.map( x => (x._1.asJava, x._2) ):_*).asJava))
      for ( den <- ds ) {
        println("    " + den._2 + ": " + den._1)
      }
    }
    results
  }
}