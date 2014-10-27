package tarski

import Environment._
import Tokens.{Token,isSpace}
import Items._
import ambiguity.ParseEddy
import Semantics.denotationScores

import scala.collection.JavaConverters._

object Tarski {
  def environment(types: java.util.Collection[NamedItem], values: java.util.Collection[NamedItem]): JavaEnvironment = {
    baseEnvironment.addObjects(types.asScala.toList++values.asScala.toList)
  }

  def fix(tokens: java.util.List[Token], env: JavaEnvironment): Unit = {
    val toks = tokens.asScala.toList.filterNot(isSpace)
    println("line " + toks)
    for ( root <- ParseEddy.parse(toks) ) {
      println("  ast: " + root)
      println("  meanings: ")
      for ( den <- denotationScores(root, env) ) {
        println("    " + den._2 + ": " + den._1)
      }
    }
  }
}