package tarski

import tarski.Environment.JavaEnvironment
import tarski.Tokens.{Token,isSpace}
import ambiguity.ParseEddy
import Semantics.denotationScores

import scala.collection.JavaConverters._

object Tarski {
  def fix(tokens: java.util.List[Token], env: JavaEnvironment): Unit = {
    val toks = tokens.asScala.toList.filterNot(isSpace)
    for ( root <- ParseEddy.parse(toks) ) {
      println("ast: " + root)
      println("meanings: ")
      for ( den <- denotationScores(root, env) ) {
        println("  " + den._2 + ": " + den._1)
      }
    }
  }
}