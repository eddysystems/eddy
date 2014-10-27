package tarski

import tarski.Environment._
import tarski.Tokens._
import ambiguity.ParseEddy
import scala.collection.JavaConverters._

object Tarski {
  def fix(tokens: java.util.List[Token], env: JavaEnvironment): Unit = {
    val toks = tokens.asScala.toList.filterNot(isSpace)
    println("    ast: " + ParseEddy.parse(toks))
  }
}