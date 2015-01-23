import java.io.File

import ambiguity._
import ambiguity.Grammar._
import scala.io.Source

object Main {
  def grammar(path: String, binary: Boolean = true): Grammar = {
    val file = Source.fromFile(path).mkString
    val Gr = read(file)
    check(Gr,generic=true)
    val Gc = complete(Gr)
    check(Gc)
    val G = if (binary) binarize(Gc) else Gc
    check(G)
    G
  }

  def parse(G: Grammar, nop: Boolean = false) =
    print(Parse.parseGen(G,nop=nop).mkString("\n")+"\n")

  def actions(G: Grammar) =
    print(Parse.actionGen(G).mkString("\n")+"\n")

  def main(args: Array[String]): Unit = args match {
    case Array(path) => parse(grammar(path))
    case Array("-n",path) => parse(grammar(path),nop=true)
    case Array("-a",path) => actions(grammar(path))
    case Array("-s",path) => actions(grammar(path,binary=false))
    case _ => throw new RuntimeException("one argument expected")
  }
}