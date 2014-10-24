import java.io.File

import ambiguity._
import scala.io.Source

object main extends App {
  def gen(path: String) = {
    val file = Source.fromFile(path).mkString
    val Gr = Grammar.read(file)
    Grammar.check(Gr,generic=true)
    val Gc = Grammar.complete(Gr)
    Grammar.check(Gc)
    val G = Grammar.binarize(Gc)
    Grammar.check(G)
    val parse = Parse.parseGen(G).mkString("\n")+"\n"
    print(parse)
  }
  args match {
    case Array(path) => gen(path)
    case _ => throw new RuntimeException("one argument expected")
  }
}