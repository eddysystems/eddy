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
  def environment(values: java.util.Collection[NamedItem], inScope: java.util.Map[NamedItem,Integer], place: PlaceItem, inside_breakable: Boolean, inside_continuable: Boolean, labels: java.util.List[String] = Nil.asJava): Env = {
    Base.baseEnv.addObjects(values.asScala.toList, inScope.asScala.toMap.mapValues(_.intValue)).move(place, inside_breakable, inside_continuable, labels.asScala.toList)
  }

  def localPkg(): PackageItem = Base.LocalPkg

  // like Types.toType, but without support for generics right now
  def toType(ty: TypeItem): Types.Type = Types.toType(ty,Nil)

  def fixJava(tokens: java.util.List[Token], env: Env): java.util.List[(Prob,java.util.List[Stmt])] = {
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
    multiple(uasts.toList map { (Prob(1.0), _) }, "Parse failed") flatMap { root => {
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