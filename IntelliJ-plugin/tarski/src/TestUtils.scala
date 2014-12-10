package tarski

import org.apache.commons.lang.StringEscapeUtils._
import tarski.AST._
import tarski.Base._
import tarski.Denotations._
import tarski.Environment.Env
import tarski.Items._
import tarski.Tokens._
import tarski.Types.{ClassType, LangType, TypeVar, VoidType}
import scala.language.implicitConversions

object TestUtils {
  // AST implicit conversions
  implicit def toAExp(i: Int): AExp = IntALit(i.toString)
  implicit def toAExp(s: String): AExp = NameAExp(s)
  implicit def toAExp(b: Boolean): AExp = BoolALit(b)
  implicit def toAExps(e: AExp): List[AExp] = List(e)
  implicit def toAStmt(e: AExp): AStmt = ExpAStmt(e)
  implicit def toAStmts(e: AExp): List[AStmt] = List(ExpAStmt(e))
  implicit def toAStmts(s: AStmt): List[AStmt] = List(s)
  implicit def toAExp(t: LangType): AExp = NameAExp(show(t))
  implicit def toAExps[A](xs: KList[A])(implicit to: A => AExp): KList[AExp] = xs map to

  // Denotation implicit conversions
  implicit def toExp(b: Boolean): Exp = BooleanLit(b)
  implicit def toExp(i: Int): Exp = IntLit(i,i.toString)
  implicit def toExp(c: Char): Exp = CharLit(c, "'" + escapeJava(c.toString) + "'")
  implicit def toExp(d: Double): Exp = DoubleLit(d,d.toString)
  implicit def toExp(x: LocalVariableItem): Exp = LocalVariableExp(x)
  implicit def toExps[A](xs: List[A])(implicit to: A => Exp): List[Exp] = xs map to
  implicit def toExps(e: Exp): List[Exp] = List(e)

  // Type implicit conversions
  implicit def toType(c: ClassItem): ClassType = c.simple

  // Statement implicit conversions
  implicit def toStmt(e: StmtExp): Stmt = ExpStmt(e)
  implicit def toStmt[A](x: A)(implicit to: A => StmtExp): Stmt = ExpStmt(to(x))
  implicit def toStmts(e: StmtExp): List[Stmt] = List(ExpStmt(e))
  implicit def toStmts(s: Stmt): List[Stmt] = List(s)

  // Variable declarations, for statements, etc.
  implicit def toVarDecl[A](v: (LocalVariableItem,A))(implicit to: A => Exp): VarDecl = (v._1,0,Some(to(v._2)))
  implicit def toVarDecls[A](v: A)(implicit to: A => VarDecl): List[VarDecl] = List(to(v))
  implicit def toForInit(n: List[Nothing]): ForInit = ForExps(Nil)
  implicit def toForInit(e: Exp): ForInit = ForExps(List(e))

    // Inside a function with a bunch of locals
  def localEnv(locals: Item*): Env = {
    val X = NormalClassItem("XX", LocalPkg)
    val f = NormalMethodItem("ff", X, Nil, VoidType, Nil, false)
    new Env(Array(f,X) ++ locals, Map((f,2),(X,2)) ++ locals.map((_,1)).toMap[Item,Int], f)
  }
  def localEnvWithBase(locals: Item*): Env = {
    val X = NormalClassItem("XX", LocalPkg)
    val f = NormalMethodItem("ff", X, Nil, VoidType, Nil, false)
    baseEnv.addObjects(Array(f,X) ++ locals, Map((f,2),(X,2)) ++ locals.map((_,1)).toMap[Item,Int])
           .move(f,inside_breakable=false,inside_continuable=false,Nil)
  }

  def assertIn[A](x: A, xs: Set[A]): Unit =
    if (!xs.contains(x))
      throw new AssertionError("assertIn failed:\nx  = "+x+"\nxs = "+xs.mkString("\n     "))

  def assertSetsEqual[A](exp: Traversable[A], got: Traversable[A]): Unit = {
    def s(n: Name, xs: Set[A]) = f"\n$n%-7s = ${xs.mkString("\n          ")}"
    val e = exp.toSet
    val g = got.toSet
    if (e != g)
      throw new AssertionError("assertSetsEqual failed:"
        +s("exp",e)+s("got",g)+s("exp-got",e--g)+s("got-exp",g--e))
  }
}
