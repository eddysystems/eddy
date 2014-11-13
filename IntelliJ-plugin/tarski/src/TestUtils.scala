package tarski

import scala.language.implicitConversions
import org.apache.commons.lang.StringEscapeUtils._
import tarski.AST._
import tarski.Denotations._
import tarski.Items._
import tarski.Types._

object TestUtils {
  // AST implicit conversions
  implicit def toAExp(i: Int): AExp = IntALit(i.toString)
  implicit def toAExp(s: String): AExp = NameAExp(s)
  implicit def toAExps(e: AExp): List[AExp] = List(e)
  implicit def toAStmt(e: AExp): AStmt = ExpAStmt(e)
  implicit def toAStmts(e: AExp): List[AStmt] = List(ExpAStmt(e))
  implicit def toAStmts(s: AStmt): List[AStmt] = List(s)

  // AST type implicit conversions
  implicit def toAType(t: PrimType): AType = PrimAType(t)

  // Denotation implicit conversions
  implicit def toExp(i: Int): Exp = IntLit(i,i.toString)
  implicit def toExp(c: Char): Exp = CharLit(c, "'" + escapeJava(c.toString) + "'")
  implicit def toExp(d: Double): Exp = DoubleLit(d,d.toString)
  implicit def toExp(x: LocalVariableItem): Exp = LocalVariableExp(x)
  implicit def toExps[A](xs: List[A])(implicit to: A => Exp): List[Exp] = xs map to
  implicit def toExps(e: Exp): List[Exp] = List(e)

  // Type implicit conversions
  implicit def toType(v: TypeParamItem): ParamType = ParamType(v)
  implicit def toType(c: ClassItem): SimpleClassType = { assert(c.params.isEmpty); SimpleClassType(c) }
  implicit def toType(i: InterfaceItem): SimpleInterfaceType = { assert(i.params.isEmpty); SimpleInterfaceType(i) }

  // Statement implicit conversions
  implicit def toStmt(e: Exp): Stmt = ExpStmt(e)
  implicit def toStmt[A](x: A)(implicit to: A => Exp): Stmt = ExpStmt(to(x))
  implicit def toStmts(e: Exp): List[Stmt] = List(ExpStmt(e))
  implicit def toStmts(s: Stmt): List[Stmt] = List(s)

  // Variable declaractions, for statements, etc.
  implicit def toVarDecl[A](v: (LocalVariableItem,A))(implicit to: A => Exp): VarDecl = (v._1,0,Some(to(v._2)))
  implicit def toVarDecls[A](v: A)(implicit to: A => VarDecl): List[VarDecl] = List(to(v))
  implicit def toForInit(n: List[Nothing]): ForInit = ForExps(Nil)
  implicit def toForInit(e: Exp): ForInit = ForExps(List(e))

  def assertIn[A](x: A, xs: Set[A]): Unit =
    if (!xs.contains(x))
      throw new AssertionError("assertIn failed:\nx  = "+x+"xs = "+xs.mkString("\n     "))

  def assertSetsEqual[A](exp: Traversable[A], got: Traversable[A]): Unit = {
    def s(n: Name, xs: Set[A]) = f"\n$n%-7s = ${xs.mkString("\n          ")}"
    val e = exp.toSet
    val g = got.toSet
    if (e != g)
      throw new AssertionError("assertSetsEqual failed:"
        +s("exp",e)+s("got",g)+s("exp-got",e--g)+s("got-exp",g--e))
  }
}
