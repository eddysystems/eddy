package tarski

import utility.Locations._
import utility.Utility._
import org.apache.commons.lang.StringEscapeUtils._
import tarski.AST._
import tarski.Base._
import tarski.Denotations._
import tarski.Environment.{Env, PlaceInfo}
import tarski.Items._
import tarski.Tokens._
import tarski.Types._
import scala.language.implicitConversions

object TestUtils {
  // AST implicit conversions
  private val r = SRange.unknown
  implicit def toAExp(i: Int): AExp = IntALit(i.toString,r)
  implicit def toAExp(s: String): AExp = NameAExp(s,r)
  implicit def toAExp(b: Boolean): AExp = NameAExp(if (b) "true" else "false",r)
  implicit def toAExps(e: AExp): List[AExp] = List(e)
  implicit def toOAExp(e: AExp): Option[AExp] = Some(e)
  implicit def toOAExp[A](e: A)(implicit to: A => AExp): Option[AExp] = Some(to(e))
  implicit def toAStmt(e: AExp): AStmt = ExpAStmt(e)
  implicit def toAStmts(e: AExp): List[AStmt] = List(ExpAStmt(e))
  implicit def toAStmts(s: AStmt): List[AStmt] = List(s)
  implicit def toAExp(t: LangType): AExp = NameAExp(t.name,r)
  implicit def toAExps[A](xs: KList[A])(implicit to: A => AExp): KList[AExp] = xs map to
  implicit def toAExps[A](x: A)(implicit to: A => AExp): KList[AExp] = SingleList(to(x))
  implicit def toAVarDecls(v: AVarDecl): KList[AVarDecl] = SingleList(v)

  // Denotation implicit conversions
  implicit def toExp(b: Boolean): Exp = BooleanLit(b)
  implicit def toExp(i: Int): Exp = IntLit(i,i.toString)
  implicit def toExp(i: Long): Exp = LongLit(i,s"${i}L")
  implicit def toExp(c: Char): Exp = CharLit(c, "'" + escapeJava(c.toString) + "'")
  implicit def toExp(d: Double): Exp = DoubleLit(d,d.toString)
  implicit def toExp(x: Local): Exp = LocalExp(x)
  implicit def toExp(x: ThisItem): Exp = ThisExp(x)
  implicit def toExps[A](xs: List[A])(implicit to: A => Exp): List[Exp] = xs map to
  implicit def toExps(e: Exp): List[Exp] = List(e)
  implicit def toOExp[A](x: A)(implicit to: A => Exp): Option[Exp] = Some(to(x))
  implicit def toOExp(e: Exp): Option[Exp] = Some(e)

  // Callable implicit conversions
  implicit def toCall(x: MethodItem): NotTypeApply = if (x.isStatic) MethodDen(None,x) else impossible

  // Type implicit conversions
  implicit def toType(c: ClassItem): ClassType = c.simple
  implicit def toTypeArgs[A](ts: List[A])(implicit to: A => TypeArg): List[TypeArg] = ts map to

  // Statement implicit conversions
  implicit def toStmt(e: StmtExp): Stmt = ExpStmt(e)
  implicit def toStmt[A](x: A)(implicit to: A => StmtExp): Stmt = ExpStmt(to(x))
  implicit def toStmts(e: StmtExp): List[Stmt] = List(ExpStmt(e))
  implicit def toStmts(s: Stmt): List[Stmt] = List(s)

  // Variable declarations, for statements, etc.
  implicit def toVarDecl[A](v: (Local,A))(implicit to: A => Exp): VarDecl = (v._1,0,Some(to(v._2)))
  implicit def toVarDecls[A](v: A)(implicit to: A => VarDecl): List[VarDecl] = List(to(v))
  implicit def toForInit(n: List[Nothing]): ForInit = ForExps(Nil)
  implicit def toForInit(e: Exp): ForInit = ForExps(List(e))

    // Inside a function with a bunch of locals
  def localEnv(locals: Item*): Env = {
    val X = NormalClassItem("XX", LocalPkg)
    val f = NormalMethodItem("ff", X, Nil, VoidType, Nil, false)
    Env(Array(f,X) ++ locals, Map((f,2),(X,2)) ++ locals.map((_,1)).toMap[Item,Int], PlaceInfo(f))
  }
  def localEnvWithBase(locals: Item*): Env = {
    val X = NormalClassItem("XX", LocalPkg)
    val f = NormalMethodItem("ff", X, Nil, VoidType, Nil, false)
    testEnv.extend(Array(f,X) ++ locals, Map((f,2),(X,2)) ++ locals.map((_,1)).toMap[Item,Int])
           .move(PlaceInfo(f))
  }

  def assertIn[A](x: A, xs: Set[A]): Unit =
    if (!xs.contains(x))
      throw new AssertionError("assertIn failed:\nx  = "+x+"\nxs = "+xs.mkString("\n     "))

  private def noClean(s: String) = s
  def assertSetsEqual[A](exp: Traversable[A], got: Traversable[A], clean: String => String = noClean): Unit = {
    def s(n: Name, xs: Set[A]) = f"\n$n%-7s = ${xs map (x => clean(x.toString)) mkString "\n          "}"
    val e = exp.toSet
    val g = got.toSet
    if (e != g)
      throw new AssertionError("assertSetsEqual failed:"
        +s("exp",e)+s("got",g)+s("exp-got",e--g)+s("got-exp",g--e))
  }
}
