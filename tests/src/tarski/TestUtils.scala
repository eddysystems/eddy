/* TestUtils: Lots of implicit conversions to easy Denotation tests
 *
 * The Denotation intermediate representation is fairly verbose, and fairly painful
 * to write when constructing unit tests.  TestUtils provides a large number of
 * implicit conversions for making this easier, such as converting the scala literal
 * 7 into IntLit(7,"7",SRange.unknown).
 */

package tarski

import utility.Locations._
import utility.Utility._
import org.apache.commons.lang.StringEscapeUtils._
import tarski.AST._
import tarski.Arounds._
import tarski.Base._
import tarski.Denotations._
import tarski.Environment.{Env, PlaceInfo}
import tarski.Items._
import tarski.Tokens._
import tarski.Types._
import tarski.Mods._
import scala.language.implicitConversions

object TestUtils {
  // Location implicit conversions
  private val r = SRange.unknown
  private val a = SGroup.unknown
  implicit def toLoc[A](x: A): Loc[A] = Loc(x,r)
  implicit def toGrouped[A](x: A): Grouped[A] = Grouped(x,a)
  implicit def toGroupLocs(n: Int): List[SGroup] = List.fill(n)(a)

  // Empty lists
  implicit def toEmptyList(xs: List[Nothing]): EmptyList.type = EmptyList

  // AST implicit conversions
  implicit def toAExp(i: Int): AExp = IntALit(i.toString,r)
  implicit def toAExp(s: String): AExp = NameAExp(s,r)
  implicit def toAExp(b: Boolean): AExp = NameAExp(if (b) "true" else "false",r)
  implicit def toAExps(e: AExp): List[AExp] = List(e)
  implicit def toOAExp(e: AExp): Option[AExp] = Some(e)
  implicit def toOAExp[A](e: A)(implicit to: A => AExp): Option[AExp] = Some(to(e))
  implicit def toAStmt(e: AExp): AStmt = ExpAStmt(e)
  implicit def toAStmts(e: AExp): List[AStmt] = List(ExpAStmt(e))
  implicit def toAStmts(s: AStmt): List[AStmt] = List(s)
  implicit def toAStmtsC(e: AExp): CommaList[AStmt] = SingleList(ExpAStmt(e))
  implicit def toAStmtsC(s: AStmt): CommaList[AStmt] = SingleList(s)
  implicit def toAExp(t: LangType): AExp = NameAExp(t.name,r)
  implicit def toAExps[A](xs: KList[A])(implicit to: A => AExp): KList[AExp] = xs map to
  implicit def toAExps[A](x: A)(implicit to: A => AExp): SingleList[AExp] = SingleList(to(x))
  implicit def toAVarDecls(v: AVarDecl): KList[AVarDecl] = SingleList(v)
  implicit def toMods(m: Mod): Mods = List(Loc(m,r))

  // Denotation implicit conversions
  implicit def toExp(b: Boolean): Exp = BooleanLit(b,r)
  implicit def toExp(i: Int): Exp = IntLit(i,i.toString,r)
  implicit def toExp(i: Long): Exp = LongLit(i,s"${i}L",r)
  implicit def toExp(c: Char): Exp = CharLit(c,"'"+escapeJava(c.toString)+"'",r)
  implicit def toExp(d: Double): Exp = DoubleLit(d,d.toString,r)
  implicit def toExp(s: String): Exp = StringLit(s,'"'+escapeJava(s)+'"',r)
  implicit def toExp(x: Local): Exp = LocalExp(x,r)
  implicit def toExp(x: ThisOrSuper): Exp = ThisOrSuperExp(x,r)
  implicit def toExps[A](xs: List[A])(implicit to: A => Exp): List[Exp] = xs map to
  implicit def toExps(e: Exp): List[Exp] = List(e)
  implicit def toOExp[A](x: A)(implicit to: A => Exp): Option[Exp] = Some(to(x))
  implicit def toOExp(e: Exp): Option[Exp] = Some(e)

  // Callable implicit conversions
  implicit def toCall(x: MethodItem)(implicit env: Env): NotTypeApply = if (x.isStatic) MethodDen(None,x,r) else impossible

  // Type implicit conversions
  implicit def toType(c: ClassItem): ClassType = c.simple
  implicit def toTypeArgs[A](ts: List[A])(implicit to: A => TypeArg): List[TypeArg] = ts map to

  // Statement implicit conversions
  implicit def toStmt(e: StmtExp)(implicit env: Env): Stmt = ExpStmt(e,env)
  implicit def toStmt[A](x: A)(implicit to: A => StmtExp, env: Env): Stmt = ExpStmt(to(x),env)
  implicit def toStmts(e: StmtExp)(implicit env: Env): List[Stmt] = List(ExpStmt(e,env))
  implicit def toStmts(s: Stmt): List[Stmt] = List(s)

  // Variable declarations, for statements, etc.
  implicit def toVarDecl[A](v: (Local,A))(implicit to: A => Exp, env: Env): VarDecl = VarDecl(v._1,r,Nil,Some(r,to(v._2)),env)
  implicit def toVarDecls[A](v: A)(implicit to: A => VarDecl): List[VarDecl] = List(to(v))
  implicit def toForInit(n: List[Nothing])(implicit env: Env): ForInit = ForExps(Nil,r,env)
  implicit def toForInit(e: Exp)(implicit env: Env): ForInit = ForExps(List(e),r,env)

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
