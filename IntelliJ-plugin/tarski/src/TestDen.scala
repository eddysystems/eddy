package tarski

import java.io.FileOutputStream

import tarski.AST._
import tarski.Denotations._

import org.apache.commons.lang.StringEscapeUtils.escapeJava
import scala.language.implicitConversions
import org.testng.annotations.{BeforeClass, Test}
import org.testng.AssertJUnit._

import tarski.Tarski.fix
import tarski.Environment.Env
import tarski.Base._
import tarski.Items._
import tarski.Lexer._
import tarski.Tokens._
import tarski.Types._
import tarski.TestUtils._
import ambiguity.Utility._

class TestDen {

  def testDen(input: String, best: Env => List[Stmt])(implicit env: Env): Unit = {
    fix(lex(input).filterNot(isSpace)).best match {
      case Left(e) => throw new RuntimeException("\n"+e.prefixed("error: "))
      case Right((env,s)) => assertEquals(best(env),s)
    }
  }
  def testDen(input: String, best: Exp)(implicit env: Env): Unit =
    testDen(input, env => List(ExpStmt(best)))

  def testOnlyDenotation(input: String, best: Env => List[Stmt])(implicit env: Env) = {
    val fixes = fix(lex(input).filterNot(isSpace))
    assertEquals(fixes.all.right.get.length, 1)
    val (env2,stmt) = fixes.best.right.get
    assertEquals(stmt, best(env2))
  }

  @Test
  def assignExp(): Unit = {
    val x = LocalVariableItem("x",IntType)
    implicit val env = new Env(List(x))
    testDen("x = 1", AssignExp(None,x,1))
  }

  @Test
  def variableStmt(): Unit = {
    implicit val env = baseEnv
    testDen("x = 1", env => List(VarStmt(IntType, List((env.exactLocal("x"), Some(toExp(1)))))))
  }

  @Test
  def arrayVariableStmtCurly(): Unit = {
    implicit val env = baseEnv
    testDen("x = {1,2,3,4}", env => List(VarStmt(ArrayType(IntType), List((env.exactLocal("x"),
      Some(ArrayExp(IntType,List(1,2,3,4))))))))
  }

  @Test
  def arrayVariableStmtParen(): Unit = {
    implicit val env = baseEnv
    testDen("x = (1,2,3,4)", env => List(VarStmt(ArrayType(IntType), List((env.exactLocal("x"),
      Some(ArrayExp(IntType,List(1,2,3,4))))))))
  }

  @Test
  def arrayVariableStmtBare(): Unit = {
    implicit val env = baseEnv
    testDen("x = 1,2,3,4", env => List(VarStmt(ArrayType(IntType), List((env.exactLocal("x"),
      Some(ArrayExp(IntType,List(1,2,3,4))))))))
  }

  @Test
  def arrayVariableStmtBrack(): Unit = {
    implicit val env = baseEnv
    testDen("x = [1,2,3,4]", env => List(VarStmt(ArrayType(IntType), List((env.exactLocal("x"),
      Some(ArrayExp(IntType,List(1,2,3,4))))))))
  }

  @Test
  def arrayLiteralAssign(): Unit = {
    val x = LocalVariableItem("x",ArrayType(IntType))
    implicit val env = Env(List(x))
    testDen("x = {1,2,3}", AssignExp(None,x,ArrayExp(IntType,List(1,2,3))))
  }

  @Test
  def arrayLiteral(): Unit = {
    val Main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val main = LocalVariableItem("main",SimpleClassType(Main))
    val f = MethodItem("f",Main,VoidType,List(ArrayType(IntType)))
    implicit val env = Env(List(Main,main,f))
    testDen("f({1,2,3,4})", ApplyExp(MethodDen(main,f),List(ArrayExp(IntType,List(1,2,3,4)))))
  }

  @Test
  def makeAndSet(): Unit = {
    implicit val env = baseEnv
    testDen("x = 1; x = 2", env => {
      val x = env.exactLocal("x")
      List(VarStmt(IntType, List((x,Some(toExp(1))))),
           ExpStmt(AssignExp(None,x,2)))
    })
  }

  @Test
  def indexExp(): Unit = {
    val x = LocalVariableItem("x", ArrayType(CharType))
    implicit val env = Env(List(x))
    testDen("""x[4] = '\n'""", AssignExp(None,IndexExp(x,4),'\n'))
  }

  @Test
  def nestedIndexExpBrack(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = Env(List(x))
    testDen("""x[4,5] = x[2][5]""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpJuxt(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = Env(List(x))
    testDen("""x 4 5 = x 2 5""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpMixed(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = Env(List(x))
    testDen("""x{4,5} = x{2}[5]""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpParen(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = Env(List(x))
    testDen("""x(4,5) = x(2)(5)""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def indexOpExp(): Unit = {
    val x = LocalVariableItem("x", ArrayType(CharType))
    implicit val env = Env(List(x))
    testDen("""x[4] *= '\n'""", AssignExp(Some(MulOp()), IndexExp(x,4), '\n'))
  }

  @Test
  def mapExp(): Unit = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = StaticMethodItem("f",main,FloatType,List(ArrayType(IntType)))
    val x = LocalVariableItem("x",ArrayType(DoubleType))
    val y = LocalVariableItem("y",ArrayType(DoubleType))
    implicit val env = Env(List(main,f))
    testDen("y = f(x)", env => Nil)
    notImplemented
  }

  @Test
  def cons(): Unit = {
    implicit val env = baseEnv
    testDen("x = Object()", env => List(VarStmt(ObjectType,
      List((env.exactLocal("x"),Some(ApplyExp(NewDen(ObjectConsItem),Nil)))))))
  }

  @Test
  def genericConsObject(): Unit = {
    val T = TypeParamItem("T")
    val A = NormalClassItem("A",LocalPkg,List(T))
    val AC = ConstructorItem(A,List(ParamType(T)))
    implicit val env = baseEnv.addObjects(List(A,AC), Map((A,2), (AC,1)))
    testDen("x = A(Object())", env => List(VarStmt(GenericClassType(A,List(ObjectType)),
      List((env.exactLocal("x"),Some(ApplyExp(NewDen(AC),List(ApplyExp(NewDen(ObjectConsItem),Nil)))))))))
  }
}
