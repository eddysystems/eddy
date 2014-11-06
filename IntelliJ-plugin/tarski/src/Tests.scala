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
import tarski.Pretty._
import ambiguity.Utility._

class Tests {
  //val testenv: Env = Environment.envFromFile("test.jenv")

  @BeforeClass
  def init(): Unit = {
    // this happens once
  }

  // Useful implicit conversions
  implicit def toAExp(i: Int): AExp = IntALit(i.toString)
  implicit def toExp(i: Int): Exp = IntLit(i,i.toString)
  implicit def toExp(c: Char): Exp = CharLit(c, "'" + escapeJava(c.toString) + "'")
  implicit def toExp(x: LocalVariableItem): Exp = LocalVariableExp(x)
  implicit def toExps[A](xs: List[A])(implicit to: A => Exp): List[Exp] = xs map to

  def testDen(input: String, best: Env => List[Stmt])(implicit env: Env): Unit = {
    val (env2,stmt) = fix(lex(input).filterNot(isSpace)).best.get
    assertEquals(best(env2),stmt)
  }
  def testDen(input: String, best: Exp)(implicit env: Env): Unit =
    testDen(input, env => List(ExpStmt(best)))

  def testOnlyDenotation(input: String, best: Env => List[Stmt])(implicit env: Env) = {
    val fixes = fix(lex(input).filterNot(isSpace))
    assertEquals(fixes.c.size, 1)
    val (env2,stmt) = fixes.best.get
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
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = MethodItem("f",main,VoidType,List(ArrayType(IntType)))
    implicit val env = Env(List(main,f))
    testDen("f({1,2,3,4})", env => List(VarStmt(ArrayType(IntType), List((env.exactLocal("x"),
      Some(ArrayExp(IntType,List(1,2,3,4))))))))
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
    implicit val env = baseEnv.addLocalObjects(List(A))
    testDen("x = A(Object())", env => List(VarStmt(GenericClassType(A,List(ObjectType)),
      List((env.exactLocal("x"),Some(ApplyExp(NewDen(AC),List(ApplyExp(NewDen(ObjectConsItem),Nil)))))))))
  }

  @Test
  def lexer(): Unit = {
    // Utilities
    def spaced(ts: List[Token]): List[Token] = ts match {
      case Nil|List(_) => ts
      case x :: xs => x :: WhitespaceTok(" ") :: spaced(xs)
    }
    def check(name: String, cons: String => Token, options: String) =
      assertEquals(spaced(splitWhitespace(options) map cons),lex(options))

    assertEquals(spaced(List(AbstractTok(),FinalTok(),DoTok())),lex("abstract final do"))
    check("ints",IntLitTok,"0 1 17l 0x81 07_43 0b1010_110")
    check("floats",FloatLitTok,"5.3 .4e-8 0x4.aP1_7")
    check("chars",CharLitTok,"""'x' '\t' '\n' '\0133'""")
    check("strings",StringLitTok,""""xyz" "\n\b\r\t" "\0\1\2"""")
  }

  @Test
  def pretty(): Unit = {
    def check(s: String, e: AExp) = assertEquals(s,show(tokens(e)))
    def add(x: AExp, y: AExp) = BinaryAExp(AddOp(),x,y)
    def mul(x: AExp, y: AExp) = BinaryAExp(MulOp(),x,y)

    check("1 + 2 + 3",     add(add(1,2),3))
    check("1 + ( 2 + 3 )", add(1,add(2,3)))
    check("1 + 2 * 3",     add(1,mul(2,3)))
    check("1 * 2 + 3",     add(mul(1,2),3))
    check("1 * ( 2 + 3 )", mul(1,add(2,3)))
    check("( 1 + 2 ) * 3", mul(add(1,2),3))
  }

  /*
  @Test
  def thisNameResolution(): Unit = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = FieldItem("f",main,FloatType)
    val f2 = LocalVariableItem("f",ObjectType)
    implicit val env = Env(List(main,f))

    // f2 shadows f, so we should only get the f
    testDenotation("f = 1", )

  }
  */
}
