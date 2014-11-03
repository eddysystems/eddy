package tarski

import tarski.Denotations._

import scala.language.implicitConversions
import org.testng.annotations.{BeforeClass, Test}
import org.testng.AssertJUnit._

import tarski.Tarski.fix
import tarski.Environment.{Env,baseEnvironment}
import tarski.Items.{LocalVariableItem, IntType, LocalVariableItemImpl}
import tarski.Lexer._
import tarski.Tokens._
import tarski.Pretty._
import ambiguity.Utility._

class Tests {

  @BeforeClass
  def init(): Unit = {
    // this happens once

    // read default java environment from file
    // TODO
  }

  // Useful implicit conversions
  implicit def toExp(i: Int): AST.Exp = AST.LitExp(AST.IntLit(i.toString))
  implicit def toDen(i: Int): ExpDen = IntLit(i,i.toString)
  implicit def toDen(x: LocalVariableItem): ExpDen = LocalVariableExpDen(x)

  def testDenotation(input: String, best: Env => List[StmtDen])(implicit env: Env) = {
    val (env2,stmt) = fix(lex(input).filterNot(isSpace)).best.get
    assertEquals(stmt, best(env2))
  }

  @Test
  def assignExp(): Unit = {
    val x = new LocalVariableItemImpl("x", IntType)
    implicit val env = new Env(List(x))
    testDenotation("x = 1", env => List(ExprStmtDen(AssignExpDen(None,x,1))))
  }

  @Test
  def variableStmt(): Unit = {
    implicit val env = baseEnvironment
    testDenotation("x = 1", env => List(VarStmtDen(IntType, List((env.exactLocal("x"), Some(toDen(1)))))))
  }

  @Test
  def makeAndSet(): Unit = {
    implicit val env = baseEnvironment
    testDenotation("x = 1; x = 2", env => {
      val x = env.exactLocal("x")
      List(VarStmtDen(IntType, List((x,Some(toDen(1))))),
           ExprStmtDen(AssignExpDen(None,x,2)))
    })
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
    def check(s: String, e: AST.Exp) = assertEquals(s,show(tokens(e)))
    def add(x: AST.Exp, y: AST.Exp) = AST.BinaryExp(AST.AddOp(),x,y)
    def mul(x: AST.Exp, y: AST.Exp) = AST.BinaryExp(AST.MulOp(),x,y)

    check("1 + 2 + 3",     add(add(1,2),3))
    check("1 + ( 2 + 3 )", add(1,add(2,3)))
    check("1 + 2 * 3",     add(1,mul(2,3)))
    check("1 * 2 + 3",     add(mul(1,2),3))
    check("1 * ( 2 + 3 )", mul(1,add(2,3)))
    check("( 1 + 2 ) * 3", mul(add(1,2),3))
  }
}
