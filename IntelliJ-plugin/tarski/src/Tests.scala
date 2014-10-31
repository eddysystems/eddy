package tarski

import scala.language.implicitConversions
import com.intellij.util.SmartList
import org.testng.annotations.{BeforeClass, Test}
import org.testng.AssertJUnit._
import tarski.AST._

import tarski.Tarski.fix
import tarski.Environment.JavaEnvironment
import tarski.Items.{IntType, LocalVariableItemImpl}
import tarski.Lexer._
import tarski.Tokens._
import tarski.Pretty._
import ambiguity.Utility._

import scala.collection.JavaConverters._

class Tests {

  @BeforeClass
  def init(): Unit = {
    // this happens once

    // read default java environment from file
    // TODO
  }

  @Test
  def simpleExpression(): Unit = {
    val env = JavaEnvironment(List(new LocalVariableItemImpl("x", IntType)))
    println("environment: " + env)

    val tokens = new SmartList[Token](IdentTok("x"), WhitespaceTok(" "), EqTok(), WhitespaceTok(" "), IntLitTok("1"))

    val result = fix(tokens, env)
    val ast = ExpStmt(AssignExp(NameExp("x"), None, LitExp(IntLit("1"))))

    assert( result.asScala.toList.exists( p => p._1 == ast ))
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
    implicit def i(i: Int): Exp = LitExp(IntLit(i.toString))
    def check(s: String, e: Exp) = assertEquals(s,show(tokens(e)))
    def add(x: Exp, y: Exp) = BinaryExp(AddOp(),x,y)
    def mul(x: Exp, y: Exp) = BinaryExp(MulOp(),x,y)

    assertEquals(AddFix,fixity(AddOp()))
    assertEquals(AddFix,fixity(add(1,2)))
    check("1 + 2 + 3",     add(add(1,2),3))
    check("1 + ( 2 + 3 )", add(1,add(2,3)))
    check("1 + 2 * 3",     add(1,mul(2,3)))
    check("1 * 2 + 3",     add(mul(1,2),3))
    check("1 * ( 2 + 3 )", mul(1,add(2,3)))
    check("( 1 + 2 ) * 3", mul(add(1,2),3))
  }
}
