package tarski

import org.testng.annotations.{BeforeClass, Test}
import org.testng.AssertJUnit._
import tarski.AST._
import Types.binaryType
import tarski.Items.{DoubleType, IntType}
import tarski.Lexer._
import tarski.Tokens._
import ambiguity.Utility._

class Tests {

  @BeforeClass
  def init(): Unit = {
    // this happens once

    // read default java environment from file
    // TODO
  }

  @Test
  def expressionTypes(): Unit = {
    val b: Boolean = false
    val i: Int = 4
    val d: Double = 1.0
    val s: String = "blah"

    val vals = (b,i,d,s)
    val ops = (AddOp(),MulOp(),RShiftOp(),LtOp(),EqOp(),AndOp(),AndAndOp())

    assert((i + i).getClass.getName == binaryType(new AddOp(), IntType, IntType).get.qualifiedName)
    assert((d + i).getClass.getName == binaryType(new AddOp(), DoubleType, IntType).get.qualifiedName)
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
}
