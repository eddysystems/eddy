package tarski

import ambiguity.Utility._
import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.AST._
import tarski.Lexer._
import tarski.Operators._
import tarski.Pretty._
import tarski.Scores._
import tarski.TestUtils._
import tarski.Tokens._
import tarski.Types._

class TestParse {
  @Test
  def lexer(): Unit = {
    // Utilities
    def spaced(ts: List[Token]): List[Token] = ts match {
      case Nil|List(_) => ts
      case x :: xs => x :: WhitespaceTok(" ") :: spaced(xs)
    }
    def check(name: String, cons: String => Token, options: String) =
      assertEquals(spaced(splitWhitespace(options) map cons),lex(options))

    assertEquals(spaced(List(AbstractTok,FinalTok,DoTok)),lex("abstract final do"))
    check("ints",IntLitTok,"0 1 17 0x81 07_43 0b1010_110")
    check("longs",LongLitTok,"0l 1L 17l 0x81L 07_43l 0b1010_110L")
    check("floats",FloatLitTok,"0f 5F 5.3f .4e-8F 0x4.aP1_7f")
    check("doubles",DoubleLitTok,"0d 5D 5.3 5.3d .4e-8 .4e-8D 0x4.aP1_7 0x4.aP1_7d")
    check("chars",CharLitTok,"""'x' '\t' '\n' '\0133'""")
    check("strings",StringLitTok,""""xyz" "\n\b\r\t" "\0\1\2"""")
  }

  @Test
  def pretty(): Unit = {
    def check(s: String, e: AExp) = assertEquals(s,show(tokens(e)))
    def add(x: AExp, y: AExp) = BinaryAExp(AddOp,x,y)
    def mul(x: AExp, y: AExp) = BinaryAExp(MulOp,x,y)

    check("1 + 2 + 3",     add(add(1,2),3))
    check("1 + ( 2 + 3 )", add(1,add(2,3)))
    check("1 + 2 * 3",     add(1,mul(2,3)))
    check("1 * 2 + 3",     add(mul(1,2),3))
    check("1 * ( 2 + 3 )", mul(1,add(2,3)))
    check("( 1 + 2 ) * 3", mul(add(1,2),3))
  }

  def testAST(s: String, ss: List[AStmt]*): Unit = {
    val asts = ParseEddy.parse(lex(s).filterNot(isSpace).map(fake))
    for (e <- ss if !asts.contains(e)) {
      println()
    }
    assertSetsEqual(ss,asts)
  }

  def testASTPossible(s: String, ss: List[AStmt]): Unit = {
    val asts = ParseEddy.parse(lex(s).filterNot(isSpace).map(fake))
    assertIn(ss,asts.toSet)
  }

  def testBest(s: String, ss: List[AStmt]): Unit = {
    val clean = lex(s).filterNot(isSpace).map(fake)
    val asts = Mismatch.repair(clean) flatMap (ts => uniform(1,ParseEddy.parse(ts),"Parse failed"))
    asts.best match {
      case Left(e) => throw new RuntimeException("\n"+e.prefixed("error: "))
      case Right(ast) => assertEquals(ss,ast)
    }
  }

  @Test def hole() = testAST("",Nil)
  @Test def x() = testAST("x",NameAExp("x"))

  @Test
  def nestApply() =
    testAST("x = A(Object())",
      AssignAExp(None,"x",ApplyAExp("A",SingleList(ApplyAExp("Object",EmptyList)))),
      AssignAExp(None,"x",ApplyAExp("A",JuxtList(List("Object",ArrayAExp(EmptyList,ParenAround))))))

  @Test
  def primTypes() =
    for (t <- List(VoidType,ByteType,ShortType,IntType,LongType,FloatType,DoubleType,CharType))
      testAST(show(t)+" x",
        VarAStmt(Nil,t,SingleList(("x",0,None))),
        ApplyAExp(t,SingleList("x"),NoAround))

  @Test
  def varArray() =
    testAST("int x[]",VarAStmt(Nil,IntType,SingleList(("x",1,None))),
                      ApplyAExp("int",SingleList(ApplyAExp("x",EmptyList,BrackAround)),NoAround))

  @Test def varField() = testAST("X().Y y",
    ApplyAExp("X",JuxtList(List(FieldAExp(ArrayAExp(EmptyList,ParenAround),None,"Y"),"y")),NoAround),
    ApplyAExp(FieldAExp(ApplyAExp("X",EmptyList,ParenAround),None,"Y"),SingleList("y"),NoAround),
    VarAStmt(Nil,FieldAExp(ApplyAExp("X",EmptyList,ParenAround),None,"Y"),SingleList(("y",0,None))))

  // Precedence
  def add(x: AExp, y: AExp) = BinaryAExp(AddOp,x,y)
  def mul(x: AExp, y: AExp) = BinaryAExp(MulOp,x,y)
  @Test def addMul() = testAST("1 + 2 * 3", add(1,mul(2,3)))
  @Test def mulAdd() = testAST("1 * 2 + 3", add(mul(1,2),3))

  // Compound statements
  val t = NameAExp("true")
  val e = EmptyAStmt
  val h = HoleAStmt
  @Test def ifStmt()      = testAST("if (true);",IfAStmt(t,e,ParenAround))
  @Test def ifBare()      = testAST("if true;",IfAStmt(t,e,NoAround),IfElseAStmt(t,e,h,NoAround))
  @Test def ifElseHole()  = testAST("if (true) else", IfElseAStmt(t,h,h,ParenAround))
  @Test def whileBare()   = testAST("while true;", WhileAStmt(t,e,false,NoAround))
  @Test def doWhileBare() = testAST("do; while true", DoAStmt(e,t,false,NoAround))
  @Test def whileHole()   = testAST("while true", WhileAStmt(t,h,false,NoAround), WhileAStmt(t,e,false,NoAround))
  @Test def untilHole()   = testAST("until true", WhileAStmt(t,h,true,NoAround), WhileAStmt(t,e,true,NoAround),
                                                  ApplyAExp("until",SingleList(true),NoAround),
                                                  VarAStmt(Nil,"until",SingleList(("true",0,None))))
  @Test def forever()     = testAST("for (;;);", ForAStmt(For(Nil,None,Nil),e,ParenAround))
  @Test def foreverHole() = testAST("for (;;)", ForAStmt(For(Nil,None,Nil),h,ParenAround))
  @Test def forSimple()   = testAST("for (x=7;true;x++)",
    ForAStmt(For(AssignAExp(None,"x",7),Some(t),UnaryAExp(PostIncOp,"x")),h,ParenAround))

  @Test def staticMethodOfObject() = testASTPossible("(X()).f();",
    ExpAStmt(ApplyAExp(FieldAExp(ParenAExp(ApplyAExp(NameAExp("X"),EmptyList,ParenAround),ParenAround),None,"f"),EmptyList,ParenAround)))

  @Test def weirdParens() = testAST("([{)]}",
    ParenAExp(ArrayAExp(SingleList(ArrayAExp(EmptyList,Grouped(Curly,Paren))),BrackAround),Grouped(Paren,Curly)))
}
