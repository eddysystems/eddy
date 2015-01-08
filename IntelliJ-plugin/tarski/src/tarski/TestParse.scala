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
import ambiguity.Locations._

class TestParse {
  val r = SRange.unknown
  val p = ParenAround
  def noLoc[A](x: Located[A]): Located[A] = Located(x.x,r)

  @Test
  def lexer(): Unit = {
    // Utilities
    def spaced(ts: List[Token]): List[Token] = ts match {
      case Nil|List(_) => ts
      case x :: xs => x :: WhitespaceTok(" ") :: spaced(xs)
    }
    def check(name: String, cons: String => Token, options: String) =
      assertEquals(spaced(splitWhitespace(options) map cons),lex(options) map (_.x))

    assertEquals(spaced(List(AbstractTok,FinalTok,DoTok)),lex("abstract final do") map (_.x))
    check("ints",IntLitTok,"0 1 17 0x81 07_43 0b1010_110")
    check("longs",LongLitTok,"0l 1L 17l 0x81L 07_43l 0b1010_110L")
    check("floats",FloatLitTok,"0f 5F 5.3f .4e-8F 0x4.aP1_7f")
    check("doubles",DoubleLitTok,"0d 5D 5.3 5.3d .4e-8 .4e-8D 0x4.aP1_7 0x4.aP1_7d")
    check("chars",CharLitTok,"""'x' '\t' '\n' '\0133'""")
    check("strings",StringLitTok,""""xyz" "\n\b\r\t" "\0\1\2"""")

    // We lex >> into GtTok RShiftSepTok GtTok.  Make sure we don't screw it up.
    def prep(s: String, ts: Token*) =
      assertEquals(ts.toList,prepare(lex(s)) map (_.x))
    prep(">",GtTok)
    prep(">>",GtTok,RShiftSepTok,GtTok)
    prep(">>>",GtTok,UnsignedRShiftSepTok,GtTok,UnsignedRShiftSepTok,GtTok)
    prep("> >",GtTok,GtTok)
    prep("> > >",GtTok,GtTok,GtTok)
    prep("> >>",GtTok,GtTok,RShiftSepTok,GtTok)
  }

  @Test
  def pretty(): Unit = {
    def check(s: String, e: AExp) = assertEquals(s,show(e))
    def add(x: AExp, y: AExp) = BinaryAExp(AddOp,x,y,r)
    def mul(x: AExp, y: AExp) = BinaryAExp(MulOp,x,y,r)

    check("1 + 2 + 3",     add(add(1,2),3))
    check("1 + (2 + 3)", add(1,add(2,3)))
    check("1 + 2 * 3",     add(1,mul(2,3)))
    check("1 * 2 + 3",     add(mul(1,2),3))
    check("1 * (2 + 3)", mul(1,add(2,3)))
    check("(1 + 2) * 3", mul(add(1,2),3))
  }

  def testAST(s: String, ss: List[AStmt]*): Unit = {
    val tokens = prepare(lex(s) map noLoc)
    println(s"tokens = $tokens")
    val asts = ParseEddy.parse(tokens)
    for (e <- ss if !asts.contains(e)) {
      println()
    }
    assertSetsEqual(ss,asts)
  }

  def testASTPossible(s: String, ss: List[AStmt]): Unit = {
    val tokens = prepare(lex(s) map noLoc)
    println(s"tokens = $tokens")
    val asts = ParseEddy.parse(tokens)
    assertIn(ss,asts.toSet)
  }

  def testBest(s: String, ss: List[AStmt]): Unit = {
    val clean = prepare(lex(s) map noLoc)
    val asts = Mismatch.repair(clean) flatMap (ts => uniform(Pr.parse,ParseEddy.parse(ts),"Parse failed"))
    asts.best match {
      case Left(e) => throw new RuntimeException("\n"+e.prefixed("error: "))
      case Right(ast) => assertEquals(ss,ast)
    }
  }

  @Test def hole() = testAST("",Nil)
  @Test def x() = testAST("x",NameAExp("x",r))

  @Test
  def nestApply() =
    testAST("x = A(Object())",
      AssignAExp(None,"x",ApplyAExp("A",SingleList(ApplyAExp("Object",EmptyList,p,r)),p,r),r),
      AssignAExp(None,"x",ApplyAExp("A",JuxtList(List("Object",ArrayAExp(EmptyList,p,r))),p,r),r))

  @Test
  def primTypes() =
    for (t <- List(VoidType,ByteType,ShortType,IntType,LongType,FloatType,DoubleType,CharType))
      testAST(show(t)+" x",
        VarAStmt(Nil,t,SingleList(("x",0,None)),r),
        ApplyAExp(t,SingleList("x"),NoAround,r))

  @Test
  def varArray() =
    testAST("int x[]",VarAStmt(Nil,IntType,SingleList(("x",1,None)),r),
                      ApplyAExp("int",SingleList(ApplyAExp("x",EmptyList,BrackAround,r)),NoAround,r))

  @Test def varField() = testAST("X().Y y",
    ApplyAExp("X",JuxtList(List(FieldAExp(ArrayAExp(EmptyList,ParenAround,r),None,"Y",r),"y")),NoAround,r),
    ApplyAExp(FieldAExp(ApplyAExp("X",EmptyList,ParenAround,r),None,"Y",r),SingleList("y"),NoAround,r),
    VarAStmt(Nil,FieldAExp(ApplyAExp("X",EmptyList,ParenAround,r),None,"Y",r),SingleList(("y",0,None)),r))

  // Precedence
  def add(x: AExp, y: AExp) = BinaryAExp(AddOp,x,y,r)
  def mul(x: AExp, y: AExp) = BinaryAExp(MulOp,x,y,r)
  @Test def addMul() = testAST("1 + 2 * 3", add(1,mul(2,3)))
  @Test def mulAdd() = testAST("1 * 2 + 3", add(mul(1,2),3))

  // Compound statements
  val t = NameAExp("true",r)
  val e = EmptyAStmt
  val h = HoleAStmt
  @Test def ifStmt()      = testAST("if (true);",IfAStmt(t,e,ParenAround,r))
  @Test def ifBare()      = testAST("if true;",IfAStmt(t,e,NoAround,r),IfElseAStmt(t,e,h,NoAround,r))
  @Test def ifElseHole()  = testAST("if (true) else", IfElseAStmt(t,h,h,ParenAround,r))
  @Test def whileBare()   = testAST("while true;", WhileAStmt(t,e,false,NoAround,r))
  @Test def doWhileBare() = testAST("do; while true", DoAStmt(e,t,false,NoAround,r))
  @Test def whileHole()   = testAST("while true", WhileAStmt(t,h,false,NoAround,r), WhileAStmt(t,e,false,NoAround,r))
  @Test def untilHole()   = testAST("until true", WhileAStmt(t,h,true,NoAround,r), WhileAStmt(t,e,true,NoAround,r),
                                                  ApplyAExp("until",SingleList(true),NoAround,r),
                                                  VarAStmt(Nil,"until",SingleList(("true",0,None)),r))
  @Test def forever()     = testAST("for (;;);", ForAStmt(For(Nil,None,Nil,r),e,ParenAround,r))
  @Test def foreverHole() = testAST("for (;;)", ForAStmt(For(Nil,None,Nil,r),h,ParenAround,r))
  @Test def forSimple()   = testAST("for (x=7;true;x++)",
    ForAStmt(For(AssignAExp(None,"x",7,r),Some(t),UnaryAExp(PostIncOp,"x",r),r),h,ParenAround,r))

  @Test def staticMethodOfObject() = testASTPossible("(X()).f();",
    ExpAStmt(ApplyAExp(FieldAExp(ParenAExp(ApplyAExp("X",EmptyList,ParenAround,r),ParenAround,r),None,"f",r),EmptyList,ParenAround,r)))

  @Test def weirdParens() = testAST("([{)]}",
    ParenAExp(ArrayAExp(SingleList(ArrayAExp(EmptyList,Grouped(Curly,Paren),r)),BrackAround,r),Grouped(Paren,Curly),r))

  @Test def thisForward() = testAST("this()",ApplyAExp("this",EmptyList,ParenAround,r))
  @Test def superForward() = testAST("super()",ApplyAExp("super",EmptyList,ParenAround,r))

  @Test def arrayVar() = {
    val rhs = ArrayAExp(CommaList(List(1,2,3)),CurlyAround,r)
    testAST("int[] x = {1,2,3}",
      VarAStmt(Nil,ApplyAExp("int",EmptyList,BrackAround,r),SingleList(("x",0,Some(rhs))),r),
      AssignAExp(None,ApplyAExp(ApplyAExp("int",EmptyList,BrackAround,r),SingleList("x"),NoAround,r),rhs,r))
  }

  @Test def genericType() =
    testASTPossible("X<String,A<String>> x = null",
      VarAStmt(Nil,TypeApplyAExp(NameAExp("X",r),CommaList(List("String",TypeApplyAExp("A",SingleList("String"),r,r))),r,r),
        SingleList(("x",0,Some(NameAExp("null",r)))),r))

  // We lex >> into GtTok GtNoSepTok GtTok.  Make sure we don't screw it up.
  @Test def rshift() = testAST("x >> y",BinaryAExp(RShiftOp,"x","y",r))
  @Test def urshift() = testAST("x >>> y",BinaryAExp(UnsignedRShiftOp,"x","y",r))
  @Test def rshiftSep() = testAST("x > > y")
  @Test def urshiftSep() = testAST("x >> > y")

  @Test def juxt() = testAST("a b c",
    ApplyAExp("a",JuxtList(List("b","c")),NoAround,r),
    VarAStmt(Nil,"a",JuxtList(List(("b",0,None),("c",0,None))),r))
}
