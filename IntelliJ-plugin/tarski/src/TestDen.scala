package tarski

import tarski.AST._
import tarski.Denotations._

import scala.language.implicitConversions
import org.testng.annotations.Test
import org.testng.AssertJUnit._

import tarski.Tarski.fix
import tarski.Environment.Env
import tarski.Base._
import tarski.Items._
import tarski.Lexer._
import tarski.Types._
import tarski.TestUtils._
import ambiguity.Utility._

class TestDen {
  // Default to an empty local environment
  implicit val env = localEnvWithBase(Nil)

  def testHelper[A](input: String, best: Env => A)(implicit env: Env, convert: A => List[Stmt]): Unit = {
    val fixes = fix(lex(input))
    //println(fixes)
    fixes.best match {
      case Left(e) => throw new RuntimeException("\n"+e.prefixed("error: "))
      case Right((env,s)) => assertEquals(convert(best(env)),s)
    }
  }

  type Local = LocalVariableItem
  def testDen[A](input: String, best: A)(implicit env: Env, c: A => List[Stmt]): Unit =
    testHelper(input, env => best)
  def testDen[A](input: String, x: Name, best: Local => A)(implicit env: Env, c: A => List[Stmt]): Unit =
    testHelper(input, env => best(env.exactLocal(x)))
  def testDen[A](input: String, x: Name, y: Name, best: (Local,Local) => A)(implicit env: Env, c: A => List[Stmt]): Unit =
    testHelper(input, env => best(env.exactLocal(x),env.exactLocal(y)))

  def testOnlyDen(input: String, best: Env => List[Stmt])(implicit env: Env) = {
    val fixes = fix(lex(input))
    assertEquals(fixes.all.right.get.length, 1)
    val (env2,stmt) = fixes.best.right.get
    assertEquals(stmt, best(env2))
  }

  def testNotDen(input: String, avoid: List[Stmt])(implicit env: Env): Unit = {
    fix(lex(input)).all match {
      case Left(_) => ()
      case Right(fs) =>
        assertFalse(s"denotation $avoid discards side effects of X()",
                    fs exists { case (p,(e,ds)) => ds==avoid })
    }
  }

  @Test
  def assignExp(): Unit = {
    val x = LocalVariableItem("x",IntType)
    implicit val env = localEnv(List(x))
    testDen("x = 1", AssignExp(None,x,1))
  }

  @Test
  def longLit() = {
    val x = LocalVariableItem("x",LongType)
    implicit val env = localEnv(List(x))
    testDen("x = 2l", AssignExp(None,x,LongLit(2,"2l")))
  }

  @Test
  def bigIntLit() = {
    val x = LocalVariableItem("x",LongType)
    implicit val env = localEnv(List(x))
    val big = 1099511627776L
    testDen(s"x = $big", AssignExp(None,x,LongLit(big,s"${big}L")))
  }

  @Test
  def variableStmt() =
    testDen("x = 1", "x", x => VarStmt(IntType,(x,1)))

  @Test
  def arrayVariableStmtCurly() =
    testDen("x = {1,2,3,4}", "x", x => VarStmt(ArrayType(IntType),(x,ArrayExp(IntType,List(1,2,3,4)))))

  @Test
  def arrayVariableStmtParen() =
    testDen("x = (1,2,3,4)", "x", x => VarStmt(ArrayType(IntType),(x,ArrayExp(IntType,List(1,2,3,4)))))

  @Test
  def arrayVariableStmtBare() =
    testDen("x = 1,2,3,4", "x", x => VarStmt(ArrayType(IntType),(x,ArrayExp(IntType,List(1,2,3,4)))))

  @Test
  def arrayVariableStmtBrack() =
    testDen("x = [1,2,3,4]", "x", x => VarStmt(ArrayType(IntType),(x,ArrayExp(IntType,List(1,2,3,4)))))

  @Test
  def arrayLiteralAssign(): Unit = {
    val x = LocalVariableItem("x",ArrayType(IntType))
    implicit val env = localEnv(List(x))
    testDen("x = {1,2,3}", AssignExp(None,x,ArrayExp(IntType,List(1,2,3))))
  }

  @Test
  def arrayLiteral(): Unit = {
    val Main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = StaticMethodItem("f",Main,Nil,VoidType,List(ArrayType(IntType)))
    implicit val env = Env(List(Main,f), Map((Main,2),(f,2)), f)
    testDen("f({1,2,3,4})", ApplyExp(StaticMethodDen(f),Nil,List(ArrayExp(IntType,List(1,2,3,4)))))
  }

  @Test
  def makeAndSet() =
    testDen("x = 1; x = 2", "x", x =>
      List(VarStmt(IntType,(x,1)),
           ExpStmt(AssignExp(None,x,2))))

  @Test
  def indexExp(): Unit = {
    val x = LocalVariableItem("x", ArrayType(CharType))
    implicit val env = localEnv(List(x))
    testDen("""x[4] = '\n'""", AssignExp(None,IndexExp(x,4),'\n'))
  }

  @Test
  def nestedIndexExpBrack(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = localEnv(List(x))
    testDen("""x[4,5] = x[2][5]""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpJuxt(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = localEnv(List(x))
    testDen("""x 4 5 = x 2 5""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpMixed(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = Env(List(x)).makeAllLocal
    testDen("""x{4,5} = x{2}[5]""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpParen(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = localEnv(List(x))
    testDen("""x(4,5) = x(2)(5)""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def indexOpExp(): Unit = {
    val x = LocalVariableItem("x", ArrayType(CharType))
    implicit val env = localEnv(List(x))
    testDen("""x[4] *= '\n'""", AssignExp(Some(MulOp), IndexExp(x,4), '\n'))
  }

  @Test
  def mapExp(): Unit = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = StaticMethodItem("f",main,Nil,FloatType,List(ArrayType(IntType)))
    val x = LocalVariableItem("x",ArrayType(DoubleType))
    val y = LocalVariableItem("y",ArrayType(DoubleType))
    implicit val env = Env(List(main,f,x,y), Map((main,2),(f,2),(x,1),(y,1)), f)
    testDen("y = f(x)", Nil)
    notImplemented
  }

  @Test
  def cons(): Unit = {
    implicit val env = localEnvWithBase(Nil)
    testDen("x = Object()", "x", x => VarStmt(ObjectType,(x,ApplyExp(NewDen(ObjectConsItem),Nil,Nil))))
  }

  @Test
  def genericConsObject(): Unit = {
    val T = new TypeParamItem("T")
    val A = NormalClassItem("A",LocalPkg,List(T))
    val AC = ConstructorItem(A,Nil,List(ParamType(T)))
    implicit val env = localEnvWithBase(Nil).addObjects(List(A,AC),Map((A,3),(AC,3)))
    testDen("x = A(Object())", "x", x => VarStmt(A.generic(List(ObjectType)),
      (x,ApplyExp(NewDen(AC),List(ObjectType),List(ApplyExp(NewDen(ObjectConsItem),Nil,Nil))))))
  }

  @Test
  def varArray() =
    testDen("int x[]", "x", x => VarStmt(IntType,List((x,1,None))))

  @Test
  def varArrayInit() =
    testDen("int x[] = {1,2,3}", "x", x => VarStmt(IntType,List((x,1,Some(ArrayExp(IntType,List(1,2,3)))))))

  @Test
  def nullInit() =
    testDen("x = null", "x", x => VarStmt(ObjectType,(x,NullLit)))

  @Test
  def inheritanceShadowing(): Unit = {
    /* corresponding to
      class Q {}
      class R {}

      class X {
        Q f;
      }
      class Y extends X {
        R f;
      }
      class Z {
        void m(Q d) {}
      }

      Y y;
      m(f); // should resolve to m( ((X)y).f )
     */

    val Q = NormalClassItem("Q", LocalPkg, Nil, ObjectType, Nil)
    val R = NormalClassItem("R", LocalPkg, Nil, ObjectType, Nil)

    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val Xf = FieldItem("f", Q.simple, X)
    val Y = NormalClassItem("Y", LocalPkg, Nil, X.simple, Nil)
    val Yf = FieldItem("f", R.simple, Y)

    val Z = NormalClassItem("Z", LocalPkg, Nil, ObjectType, Nil)
    val m = MethodItem("m", Z, Nil, VoidType, List(Q.simple))
    val y = LocalVariableItem("y", Y.simple)
    implicit val env = Env(List(X,Y,Z,Xf,Yf,m,y), Map((y,1),(m,2)))

    testDen("m(f)", ApplyExp(LocalMethodDen(m),Nil,List(FieldExp(CastExp(X.simple,y),Xf))))
  }

  @Test
  def thisToSuper(): Unit = {
    /* corresponding to
      class Q {}
      class R {}

      class X {
        Q f;
      }
      class Y extends X {
        R f;
        void m(Q d) {}

        ...
        m(f); // should resolve to m( super.f )
      }
     */

    val Q = NormalClassItem("Q", LocalPkg, Nil, ObjectType, Nil)
    val R = NormalClassItem("R", LocalPkg, Nil, ObjectType, Nil)

    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val Xf = FieldItem("f", Q.simple, X)
    val Y = NormalClassItem("Y", LocalPkg, Nil, X.simple, Nil)
    val Yf = FieldItem("f", R.simple, Y)

    val m = MethodItem("m", Y, Nil, VoidType, List(Q.simple))
    val tY = ThisItem(Y)
    implicit val env = Env(List(X,Y,Xf,Yf,m,tY), Map((tY,2),(m,2),(Y,2),(Yf,2),(X,3),(Xf,3)))

    testDen("m(f)", ApplyExp(LocalMethodDen(m),Nil,List(FieldExp(SuperExp(tY), Xf))))
  }

  @Test
  def thisExp(): Unit = {
    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val Xx = FieldItem("x", IntType, X)
    val x = LocalVariableItem("x", StringType)
    val t = ThisItem(X)
    implicit val env = Env(List(X,Xx,x,t), Map((x,1),(X,2),(t,2),(Xx,2)))

    testDen("x = 1", AssignExp(None,FieldExp(ThisExp(t),Xx),1))
  }

  /*
  @Test
  def relativeNames() = {
    val Z = NormalClassItem("Z", LocalPkg, Nil, ObjectType, Nil)
    val X = NormalClassItem("X", LocalPkg, Nil, SimpleClassType(Z), Nil)
    val S = NormalClassItem("S", LocalPkg, Nil, ObjectType, Nil)
    val Y = NormalClassItem("Y", S, Nil, SimpleClassType(X), Nil)
    val t = MethodItem("t", Y, VoidType, Nil)
    val Zx = FieldItem("x", IntType, Z)
    val Xx = FieldItem("x", IntType, X)
    val Sx = FieldItem("x", IntType, S)
    val Yx = FieldItem("x", IntType, Y)
    val x = LocalVariableItem("x", IntType)

    val Zxden =

    assertEquals(tokens(Zx), List(LParenTok(),LParenTok(), IdentTok("Z"), RParenTok(), ThisTok(), RParenTok(), DotTok(), IdentTok("x"))) // ((Z)this).x
    assertEquals(tokens(Xx), List(SuperTok(), DotTok(), IdentTok("x"))) // super.x
    assertEquals(tokens(Sx), List(IdentTok("S"), DotTok(), ThisTok(), DotTok(), IdentTok("x"))) // S.this.x
    assertEquals(tokens(Yx), List(ThisTok(), DotTok(), IdentTok("x"))) // this.x
    assertEquals(tokens(x), List(IdentTok("x"))) // Local x
  }
  */

  @Test
  def byteLiteral() = testDen("byte x = 3", "x", x => VarStmt(ByteType,(x,IntLit(3,"3"))))

  @Test
  def intLiteral() = testDen("int x = 3", "x", x => VarStmt(IntType,(x,IntLit(3,"3"))))

  @Test
  def parens() = testDen("(1)", ParenExp(1))

  // If statements
  val t = BooleanLit(true)
  val f = UnaryExp(NotOp,t)
  val e = EmptyStmt
  val h = HoleStmt
  @Test def ifStmt()       = testDen("if (true);", IfStmt(t,e))
  @Test def ifHole()       = testDen("if (true)", IfStmt(t,h))
  @Test def ifBare()       = testDen("if true;", IfStmt(t,e))
  @Test def ifBareHole()   = testDen("if true", IfStmt(t,h))
  @Test def ifThen()       = testDen("if true then;", IfStmt(t,e))
  @Test def ifThenHole()   = testDen("if true then", IfStmt(t,h))
  @Test def ifThenParens() = testDen("if (true) then", IfStmt(t,h))
  @Test def ifElse()       = testDen("if (true) 1 else 2", IfElseStmt(t,1,2))
  @Test def ifElseHole()   = testDen("if (true) else", IfElseStmt(t,h,h))
  @Test def ifThenElse()   = testDen("if true then 1 else 2", IfElseStmt(t,1,2))

  // While and do
  @Test def whileStmt()       = testDen("while (true);", WhileStmt(t,e))
  @Test def whileHole()       = testDen("while (true)", WhileStmt(t,h))
  @Test def whileBare()       = testDen("while true;", WhileStmt(t,e))
  @Test def whileBareHole()   = testDen("while true", WhileStmt(t,h))
  @Test def untilHole()       = testDen("until true", WhileStmt(f,h))
  @Test def doWhile()         = testDen("do; while (true)", DoStmt(e,t))
  @Test def doWhileHole()     = testDen("do while (true)", DoStmt(h,t))
  @Test def doWhileBare()     = testDen("do; while true", DoStmt(e,t))
  @Test def doWhileHoleBare() = testDen("do while true", DoStmt(h,t))
  @Test def doUntil()         = testDen("do until true", DoStmt(h,f))

  // For
  @Test def forever()     = testDen("for (;;);", ForStmt(Nil,None,Nil,EmptyStmt))
  @Test def foreverHole() = testDen("for (;;)", ForStmt(Nil,None,Nil,HoleStmt))
  @Test def forSimple()   = testDen("for (x=7;true;x++)", "x", x =>
    ForStmt(VarStmt(IntType,(x,7)),Some(t),UnaryExp(PostIncOp,x),HoleStmt))
  @Test def forTwo()      = testDen("for (x=7,y=8.1;true;)", "x", "y", (x,y) =>
    BlockStmt(List(VarStmt(IntType,(x,7)),
                   VarStmt(DoubleType,(y,8.1)),
                   ForStmt(Nil,Some(t),Nil,HoleStmt))))
  @Test def foreach()     = testDen("for (x : 1,2)", "x", x =>
    ForeachStmt(IntType,x,ArrayExp(IntType,List(1,2)),HoleStmt))

  @Test def sideEffects() = {
    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val cons = ConstructorItem(X,Nil,Nil)
    val f = StaticMethodItem("f",X,Nil,VoidType,Nil)

    implicit val env = Env(List(X,cons,f),Map((f,2),(X,3)),f)
    // We are not allowed to discard the possible side effects in the X constructor.
    testNotDen("(X()).f();", List(ExpStmt(ApplyExp(StaticMethodDen(f),Nil,Nil))))
  }

  // Synchronized
  @Test def sync() = testDen("synchronized null", SyncStmt(NullLit,HoleStmt))

  // inserting a cast to bool
  @Test def insertIntComparison() = testDen("if 1 then;", IfStmt(BinaryExp(NeOp,1,0), e))
  @Test def insertRefTypeComparison() = {
    val o = LocalVariableItem("o", ObjectType)
    implicit val env = localEnvWithBase(List(o))
    testDen("if o then;", IfStmt(BinaryExp(NeOp,o,NullLit), e))
  }

  @Test def shuffleArgs() = {
    val X = NormalClassItem("X", LocalPkg, Nil)
    val f = StaticMethodItem("f", X, Nil, VoidType, List(X.simple, DoubleType, StringType, BooleanType))
    val x = LocalVariableItem("x", X.simple)
    val d = LocalVariableItem("d", DoubleType)
    val s = LocalVariableItem("s", StringType)
    val b = LocalVariableItem("b", BooleanType)
    implicit val env = Env(List(X,f), Map((X,3),(f,2)), f).addLocalObjects(List(x,d,s,b))
    testDen("f(s, b, d, x)", ApplyExp(StaticMethodDen(f), Nil, List(x,d,s,b)))
  }

  @Test def capture() = {
    val T = new TypeParamItem("T")
    val S = new TypeParamItem("S")
    val A = NormalClassItem("A",LocalPkg,List(T))
    val x = LocalVariableItem("x",A.generic(List(WildSub())))
    val F = NormalClassItem("F",LocalPkg,Nil)
    val f = StaticMethodItem("f",F,List(S),VoidType,List(ParamType(S)))
    implicit val env = localEnv(List(A,x,F,f))
    testDen("f(x)",ApplyExp(StaticMethodDen(f),List(T),List(x)))
  }
}
