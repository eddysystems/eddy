package tarski

import ambiguity.Utility._
import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.AST._
import tarski.Base._
import tarski.Denotations._
import tarski.Environment.Env
import tarski.Items._
import tarski.Lexer._
import tarski.Operators._
import tarski.Scores.{Alt, Prob, Scored}
import tarski.Tarski.fix
import tarski.TestUtils._
import tarski.Types._

import scala.language.implicitConversions

class TestDen {
  // Default to an empty local environment
  implicit val env = localEnvWithBase()

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

  def testFail(input: String)(implicit env: Env) = {
    fix(lex(input)).all match {
      case Left(_) => ()
      case Right(fs) => throw new AssertionError(s"excepted no denotations, got $fs")
    }
  }

  def getProb[A](s: Scored[A], a: A): Prob = {
    if (s.all.isLeft)
      Prob(0.0)
    else
      s.all.right.get.find({ case Alt(p,i) => i == a }) match {
        case None => Prob(0.0)
        case Some(Alt(p,i)) => p
      }
  }

  def assertFinal(v: Local) =
    assert(v.isFinal)

  @Test
  def assignExp(): Unit = {
    val x = LocalVariableItem("x",IntType,false)
    implicit val env = localEnv(x)
    testDen("x = 1", AssignExp(None,x,1))
  }

  @Test
  def assignExpFinal(): Unit = {
    val x = LocalVariableItem("x",IntType,true)
    implicit val env = localEnv(x)
    testFail("x = 1")
  }

  @Test
  def longLit() = {
    val x = LocalVariableItem("x",LongType,false)
    implicit val env = localEnv(x)
    testDen("x = 2l", AssignExp(None,x,LongLit(2,"2l")))
  }

  @Test
  def bigIntLit() = {
    val x = LocalVariableItem("x",LongType,false)
    implicit val env = localEnv(x)
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
    val x = LocalVariableItem("x",ArrayType(IntType),false)
    implicit val env = localEnv(x)
    testDen("x = {1,2,3}", AssignExp(None,x,ArrayExp(IntType,List(1,2,3))))
  }

  @Test
  def arrayLiteral(): Unit = {
    val Main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = StaticMethodItem("f",Main,Nil,VoidType,List(ArrayType(IntType)))
    implicit val env = new Env(List(Main,f), Map((Main,2),(f,2)), f)
    testDen("f({1,2,3,4})", ApplyExp(StaticMethodDen(None,f),Nil,List(ArrayExp(IntType,List(1,2,3,4)))))
  }

  @Test
  def makeAndSet() =
    testDen("x = 1; x = 2", "x", x =>
      List(VarStmt(IntType,(x,1)),
           ExpStmt(AssignExp(None,x,2))))

  @Test
  def indexExp(): Unit = {
    val x = LocalVariableItem("x",ArrayType(CharType),true)
    implicit val env = localEnv(x)
    testDen("""x[4] = '\n'""", AssignExp(None,IndexExp(x,4),'\n'))
  }

  @Test
  def nestedIndexExpBrack(): Unit = {
    val x = new LocalVariableItem("x",ArrayType(ArrayType(CharType)),true)
    implicit val env = localEnv(x)
    testDen("""x[4,5] = x[2][5]""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpJuxt(): Unit = {
    val x = new LocalVariableItem("x",ArrayType(ArrayType(CharType)),true)
    implicit val env = localEnv(x)
    testDen("""x 4 5 = x 2 5""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpMixed(): Unit = {
    val x = new LocalVariableItem("x",ArrayType(ArrayType(CharType)),true)
    implicit val env = new Env(List(x), Map((x,1)))
    testDen("""x{4,5} = x{2}[5]""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpParen(): Unit = {
    val x = new LocalVariableItem("x",ArrayType(ArrayType(CharType)),true)
    implicit val env = localEnv(x)
    testDen("""x(4,5) = x(2)(5)""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def indexOpExp(): Unit = {
    val x = LocalVariableItem("x",ArrayType(CharType),true)
    implicit val env = localEnv(x)
    testDen("""x[4] *= '\n'""", AssignExp(Some(MulOp), IndexExp(x,4), '\n'))
  }

  @Test
  def mapExp(): Unit = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = StaticMethodItem("f",main,Nil,FloatType,List(ArrayType(IntType)))
    val x = LocalVariableItem("x",ArrayType(DoubleType),true)
    val y = LocalVariableItem("y",ArrayType(DoubleType),false)
    implicit val env = new Env(List(main,f,x,y), Map((main,2),(f,2),(x,1),(y,1)), f)
    testDen("y = f(x)", Nil)
    notImplemented
  }

  @Test
  def cons(): Unit = {
    implicit val env = localEnvWithBase()
    testDen("x = Object()", "x", x => VarStmt(ObjectType,(x,ApplyExp(NewDen(ObjectConsItem),Nil,Nil))))
  }

  @Test
  def genericConsObject(): Unit = {
    val T = SimpleTypeVar("T")
    val A = NormalClassItem("A",LocalPkg,List(T))
    val AC = ConstructorItem(A,Nil,List(T))
    implicit val env = localEnvWithBase().addObjects(List(A,AC),Map((A,3),(AC,3)))
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
    val Xf = FieldItem("f",Q.simple,X,true)
    val Y = NormalClassItem("Y", LocalPkg, Nil, X.simple, Nil)
    val Yf = FieldItem("f",R.simple,Y,true)

    val Z = NormalClassItem("Z", LocalPkg, Nil, ObjectType, Nil)
    val m = MethodItem("m", Z, Nil, VoidType, List(Q.simple))
    val y = LocalVariableItem("y",Y.simple,true)
    implicit val env = new Env(List(X,Y,Z,Xf,Yf,m,y), Map((y,1),(m,2)))

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
    val Xf = FieldItem("f",Q.simple,X,true)
    val Y = NormalClassItem("Y", LocalPkg, Nil, X.simple, Nil)
    val Yf = FieldItem("f",R.simple,Y,true)

    val m = MethodItem("m", Y, Nil, VoidType, List(Q.simple))
    val tY = ThisItem(Y)
    implicit val env = new Env(List(X,Y,Xf,Yf,m,tY), Map((tY,2),(m,2),(Y,2),(Yf,2),(X,3),(Xf,3)))

    testDen("m(f)", ApplyExp(LocalMethodDen(m),Nil,List(FieldExp(SuperExp(tY), Xf))))
  }

  @Test
  def thisExp(): Unit = {
    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val Xx = FieldItem("x",IntType,X,false)
    val x = LocalVariableItem("x",StringType,false)
    val t = ThisItem(X)
    implicit val env = new Env(List(X,Xx,x,t), Map((x,1),(X,2),(t,2),(Xx,2)))

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
  def parens() = testDen("x = (1)", "x", x => VarStmt(IntType,(x,ParenExp(1))))

  // If statements
  val t = BooleanLit(true)
  val f = NonImpExp(NotOp,t)
  val e = EmptyStmt
  val h = HoleStmt
  def testDenX(input: String, best: (Stmt,Stmt) => Stmt) = {
    val x = LocalVariableItem("x",IntType,false)
    implicit val env = localEnv(x)
    testDen(input,best(AssignExp(None,x,1),AssignExp(None,x,2)))
  }
  @Test def ifStmt()       = testDen ("if (true);", IfStmt(t,e))
  @Test def ifHole()       = testDen ("if (true)", IfStmt(t,h))
  @Test def ifBare()       = testDen ("if true;", IfStmt(t,e))
  @Test def ifBareHole()   = testDen ("if true", IfStmt(t,h))
  @Test def ifThen()       = testDen ("if true then;", IfStmt(t,e))
  @Test def ifThenHole()   = testDen ("if true then", IfStmt(t,h))
  @Test def ifThenParens() = testDen ("if (true) then", IfStmt(t,h))
  @Test def ifElse()       = testDenX("if (true) x=1 else x=2", (a,b) => IfElseStmt(t,a,b))
  @Test def ifElseHole()   = testDen ("if (true) else", IfElseStmt(t,h,h))
  @Test def ifThenElse()   = testDenX("if true then x=1 else x=2", (a,b) => IfElseStmt(t,a,b))

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
    ForStmt(VarStmt(IntType,(x,7)),Some(t),ImpExp(PostIncOp,x),HoleStmt))
  @Test def forTwo()      = testDen("for (x=7,y=8.1;true;)", "x", "y", (x,y) =>
    BlockStmt(List(VarStmt(IntType,(x,7)),
                   VarStmt(DoubleType,(y,8.1)),
                   ForStmt(Nil,Some(t),Nil,HoleStmt))))
  @Test def foreach()     = testDen("for (x : 1,2)", "x", x => {
    assertFinal(x); ForeachStmt(IntType,x,ArrayExp(IntType,List(1,2)),HoleStmt) })

  @Test def sideEffects() = {
    val X = NormalClassItem("X",LocalPkg)
    val cons = ConstructorItem(X,Nil,Nil)
    val f = StaticMethodItem("f",X,Nil,VoidType,Nil)

    implicit val env = new Env(List(X,cons,f),Map((f,2),(X,3)),f)
    // We are not allowed to discard the possible side effects in the X constructor.
    testDen("X().f();", List(ExpStmt(ApplyExp(StaticMethodDen(Some(ApplyExp(NewDen(cons),Nil,Nil)),f),Nil,Nil))))
  }

  @Test def sideEffectsCons() = {
    val X = NormalClassItem("X",LocalPkg)
    val cons = ConstructorItem(X,Nil,Nil)
    val Y = NormalClassItem("Y",X,Nil)
    implicit val env = localEnv(X,cons,Y)
    testDen("X().Y y", "y", y => List(ExpStmt(ApplyExp(NewDen(cons),Nil,Nil)),VarStmt(Y,List((y,0,None)))))
  }

  @Test def sideEffectsFail() = {
    val x = LocalVariableItem("x",IntType,true)
    implicit val env = localEnv(x)
    testFail("x")
  }

  @Test def sideEffectsSplit() = {
    val x = LocalVariableItem("x",IntType,false)
    implicit val env = localEnv(x)
    testDen("true ? x = 1 : (x = 2)", IfElseStmt(true,AssignExp(None,x,1),AssignExp(None,x,2)))
  }

  // Synchronized
  @Test def sync() = testDen("synchronized null", SyncStmt(NullLit,HoleStmt))

  // inserting a cast to bool
  @Test def insertIntComparison() = testDen("if 1 then;", IfStmt(BinaryExp(NeOp,1,0), e))
  @Test def insertRefTypeComparison() = {
    val o = LocalVariableItem("o",ObjectType,true)
    implicit val env = localEnvWithBase(o)
    testDen("if o then;", IfStmt(BinaryExp(NeOp,o,NullLit), e))
  }

  @Test def shuffleArgs() = {
    val X = NormalClassItem("X", LocalPkg, Nil)
    val f = StaticMethodItem("f", X, Nil, VoidType, List(X.simple, DoubleType, StringType, BooleanType))
    val x = LocalVariableItem("x",X.simple,true)
    val d = LocalVariableItem("d",DoubleType,true)
    val s = LocalVariableItem("s",StringType,true)
    val b = LocalVariableItem("b",BooleanType,true)
    implicit val env = new Env(List(X,f), Map((X,3),(f,2)), f).addLocalObjects(List(x,d,s,b))
    testDen("f(s,b,d,x)", ApplyExp(StaticMethodDen(None,f), Nil, List(x,d,s,b)))
  }

  @Test def omittedQualifier() = {
    val P = PackageItem("com.P", "com.P")
    val Z = NormalClassItem("Z", P, Nil)
    val Zx = StaticFieldItem("x", BooleanType, Z, false)
    val Y = NormalClassItem("Y", LocalPkg, Nil)
    val Yx = StaticFieldItem("x", BooleanType, Y, false)
    val X = NormalClassItem("X", LocalPkg, Nil)
    val Xx = StaticFieldItem("x", BooleanType, X, false)
    val f = StaticMethodItem("f", X, Nil, VoidType, Nil)
    val x = LocalVariableItem("x", BooleanType, false)
    implicit val env = new Env(List(P,Z,Zx,Y,Yx,X,Xx,f,x), Map((Y,3),(X,3),(Xx,2),(f,2),(x,1)),f)
    val fixes = fix(lex("x = true"))
    // make sure that local x is the most likely, then X.x (shadowed, but in scope), then Y.x (not in scope), then Z.x (different package)
    def set(e: Exp): List[Stmt] = List(ExpStmt(AssignExp(None,e,true)))
    val px = getProb(fixes,set(LocalVariableExp(x)))
    val pXx = getProb(fixes,set(StaticFieldExp(None,Xx)))
    val pYx = getProb(fixes,set(StaticFieldExp(None,Yx)))
    val pZx = getProb(fixes,set(StaticFieldExp(None,Zx)))

    println("probabilities: ", px, pXx, pYx, pZx)

    assertTrue(s"local variable not more likely ($px) than shadowed field ($pXx)", px > pXx)
    assertTrue(s"same class static field not more likely ($pXx) than other class field ($pYx)", pXx > pYx)
    assertTrue(s"other class field not more likely ($pYx) than other package field ($pZx)", pYx > pZx) // this may not be what we want. only learning will really figure this out
    notImplemented
  }

  @Test def capture() = {
    val T = SimpleTypeVar("T")
    val S = SimpleTypeVar("S")
    val A = NormalClassItem("A",LocalPkg,List(T))
    val B = NormalClassItem("B",LocalPkg)
    val F = NormalClassItem("F",LocalPkg)
    val f = StaticMethodItem("f",F,List(S),VoidType,List(S))
    for (w <- List(WildSub(),WildSub(B),WildSuper(B))) {
      val x = LocalVariableItem("x",A.generic(List(w)),true)
      implicit val env = localEnv(A,x,F,f)
      testDen("f(x)",ApplyExp(StaticMethodDen(None,f),List(ObjectType),List(x)))
    }
  }

  // Mismatched parentheses
  @Test def mismatchedParens() = {
    val X = NormalClassItem("X",LocalPkg)
    val cons = ConstructorItem(X,Nil,Nil)
    val f = MethodItem("f",X,Nil,VoidType,Nil)
    implicit val env = localEnv(X,cons,f)
    val best = ApplyExp(MethodDen(ParenExp(ApplyExp(NewDen(cons),Nil,Nil)),f),Nil,Nil)
    testDen("((X()).f()",best)
    testDen("((X()).f(",best)
  }
}
