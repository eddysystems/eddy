package tarski

import ambiguity.Utility._
import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.AST._
import tarski.Base._
import tarski.Denotations._
import tarski.Environment.{Env, PlaceInfo}
import tarski.Items._
import tarski.Lexer._
import tarski.Operators._
import tarski.Scores._
import tarski.Tarski.fix
import tarski.TestUtils._
import tarski.Types._
import tarski.JavaScores._
import tarski.Semantics._

import scala.annotation.tailrec
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

  // Ensure that all options have probability at most bound
  def testFail(input: String, bound: Double = -1)(implicit env: Env) =
    checkFail(fix(lex(input)),bound)(_._2)
  @tailrec final def checkFail[A,B](s: Scored[A], bound: Double = -1)(f: A => B): Unit = if (s.p > bound) s match {
    case s:LazyScored[A] => checkFail(s force bound,bound)(f)
    case Best(p,x,_) => throw new AssertionError(s"\nExpected probability <= $bound, got\n  $p : ${f(x)}")
    case _:EmptyOrBad => ()
  }

  def probOf(s: Scored[(Env,List[Stmt])], a: List[Stmt]): Double = {
    def loop(s: Stream[Alt[(Env,List[Stmt])]]): Double =
      if (s.isEmpty) -1
      else if (a == s.head.x._2) s.head.p
      else loop(s.tail)
    loop(s.stream)
  }

  def assertFinal(v: Local) =
    assert(v.isFinal)

  @Test
  def assignExp(): Unit = {
    val x = Local("x",IntType,false)
    implicit val env = localEnv(x)
    testDen("x = 1", AssignExp(None,x,1))
  }

  @Test
  def assignExpFinal(): Unit = {
    val x = Local("x",IntType,true)
    implicit val env = localEnv(x)
    testFail("x = 1")
  }

  @Test
  def longLit() = {
    val x = Local("x",LongType,false)
    implicit val env = localEnv(x)
    testDen("x = 2l", AssignExp(None,x,LongLit(2,"2l")))
  }

  @Test
  def bigIntLit() = {
    val x = Local("x",LongType,false)
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
    val x = Local("x",ArrayType(IntType),isFinal=false)
    implicit val env = localEnv(x)
    testDen("x = {1,2,3}", AssignExp(None,x,ArrayExp(IntType,List(1,2,3))))
  }

  @Test
  def arrayLiteral(): Unit = {
    val Main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = NormalMethodItem("f",Main,Nil,VoidType,List(ArrayType(IntType)),isStatic=true)
    implicit val env = Env(Array(Main,f),Map((Main,2),(f,2)),PlaceInfo(f))
    testDen("f({1,2,3,4})", ApplyExp(f,List(ArrayExp(IntType,List(1,2,3,4)))))
  }

  @Test def arrayType() = {
    implicit val env = localEnvWithBase()
    testDen("int[] x = {1,2,3}", "x", x => VarStmt(ArrayType(IntType),(x,ArrayExp(IntType,List(1,2,3)))))
  }

  @Test def noIndex() = testFail("x = 1[]")

  @Test
  def makeAndSet() =
    testDen("x = 1; x = 2", "x", x =>
      List(VarStmt(IntType,(x,1)),
           ExpStmt(AssignExp(None,x,2))))

  @Test
  def indexExp(): Unit = {
    val x = Local("x",ArrayType(CharType),isFinal=true)
    implicit val env = localEnv(x)
    testDen("""x[4] = '\n'""", AssignExp(None,IndexExp(x,4),'\n'))
  }

  @Test
  def nestedIndexExpBrack(): Unit = {
    val x = new Local("x",ArrayType(ArrayType(CharType)),isFinal=true)
    implicit val env = localEnv(x)
    testDen("""x[4,5] = x[2][5]""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpJuxt(): Unit = {
    val x = new Local("x",ArrayType(ArrayType(CharType)),isFinal=true)
    implicit val env = localEnv(x)
    testDen("""x 4 5 = x 2 5""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpMixed(): Unit = {
    val x = new Local("x",ArrayType(ArrayType(CharType)),isFinal=true)
    implicit val env = Env(Array(x), Map((x,1)))
    testDen("""x{4,5} = x{2}[5]""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpParen(): Unit = {
    val x = new Local("x",ArrayType(ArrayType(CharType)),isFinal=true)
    implicit val env = localEnv(x)
    testDen("""x(4,5) = x(2)(5)""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def indexOpExp(): Unit = {
    val x = Local("x",ArrayType(CharType),isFinal=true)
    implicit val env = localEnv(x)
    testDen("""x[4] *= '\n'""", AssignExp(Some(MulOp), IndexExp(x,4), '\n'))
  }

  @Test
  def mapExp(): Unit = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = NormalMethodItem("f",main,Nil,FloatType,List(ArrayType(IntType)),isStatic=true)
    val x = Local("x",ArrayType(DoubleType),isFinal=true)
    val y = Local("y",ArrayType(DoubleType),isFinal=false)
    implicit val env = Env(Array(main,f,x,y), Map((main,2),(f,2),(x,1),(y,1)),PlaceInfo(f))
    testDen("y = f(x)", Nil)
    notImplemented
  }

  @Test
  def cons(): Unit = {
    implicit val env = localEnvWithBase()
    testDen("x = Object()", "x", x => VarStmt(ObjectType,(x,ApplyExp(NewDen(None,ObjectConsItem),Nil))))
  }

  @Test
  def genericConsObject(): Unit = {
    val T = SimpleTypeVar("T")
    lazy val A: ClassItem = NormalClassItem("A",LocalPkg,List(T),constructors=Array(AC))
    lazy val AC = NormalConstructorItem(A,Nil,List(T))
    implicit val env = localEnvWithBase().extend(Array(A,AC),Map((A,3),(AC,3)))
    // should result in A<Object> x = new A<Object>(new Object());
    testDen("x = A(Object())", "x", x => VarStmt(A.generic(List(ObjectType)),
      (x,ApplyExp(NewDen(None,AC,Some(List(ObjectType))),List(ApplyExp(NewDen(None,ObjectConsItem),Nil))))))
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

    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil, false, Set("f"))
    val Xf = NormalFieldItem("f",Q.simple,X,true)
    val Y = NormalClassItem("Y", LocalPkg, Nil, X.simple, Nil, false, Set("f"))
    val Yf = NormalFieldItem("f",R.simple,Y,true)

    val Z = NormalClassItem("Z", LocalPkg, Nil, ObjectType, Nil)
    val m = NormalMethodItem("m", Z, Nil, VoidType, List(Q.simple), false)
    val y = Local("y",Y.simple,true)
    implicit val env = Env(Array(X,Y,Z,Xf,Yf,m,y), Map((y,1),(m,2)))

    testDen("m(f)", ApplyExp(LocalMethodDen(m),List(FieldExp(CastExp(X.simple,y),Xf))))
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

    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil, false, Set("f"))
    val Xf = NormalFieldItem("f",Q.simple,X,true)
    val Y = NormalClassItem("Y", LocalPkg, Nil, X.simple, Nil, false, Set("f"))
    val Yf = NormalFieldItem("f",R.simple,Y,true)

    val m = NormalMethodItem("m", Y, Nil, VoidType, List(Q.simple), false)
    val This = ThisItem(Y)
    val Super = SuperItem(Y.base)
    implicit val env = Env(Array(X,Y,Xf,Yf,m,This,Super), Map((This,2),(Super,2),(m,2),(Y,2),(Yf,2),(X,3),(Xf,3)))

    testDen("m(f)", ApplyExp(LocalMethodDen(m),List(FieldExp(SuperExp(Super),Xf))))
  }

  @Test
  def thisExp(): Unit = {
    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val Xx = NormalFieldItem("x",IntType,X,false)
    val x = Local("x",StringType,false)
    val t = ThisItem(X)
    implicit val env = Env(Array(X,Xx,x,t), Map((x,1),(X,2),(t,2),(Xx,2)))

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
    val x = Local("x", IntType)

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
    val x = Local("x",IntType,false)
    implicit val env = extraEnv.extendLocal(Array(x))
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
    lazy val X: ClassItem = NormalClassItem("X",LocalPkg,constructors=Array(cons))
    lazy val cons = NormalConstructorItem(X,Nil,Nil)
    val f = NormalMethodItem("f",X,Nil,VoidType,Nil,true)

    implicit val env = Env(Array(X,cons,f),Map((f,2),(X,3)),PlaceInfo(f))
    // We are not allowed to discard the possible side effects in the X constructor.
    testDen("X().f();", List(ExpStmt(ApplyExp(MethodDen(ApplyExp(NewDen(None,cons),Nil),f),Nil))))
  }

  @Test def sideEffectsCons() = {
    lazy val X: ClassItem = NormalClassItem("X",LocalPkg,constructors=Array(cons))
    lazy val cons = NormalConstructorItem(X,Nil,Nil)
    val Y = NormalClassItem("Y",X,Nil)
    implicit val env = localEnv(X,cons,Y)
    testDen("X().Y y", "y", y => List(ExpStmt(ApplyExp(NewDen(None,cons),Nil)),VarStmt(Y,List((y,0,None)))))
  }

  @Test def sideEffectsFail() = {
    val x = Local("x",IntType,true)
    implicit val env = localEnv(x)
    testFail("x")
  }

  @Test def sideEffectsSplit() = {
    val x = Local("x",IntType,false)
    implicit val env = extraEnv.extendLocal(Array(x))
    testDen("true ? x = 1 : (x = 2)", IfElseStmt(true,AssignExp(None,x,1),AssignExp(None,x,2)))
  }

  @Test def trueTypo() = {
    implicit val env = localEnvWithBase()
    testDen("x = tru", "x", x => VarStmt(BooleanType,(x,true)))
  }

  // Synchronized
  @Test def sync() = testDen("synchronized null", SyncStmt(NullLit,HoleStmt))

  // inserting a cast to bool
  @Test def insertIntComparison() = testDen("if 1 then;", IfStmt(BinaryExp(NeOp,1,0), e))
  @Test def insertRefTypeComparison() = {
    val o = Local("o",ObjectType,true)
    implicit val env = localEnvWithBase(o)
    testDen("if o then;", IfStmt(BinaryExp(NeOp,o,NullLit), e))
  }

  @Test def shuffleArgs() = {
    val X = NormalClassItem("X", LocalPkg, Nil)
    val f = NormalMethodItem("f", X, Nil, VoidType, List(X.simple, DoubleType, StringType, BooleanType), true)
    val x = Local("x",X.simple,true)
    val d = Local("d",DoubleType,true)
    val s = Local("s",StringType,true)
    val b = Local("b",BooleanType,true)
    implicit val env = Env(Array(X,f),Map((X,3),(f,2)),PlaceInfo(f)).extendLocal(Array(x,d,s,b))
    testDen("f(s,b,d,x)", ApplyExp(f,List(x,d,s,b)))
  }

  @Test def omittedQualifier(): Unit = {
    val P = PackageItem("com.P", "com.P")
    val Z = NormalClassItem("Z", P, Nil)
    val Y = NormalClassItem("Y", LocalPkg, Nil)
    val X = NormalClassItem("X", LocalPkg, Nil)
    val Zx = NormalStaticFieldItem("x", BooleanType, Z, isFinal=false)
    val Yx = NormalStaticFieldItem("x", BooleanType, Y, isFinal=false)
    val Xx = NormalStaticFieldItem("x", BooleanType, X, isFinal=false)
    val f = NormalMethodItem("f", X, Nil, VoidType, Nil, isStatic=true)
    val x = Local("x", BooleanType, isFinal=false)
    implicit val env = baseEnv.extend(Array(P,Z,Zx,Y,Yx,X,Xx,f,x),Map((Y,3),(X,3),(Xx,2),(f,2),(x,1))).move(PlaceInfo(f))
    val fixes = fix(lex("x = true"))
    // make sure that local x is the most likely, then X.x (shadowed, but in scope), then Y.x (not in scope), then Z.x (different package)
    def set(e: Exp): List[Stmt] = AssignExp(None,e,true)
    val px = probOf(fixes,set(x))
    val pXx = probOf(fixes,set(FieldExp(None,Xx)))
    val pYx = probOf(fixes,set(FieldExp(None,Yx)))
    val pZx = probOf(fixes,set(FieldExp(None,Zx)))

    println(s"probabilities:\n  x   : $px\n  X.x : $pXx\n  Y.x : $pYx\n  Z.x : $pZx")
    assertTrue(s"All probabilities should be positive",px > 0 && pXx > 0 && pYx > 0 && pZx > 0)

    assertTrue(s"Local variable not more likely (x : $px) than shadowed field (X.x : $pXx)", px > pXx)
    assertTrue(s"Same class static field not more likely (X.x : $pXx) than other class field (Y.x : $pYx)", pXx > pYx)
    // The next may not be what we want. only learning will really figure this out
    assertTrue(s"Other class field not more likely (Y.x : $pYx) than other package field (Z.x : $pZx)", pYx > pZx)
  }

  @Test def capture() = {
    val T = SimpleTypeVar("T")
    val S = SimpleTypeVar("S")
    val A = NormalClassItem("A",LocalPkg,List(T))
    val B = NormalClassItem("B",LocalPkg)
    val F = NormalClassItem("F",LocalPkg)
    val f = NormalMethodItem("f",F,List(S),VoidType,List(S),true)
    for (w <- List(WildSub(),WildSub(B),WildSuper(B))) {
      val x = Local("x",A.generic(List(w)),true)
      implicit val env = localEnv(A,x,F,f)
      testDen("f(x)",ApplyExp(TypeApply(f,List(ObjectType)),List(x)))
    }
  }

  // Mismatched parentheses
  @Test def mismatchedParens() = {
    lazy val X: ClassItem = NormalClassItem("X",LocalPkg,constructors=Array(cons))
    lazy val cons = NormalConstructorItem(X,Nil,Nil)
    val f = NormalMethodItem("f",X,Nil,VoidType,Nil,false)
    implicit val env = localEnv(X,cons,f)
    val best = ApplyExp(MethodDen(ParenExp(ApplyExp(NewDen(None,cons),Nil)),f),Nil)
    testDen("((X()).f()",best)
    testDen("((X()).f(",best)
  }

  @Test def omittedEmptyCallParens() = {
    val X = NormalClassItem("X",LocalPkg)
    val f = NormalMethodItem("f",X,Nil,VoidType,Nil,false)
    implicit val env = localEnv(X,f)
    testDen("f", ApplyExp(LocalMethodDen(f),Nil))
  }

  @Test def constructorForward(): Unit = {
    lazy val Y: ClassItem = NormalClassItem("Y", LocalPkg, constructors = Array(Yc))
    lazy val Yc = NormalConstructorItem(Y,Nil,Nil)
    lazy val X: ClassItem = NormalClassItem("X", LocalPkg, Nil, Y, constructors = Array(Xc,Xc2))
    lazy val Xc = NormalConstructorItem(X, Nil, Nil)
    lazy val Xc2 = NormalConstructorItem(X, Nil, List(IntType))
    val This = ThisItem(X)
    val Super = SuperItem(Y)
    implicit val env = Env(Array(Y,Yc,X,Xc,This,Super),
                           Map((Xc,2),(Xc2,2),(X,2),(Y,3),(Yc,3),(This,2),(Super,2)),
                           PlaceInfo(Xc2))
    testDen("this()", ApplyExp(ForwardDen(Some(X.simple),Xc),Nil))
    testDen("super()", ApplyExp(ForwardDen(Some(Y.simple),Yc),Nil))
    // TODO: This should only work as the first statement of a different constructor, which is not tracked by the PlaceInfo right now
  }

  @Test def illegalConstructorForward(): Unit = {
    // cannot forward to constructor outside of constructor
    val Y = NormalClassItem("Y", LocalPkg)
    val Yc = NormalConstructorItem(Y,Nil,Nil)
    val X = NormalClassItem("X", LocalPkg, Nil, Y)
    val Xc = NormalConstructorItem(X, Nil, Nil)
    val f = NormalMethodItem("f", X, Nil, VoidType, Nil, isStatic = false)
    implicit val env = Env(Array(Y,Yc,X,Xc), Map((f,2),(Xc,2),(X,2),(Y,3),(Yc,3)), PlaceInfo(f))
    testFail("this()")
    testFail("super()")
  }

  def setupGenericClass(): Env = {
    /**
     * equivalent to:
     *
     * class B<A> {}
     *
     * class X<A,BA extends B<A>> {
     *   <T extends Number> A f(T) {
     *     <caret>
     *   }
     * }
     *
     */

    val A2 = SimpleTypeVar("A")
    val B = NormalClassItem("B", LocalPkg, List(A2))

    val A = SimpleTypeVar("A")
    val BA = NormalTypeVar("BA", B.generic(List(A)), Nil)
    val X = NormalClassItem("X", LocalPkg, List(A,BA))
    val This = ThisItem(X)

    val T = NormalTypeVar("T", NumberItem.simple, Nil)
    val f = NormalMethodItem("f", X, List(T), A, List(T), isStatic=false)
    baseEnv.extend(Array(A,A2,B,BA,X,T,f,This), Map((A,2),(BA,2),(X,2),(This,2),(T,2),(f,2))).move(PlaceInfo(f))
  }

  @Test def genericClass(): Unit = {
    implicit val env = setupGenericClass()
    val X = env.allItems.find(_.name == "X").get.asInstanceOf[NormalClassItem]
    val B = env.allItems.find(_.name == "B").get.asInstanceOf[NormalClassItem]
    testDen("X<String,B<String>> x = null", "x", x => VarStmt(X.generic(List(StringType,B.generic(List(StringType)))), List((x, 0, Some(NullLit)))))
  }

  @Test def genericMethod(): Unit = {
    implicit val env = setupGenericClass()
    val f = env.allItems.find(_.name == "f").get.asInstanceOf[NormalMethodItem]
    val This = env.allItems.find(_.isInstanceOf[ThisItem]).get.asInstanceOf[ThisItem]
    testDen("""this.<Integer>f(7)""",ApplyExp(TypeApply(MethodDen(This,f),List(IntType.box)),List(7)))
    testDen("""<Integer>f(7)""",     ApplyExp(TypeApply(LocalMethodDen(f),List(IntType.box)),List(7)))
    testDen("""f<Integer>(7)""",     ApplyExp(TypeApply(LocalMethodDen(f),List(IntType.box)),List(7)))
    // These three fail because f's type argument extends Number
    testFail("""this.<String>f("test")""")
    testFail("""<String>f("test")""")
    testFail("""f<String>("test")""")
  }

  @Test def typeApply(): Unit = {
    /*
     * class S {}
     * class T {}
     * class U {}
     * class X<A extends S, B extends T, C extends U> {}
     * class Y {
     *   void f() {
     *     X<S,T,U> x<caret>
     *   }
     * }
     */
    val S = NormalClassItem("S",LocalPkg)
    val T = NormalClassItem("T",LocalPkg)
    val U = NormalClassItem("U",LocalPkg)
    val AS = NormalTypeVar("A",S,Nil)
    val BT = NormalTypeVar("B",T,Nil)
    val CU = NormalTypeVar("C",U,Nil)
    val X = NormalClassItem("X",LocalPkg,List(AS,BT,CU))
    val Y = NormalClassItem("Y",LocalPkg)
    val f = NormalMethodItem("f",Y,Nil,VoidType,Nil,isStatic=false)

    implicit val env = Env(Array(S,T,U,AS,BT,CU,X,Y,f), Map((X,3),(Y,2),(f,2),(S,3),(T,3),(U,3)), PlaceInfo(f))
    // Until we make some fiddling happen (in which case this test should test probabilities), only A<S,T,U> should work.
    testDen("X<S,T,U> x", "x", x => VarStmt(X.generic(List(S,T,U)),List((x,0,None))))
    testDen("A<S,S,U> x", "x", x => VarStmt(X.generic(List(S,T,U)),List((x,0,None)))) // Unlikely, but should work
    def bad(s: String) = testFail(s,bound=1e-5)
    bad("A<S,S,S> x")
    bad("A<S,S,T> x")
    bad("A<S,S,U> x")
    bad("A<S,T,S> x")
    bad("A<S,T,T> x")
    bad("A<S,U,S> x")
    bad("A<S,U,T> x")
    bad("A<S,U,U> x")
    bad("A<T,S,S> x")
    bad("A<T,S,T> x")
    bad("A<T,S,U> x")
    bad("A<T,T,S> x")
    bad("A<T,T,T> x")
    bad("A<T,T,U> x")
    bad("A<T,U,S> x")
    bad("A<T,U,T> x")
    bad("A<T,U,U> x")
    bad("A<U,S,S> x")
    bad("A<U,S,T> x")
    bad("A<U,S,U> x")
    bad("A<U,T,S> x")
    bad("A<U,T,T> x")
    bad("A<U,T,U> x")
    bad("A<U,U,S> x")
    bad("A<U,U,T> x")
    bad("A<U,U,U> x")
  }

  @Test def boxInt() = {
    implicit val env = localEnvWithBase()
    testDen("Integer x = 1","x",x => VarStmt(IntType.box,(x,1)))
  }

  @Test def boxByte() = {
    implicit val env = localEnvWithBase()
    testDen("Byte x = 1","x",x => VarStmt(ByteType.box,(x,1)))
  }

  @Test def fizz() = {
    val A = NormalClassItem("A",LocalPkg)
    val fizz = NormalMethodItem("fizz",A,Nil,IntType,List(StringType,IntType.box,DoubleType.box),isStatic=true)
    val x = Local("x",IntType,true)
    val q = Local("q",DoubleType,true)
    implicit val env = baseEnv.extend(Array(A,fizz,x,q),Map(A->1,fizz->1,x->1,q->1)).move(PlaceInfo(fizz))
    testDen("""fizz "s" x q""",ApplyExp(fizz,List(StringLit("s","\"s\""),x,q)))
  }

  @Test def shadowedParameter() = {
    val A = NormalClassItem("A",LocalPkg)
    val B = NormalClassItem("B",LocalPkg)
    val C = NormalClassItem("C",LocalPkg)
    val f = NormalMethodItem("f",A,Nil,VoidType,List(B),isStatic=true)
    val bx = Local("x",B,true)
    val cx = Local("x",C,true)
    def env(bs: Int, cs: Int) = baseEnv.extend(Array(A,B,C,f,bx,cx),Map(bx->bs,cx->cs)).move(PlaceInfo(f))
    def unit(x: Unit) = x
    unit({ implicit val bad = env(bs=2,cs=1); testFail("f x") })
    unit({ implicit val good = env(bs=1,cs=2); testDen("f x",ApplyExp(f,List(bx))) })
  }

  @Test def memberToInfix() = {
    val A = NormalClassItem("A", LocalPkg)
    val f = NormalMethodItem("f", A, Nil, VoidType, List(A), isStatic=true)
    val a = Local("a", A, true)
    implicit val env = baseEnv.extend(Array(A,f,a),Map(A->2,f->2,a->1)).move(PlaceInfo(f))
    testDen("a f a", ApplyExp(MethodDen(a,f),List(a)))
  }

  @Test def javascriptStyleMember() = {
    val A = NormalClassItem("A", LocalPkg)
    val f = NormalFieldItem("f", A, A, isFinal=false)
    val a = Local("a", A, isFinal=true)
    implicit val env = baseEnv.extend(Array(A,f,a), Map(A->2,f->2,a->1))
    testDen("a[f] = a", AssignExp(None,FieldExp(a,f),a))
  }

  @Test def newObject() = testDen("new Object()",ApplyExp(NewDen(None,ObjectConsItem),Nil))
  @Test def newObjectBare() = testDen("new Object",ApplyExp(NewDen(None,ObjectConsItem),Nil))

  @Test def newGeneric() = {
    val T = SimpleTypeVar("T")
    val S = SimpleTypeVar("S")
    val B = NormalClassItem("B",LocalPkg)
    val C = NormalClassItem("C",LocalPkg)
    lazy val A: ClassItem = NormalClassItem("A",LocalPkg,List(T),constructors=Array(cons))
    lazy val cons = NormalConstructorItem(A,List(S),Nil)
    implicit val env = localEnv().extend(Array(A,cons,B,C),Map(A->1,B->1,C->1))
    testDen("x = new<C>A<B>","x",x =>
      VarStmt(A.generic(List(B)),(x,ApplyExp(TypeApply(NewDen(None,cons,Some(List(B))),List(C)),Nil))))
  }

  @Test def classInPackage() = {
    val P = PackageItem("P","P")
    lazy val A: ClassItem = NormalClassItem("A",P,constructors=Array(cons))
    lazy val cons = NormalConstructorItem(A,Nil,Nil)
    implicit val env = localEnv().extend(Array(P,A,cons),Map.empty)
    val e = ApplyExp(NewDen(None,cons),Nil)
    testDen("P.A()",e)
    testDen("new P.A()",e)
  }

  @Test def noLocalPkg(): Unit = {
    val is = localEnvWithBase().exactQuery("")
    if (is.nonEmpty) throw new AssertionError(s"Unexpected empty strings: ${is map (_.getClass)}")
  }

  @Test def fieldAccess() = {
    val T = NormalClassItem("T", LocalPkg)
    val x = NormalFieldItem("x", IntType, T, isFinal=false)
    val t = Local("t", T.simple, isFinal=true)
    implicit val env = localEnv().extendLocal(Array(T,x), 3).extendLocal(Array(t), 1)
    testDen("t.x = 1", AssignExp(None, FieldExp(t, x), 1))
  }
}
