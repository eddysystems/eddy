package tarski

import tarski.Mods.Final
import utility.Locations._
import utility.Utility._
import tarski.AST._
import tarski.Base._
import tarski.Denotations._
import tarski.Environment.{Env, PlaceInfo}
import tarski.Items._
import tarski.JavaScores._
import tarski.Lexer._
import tarski.Operators._
import tarski.Pretty._
import tarski.Scores._
import tarski.Tarski.fix
import tarski.TestUtils._
import tarski.Tokens._
import tarski.Types._
import org.testng.annotations.Test
import org.testng.AssertJUnit._
import scala.annotation.tailrec

class TestDen {
  // Default to an empty local environment
  implicit val env = localEnvWithBase()
  implicit val showFlags = abbrevShowFlags

  // Dummy ranges
  val r = SRange.unknown
  val a = SGroup.unknown

  // By default, we ignore locations
  case class TrackLoc(on: Boolean)
  implicit val tl = TrackLoc(on=false)

  // Ideally, we'd run a full fixpoint test on the location tracking to verify that everything flows through correctly.
  // Unfortunately, we're lazy in several places (new, commas, types), so location tracking is not perfect.  Even weak
  // fixpoint tests (without location tracking) are useful though, and hopefully we can turn on the full version later.
  val locationFixpoint = false

  def fp(p: Prob) = pp(p) + (if (!trackProbabilities) "" else s"\n${ppretty(p).prefixed("  ")}")

  def testHelper[A](input: String, best: Env => A, margin: Double = .9)
                   (implicit env: Env, convert: A => List[Stmt], tl: TrackLoc): Unit = {
    val ts: Tokens = lex(input)
    val fixes = fix(if (tl.on) ts else ts map (x => Loc(x.x,r)))
    fixes.strict match {
      case e:EmptyOrBad => throw new RuntimeException(s"Denotation test failed:\ninput: $input\n"+e.error.prefixed("error: "))
      case Best(p,(env2,s),rest) =>
        val b = convert(best(env2))
        def sh(s: List[Stmt]) = show(s)(prettyStmts(_)(env2),showFlags)
        val sb = sh(b)
        if (b != s) {
          val ep = probOf(fixes,env => convert(best(env)))
          throw new AssertionError(s"Denotation test failed:\ninput: $input" +
                                   s"\nexpected: $sb\nactual  : ${sh(s)}" +
                                   s"\nexpected p = ${fp(ep)}" +
                                   s"\nactual   p = ${fp(p)}" +
                                   s"\nexpected (full): $b\nactual   (full): $s")
        } else if (!rest.below(margin*pp(p))) {
          val n = rest.stream.head
          throw new AssertionError(s"Denotation test failed:\ninput: $input" +
                                   s"\nwanted margin $margin, got ${pp(n.dp)} / ${pp(p)} = ${pp(n.dp) / pp(p)}" +
                                   s"\nbest: $sb\nnext: ${sh(n.x._2)}" +
                                   s"\nbest p = ${fp(p)}" +
                                   s"\nnext p = ${fp(n.dp)}")
        }
        // Verify that locations are correctly threaded, by rerunning fix with full locations
        val ts2 = lex(sb)
        def ignore(x: Loc[Token]): Boolean = isSpace(x.x) || x.x==HoleTok
        fix(ts2)(env).strict match {
          case e:EmptyOrBad => throw new RuntimeException(s"Fixpoint test failed:\n"
                                                        + s"input: ${print(ts2 map (_.x))}\n"
                                                        + s"ts2: ${ts2 mkString " "}\n"
                                                        + s"ts2: ${ts2 filterNot ignore mkString " "}\n"
                                                        + e.error.prefixed("error: "))
          case Best(_,(env3,s3),_) =>
            val ts3 = tokens(s3)(prettyStmts(_)(env3))
            val i2 = ts2 filterNot ignore
            val i3 = ts3 filterNot ignore
            val si2 = i2 map (_.x)
            val si3 = i3 map (_.x)
            if (if (locationFixpoint) i2 != i3 else si2 != si3) {
              println(s"Fixpoint not reached:\nsb = $sb")
              if (locationFixpoint) assertEquals(i2,i3)
              else assertEquals(si2,si3)
            }
        }
    }
  }

  def test[A](input: String, best: A, margin: Double = .9)(implicit env: Env, c: A => List[Stmt], tl: TrackLoc): Unit =
    testHelper(input, env => best, margin=margin)
  def test[A](input: String, x: Name, best: Local => A)(implicit env: Env, c: A => List[Stmt], tl: TrackLoc): Unit =
    testHelper(input, env => best(env.exactLocal(x)))
  def test[A](input: String, x: Name, y: Name, best: (Local,Local) => A)(implicit env: Env, c: A => List[Stmt], tl: TrackLoc): Unit =
    testHelper(input, env => best(env.exactLocal(x),env.exactLocal(y)))

  // Ensure that all options have probability at most bound
  def testFail(input: String, bound: Double = -1)(implicit env: Env): Unit = {
    @tailrec def checkFail[A,B](s: Scored[A], bound: Double = -1)(f: A => B): Unit = if (s.p > bound) s match {
      case s:LazyScored[A] => checkFail(s force bound,bound)(f)
      case Best(p,x,_) => throw new AssertionError(s"\nExpected probability <= $bound for $input, got\n  $p : ${f(x)}")
      case _:EmptyOrBad => ()
    }
    checkFail(fix(lex(input)),bound)(x => s"${show(x._2)} (${x._2})")
  }

  // Ensure that an option doesn't appear
  def testAvoid[A](input: String, bad: A)(implicit env: Env, convert: A => List[Stmt]): Unit = {
    val b = convert(bad)
    val p = probOf(fix(lex(input)),b)
    if (pp(p) >= 0)
      throw new AssertionError(s"Avoidance test failed:\ninput: $input" +
                               s"\nbad: ${show(b)}" +
                               s"\nbad p = ${fp(p)}" +
                               s"\nbad (full): $b")
  }

  def probOf(s: Scored[(Env,List[Stmt])], a: Env => List[Stmt]): Prob = {
    def loop(s: Stream[Alt[(Env,List[Stmt])]]): Prob =
      if (s.isEmpty) Prob("not found",-1)
      else {
        val (env,ss) = s.head.x
        if (a(env) == ss) s.head.dp
        else loop(s.tail)
      }
    loop(s.stream)
  }
  def probOf(s: Scored[(Env,List[Stmt])], a: List[Stmt]): Prob =
    probOf(s,env => a)

  def assertFinal(v: Local) =
    assert(v.isFinal)

  @Test
  def assignExp(): Unit = {
    val x = NormalLocal("x",IntType,isFinal=false)
    implicit val env = localEnv(x)
    test("x = 1", AssignExp(None,r,x,1))
  }

  @Test
  def assignExpFinal(): Unit = {
    val x = NormalLocal("x",IntType,isFinal=true)
    implicit val env = localEnv(x)
    testFail("x = 1")
  }

  @Test
  def longLit() = {
    val x = NormalLocal("x",LongType,isFinal=false)
    implicit val env = localEnv(x)
    test("x = 2l", AssignExp(None,r,x,LongLit(2,"2l",r)))
  }

  @Test
  def bigIntLit() = {
    val x = NormalLocal("x",LongType,isFinal=false)
    implicit val env = localEnv(x)
    val big = 1099511627776L
    test(s"x = $big", AssignExp(None,r,x,LongLit(big,s"${big}L",r)))
  }

  @Test
  def variableStmt() =
    test("x = 1", "x", x => VarStmt(IntType,r,(x,1)))

  @Test
  def arrayVariableStmtCurly() =
    test("x = {1,2,3,4}", "x", x => VarStmt(ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3,4),a))))

  @Test
  def arrayVariableStmtParen() =
    test("x = (1,2,3,4)", "x", x => VarStmt(ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3,4),a))))

  @Test
  def arrayVariableStmtBare() =
    test("x = 1,2,3,4", "x", x => VarStmt(ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3,4),a))))

  @Test
  def arrayVariableStmtBrack() =
    test("x = [1,2,3,4]", "x", x => VarStmt(ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3,4),a))))

  @Test
  def arrayLiteralAssign(): Unit = {
    val x = NormalLocal("x",ArrayType(IntType),isFinal=false)
    implicit val env = localEnv(x)
    test("x = {1,2,3}", AssignExp(None,r,x,ArrayExp(IntType,List(1,2,3),a)))
  }

  @Test
  def arrayLiteral(): Unit = {
    val Main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = NormalMethodItem("f",Main,Nil,VoidType,List(ArrayType(IntType)),isStatic=true)
    implicit val env = Env(Array(Main,f),Map((Main,2),(f,2)),PlaceInfo(f))
    test("f({1,2,3,4})", ApplyExp(f,List(ArrayExp(IntType,List(1,2,3,4),a)),a,auto=false))
  }

  @Test def arrayType() = {
    implicit val env = localEnvWithBase()
    test("int[] x = {1,2,3}", "x", x => VarStmt(ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3),a))))
  }

  @Test def noIndex() = testFail("x = 1[]")

  @Test
  def makeAndSet() =
    test("x = 1; x = 2", "x", x =>
      List(SemiStmt(VarStmt(IntType,r,(x,1)),r),
           ExpStmt(AssignExp(None,r,x,2))))

  @Test
  def indexExp(): Unit = {
    val x = NormalLocal("x",ArrayType(CharType),isFinal=true)
    implicit val env = localEnv(x)
    test("""x[4] = '\n'""", AssignExp(None,r,IndexExp(x,4,a),'\n'))
  }

  @Test
  def nestedIndexExpBrack(): Unit = {
    val x = NormalLocal("x",ArrayType(ArrayType(CharType)),isFinal=true)
    implicit val env = localEnv(x)
    test("""x[4,5] = x[2][5]""", AssignExp(None,r,IndexExp(IndexExp(x,4,a),5,a),IndexExp(IndexExp(x,2,a),5,a)))
  }

  @Test
  def nestedIndexExpJuxt(): Unit = {
    val x = NormalLocal("x",ArrayType(ArrayType(CharType)),isFinal=true)
    implicit val env = localEnv(x)
    test("""x 4 5 = x 2 5""", AssignExp(None,r,IndexExp(IndexExp(x,4,a),5,a),IndexExp(IndexExp(x,2,a),5,a)))
  }

  @Test
  def nestedIndexExpMixed(): Unit = {
    val x = NormalLocal("x",ArrayType(ArrayType(CharType)),isFinal=true)
    implicit val env = Env(Array(x), Map((x,1)))
    test("""x{4,5} = x{2}[5]""", AssignExp(None,r,IndexExp(IndexExp(x,4,a),5,a),IndexExp(IndexExp(x,2,a),5,a)))
  }

  @Test
  def nestedIndexExpParen(): Unit = {
    val x = NormalLocal("x",ArrayType(ArrayType(CharType)),isFinal=true)
    implicit val env = localEnv(x)
    test("""x(4,5) = x(2)(5)""", AssignExp(None,r,IndexExp(IndexExp(x,4,a),5,a),IndexExp(IndexExp(x,2,a),5,a)))
  }

  @Test
  def indexOpExp(): Unit = {
    val x = NormalLocal("x",ArrayType(CharType),isFinal=true)
    implicit val env = localEnv(x)
    test("""x[4] *= '\n'""", AssignExp(Some(MulOp),r,IndexExp(x,4,a),'\n'))
  }

  @Test
  def mapExp(): Unit = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = NormalMethodItem("f",main,Nil,FloatType,List(ArrayType(IntType)),isStatic=true)
    val x = NormalLocal("x",ArrayType(DoubleType),isFinal=true)
    val y = NormalLocal("y",ArrayType(DoubleType),isFinal=false)
    implicit val env = Env(Array(main,f,x,y), Map((main,2),(f,2),(x,1),(y,1)),PlaceInfo(f))
    test("y = f(x)", Nil)
    notImplemented
  }

  @Test
  def cons(): Unit = {
    implicit val env = localEnvWithBase()
    test("x = Object()", "x", x => VarStmt(ObjectType,r,(x,ApplyExp(NewDen(r,None,ObjectConsItem,r),Nil,a,auto=false))))
  }

  @Test
  def genericConsObject(): Unit = {
    val T = SimpleTypeVar("T")
    lazy val A: ClassItem = NormalClassItem("A",LocalPkg,List(T),constructors=Array(AC))
    lazy val AC = NormalConstructorItem(A,Nil,List(T))
    implicit val env = localEnvWithBase().extend(Array(A,AC),Map((A,3),(AC,3)))
    // should result in A<Object> x = new A<Object>(new Object());
    test("x = A(Object())", "x", x => VarStmt(A.generic(List(ObjectType)),r,
      (x,ApplyExp(NewDen(r,None,AC,r,Some(Grouped(List(ObjectType),a))),
                  List(ApplyExp(NewDen(r,None,ObjectConsItem,r),Nil,a,auto=false)),a,auto=false))))
  }

  @Test
  def varArray() =
    test("int x[]", "x", x => VarStmt(IntType,r,VarDecl(x,r,1,None)))

  @Test
  def varArrayInit() =
    test("int x[] = {1,2,3}", "x", x => VarStmt(IntType,r,VarDecl(x,r,1,Some(r,ArrayExp(IntType,List(1,2,3),a)))))

  @Test
  def nullInit() =
    test("x = null", "x", x => VarStmt(ObjectType,r,(x,NullLit(r))))

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
    val y = NormalLocal("y",Y.simple,isFinal=true)
    implicit val env = Env(Array(X,Y,Z,Xf,Yf,m,y), Map((y,1),(m,2)))

    test("m(f)", ApplyExp(LocalMethodDen(m,r),List(FieldExp(CastExp(X.simple,a,y),Xf,r)),a,auto=false))
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

    test("m(f)", ApplyExp(LocalMethodDen(m,r),List(FieldExp(SuperExp(Super,r),Xf,r)),a,auto=false))
  }

  @Test
  def thisExp(): Unit = {
    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val Xx = NormalFieldItem("x",IntType,X,isFinal=false)
    val x = NormalLocal("x",StringType,isFinal=false)
    val t = ThisItem(X)
    implicit val env = Env(Array(X,Xx,x,t), Map((x,1),(X,2),(t,2),(Xx,2)))

    test("x = 1", AssignExp(None,r,FieldExp(ThisExp(t,r),Xx,r),1))
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
  def byteLiteral() = test("byte x = 3", "x", x => VarStmt(ByteType,r,(x,IntLit(3,"3",r))))

  @Test
  def intLiteral() = test("int x = 3", "x", x => VarStmt(IntType,r,(x,IntLit(3,"3",r))))

  @Test
  def parens() = test("x = (1)", "x", x => VarStmt(IntType,r,(x,ParenExp(1,a))))

  // If statements
  val t = BooleanLit(true,r)
  val f = BooleanLit(false,r)
  val e = EmptyStmt(r)
  val es = SemiStmt(e,r)
  val h = HoleStmt(r)
  val he = HoleStmt(SRange.empty)
  def testX(input: String, best: (Stmt,Stmt) => Stmt) = {
    val x = NormalLocal("x",IntType,isFinal=false)
    implicit val env = extraEnv.extendLocal(Array(x))
    test(input,best(AssignExp(None,r,x,1),AssignExp(None,r,x,2)))
  }
  @Test def ifStmt()       = test ("if (true);", IfStmt(r,t,a,es))
  @Test def ifHole()       = test ("if (true)", IfStmt(r,t,a,h))
  @Test def ifBare()       = test ("if true;", SemiStmt(IfStmt(r,t,a,e),r), margin=.99)
  @Test def ifBareHole()   = test ("if true", IfStmt(r,t,a,h))
  @Test def ifThen()       = test ("if true then;", IfStmt(r,t,a,es))
  @Test def ifThenHole()   = test ("if true then", IfStmt(r,t,a,h))
  @Test def ifThenParens() = test ("if (true) then", IfStmt(r,t,a,h))
  @Test def ifElse()       = testX("if (true) x=1 else x=2", (x,y) => IfElseStmt(r,t,a,x,r,y))
  @Test def ifElseHole()   = test ("if (true) else", IfElseStmt(r,t,a,he,r,h))
  @Test def ifThenElse()   = testX("if true then x=1 else x=2", (x,y) => IfElseStmt(r,t,a,x,r,y))
  @Test def elif()         = testX("if (true) x=1 elif (false) x=2", (x,y) => IfElseStmt(r,t,a,x,r,IfStmt(r,f,a,y)))
  @Test def elifBraces()   = testX("if (true) { x=1; } elif false { x=2 }", (x,y) =>
    IfElseStmt(r,t,a,BlockStmt(SemiStmt(x,r),a),r,IfStmt(r,f,a,BlockStmt(y,a))))
  @Test def ifBraces()     = testX("if true { x=1 }", (x,y) => IfStmt(r,t,a,BlockStmt(x,a)))

  // While and do
  @Test def whileStmt()       = test("while (true);", WhileStmt(r,t,a,es))
  @Test def whileHole()       = test("while (true)", WhileStmt(r,t,a,h))
  @Test def whileBare()       = test("while true;", SemiStmt(WhileStmt(r,t,a,e),r))
  @Test def whileBareHole()   = test("while true", WhileStmt(r,t,a,h))
  @Test def untilHole()       = test("until true", WhileStmt(r,f,a,h))
  @Test def doWhile()         = test("do; while (true)", DoStmt(r,es,r,t,a))
  @Test def doWhileHole()     = test("do while (true)", DoStmt(r,he,r,t,a))
  @Test def doWhileBare()     = test("do; while true", DoStmt(r,es,r,t,a))
  @Test def doWhileHoleBare() = test("do while true", DoStmt(r,he,r,t,a))
  @Test def doUntil()         = test("do until true", DoStmt(r,he,r,f,a))

  // For
  @Test def forever()     = test("for (;;);", ForStmt(r,Nil,None,r,Nil,a,es))
  @Test def foreverHole() = test("for (;;)", ForStmt(r,Nil,None,r,Nil,a,h))
  @Test def forSimple()   = test("for (x=7;true;x++)", "x", x =>
    ForStmt(r,VarStmt(IntType,r,(x,7)),Some(t),r,ImpExp(PostIncOp,r,x),a,h))
  @Test def forTwo()      = test("for (x=7,y=8.1;true;)", "x", "y", (x,y) =>
    BlockStmt(List(VarStmt(IntType,r,(x,7)),
                   VarStmt(DoubleType,r,(y,8.1)),
                   ForStmt(r,Nil,Some(t),r,Nil,a,h)),a))
  @Test def foreach()     = test("for (x : 1,2)", "x", x => {
    assertFinal(x); ForeachStmt(r,Nil,IntType,r,x,r,ArrayExp(IntType,List(1,2),a),a,h) })

  @Test def sideEffects() = {
    lazy val X: ClassItem = NormalClassItem("X",LocalPkg,constructors=Array(cons))
    lazy val cons = NormalConstructorItem(X,Nil,Nil)
    val f = NormalMethodItem("f",X,Nil,VoidType,Nil,true)

    implicit val env = Env(Array(X,cons,f),Map((f,2),(X,3)),PlaceInfo(f))
    // We are not allowed to discard the possible side effects in the X constructor.
    test("X().f();", SemiStmt(ExpStmt(ApplyExp(MethodDen(ApplyExp(NewDen(r,None,cons,r),Nil,a,auto=false),f,r),Nil,a,auto=false)),r))
  }

  @Test def sideEffectsCons() = {
    lazy val X: ClassItem = NormalClassItem("X",LocalPkg,constructors=Array(cons))
    lazy val cons = NormalConstructorItem(X,Nil,Nil)
    val Y = NormalClassItem("Y",X,Nil)
    implicit val env = localEnv(X,cons,Y)
    test("X().Y y", "y", y => List(ExpStmt(ApplyExp(NewDen(r,None,cons,r),Nil,a,auto=false)),VarStmt(Y,r,VarDecl(y,r,Nil,None))))
  }

  @Test def sideEffectsFail() = {
    val x = NormalLocal("x",IntType,isFinal=true)
    implicit val env = localEnv(x)
    testFail("x")
  }

  @Test def sideEffectsSplit() = {
    val x = NormalLocal("x",IntType,isFinal=false)
    implicit val env = extraEnv.extendLocal(Array(x))
    test("true ? x = 1 : (x = 2)", IfElseStmt(r,true,a,AssignExp(None,r,x,1),r,AssignExp(None,r,x,2)))
  }

  @Test def trueTypo() = {
    implicit val env = localEnvWithBase()
    test("x = tru", "x", x => VarStmt(BooleanType,r,(x,true)))
  }

  // Synchronized
  @Test def sync () = test("synchronized null", SyncStmt(r,NullLit(r),a,h))
  @Test def sync2() = test("synchronized null {}", SyncStmt(r,NullLit(r),a,BlockStmt(Nil,a)))
  @Test def sync3() = test("synchronized (null) {}", SyncStmt(r,NullLit(r),a,BlockStmt(Nil,a)))

  // inserting a cast to bool
  @Test def insertIntComparison() = test("if 1 then;", IfStmt(r,BinaryExp(NeOp,r,1,0),a,es))
  @Test def insertRefTypeComparison() = {
    val o = NormalLocal("o",ObjectType,isFinal=true)
    implicit val env = localEnvWithBase(o)
    test("if o then;", IfStmt(r,BinaryExp(NeOp,r,o,NullLit(r)),a,es))
  }

  @Test def shuffleArgs() = {
    val X = NormalClassItem("X")
    val f = NormalMethodItem("f",X,Nil,VoidType,List(X.simple,DoubleType,StringType,BooleanType),isStatic=true)
    val x = NormalLocal("x",X.simple,isFinal=true)
    val d = NormalLocal("d",DoubleType,isFinal=true)
    val s = NormalLocal("s",StringType,isFinal=true)
    val b = NormalLocal("b",BooleanType,isFinal=true)
    implicit val env = Env(Array(X,f),Map((X,3),(f,2)),PlaceInfo(f)).extendLocal(Array(x,d,s,b))
    test("f(s,b,d,x)", ApplyExp(f,List(x,d,s,b),a,auto=false))
  }

  @Test def omittedQualifier(): Unit = {
    val P = Package("com","P")
    val Z = NormalClassItem("Z",P)
    val Y = NormalClassItem("Y")
    val X = NormalClassItem("X")
    val Zx = NormalStaticFieldItem("x", BooleanType, Z, isFinal=false)
    val Yx = NormalStaticFieldItem("x", BooleanType, Y, isFinal=false)
    val Xx = NormalStaticFieldItem("x", BooleanType, X, isFinal=false)
    val f = NormalMethodItem("f", X, Nil, VoidType, Nil, isStatic=true)
    val x = NormalLocal("x", BooleanType, isFinal=false)
    implicit val env = baseEnv.extend(Array(P,Z,Zx,Y,Yx,X,Xx,f,x),Map((Y,3),(X,3),(Xx,2),(f,2),(x,1))).move(PlaceInfo(f))
    val fixes = fix(lex("x = true") map (x => Loc(x.x,r)))
    // make sure that local x is the most likely, then X.x (shadowed, but in scope), then Y.x (not in scope), then Z.x (different package)
    def set(e: Exp): List[Stmt] = AssignExp(None,r,e,true)
    val px  = pp(probOf(fixes,set(x)))
    val pXx = pp(probOf(fixes,set(FieldExp(None,Xx,r))))
    val pYx = pp(probOf(fixes,set(FieldExp(None,Yx,r))))
    val pZx = pp(probOf(fixes,set(FieldExp(None,Zx,r))))

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
    val B = NormalClassItem("B")
    val F = NormalClassItem("F")
    val f = NormalMethodItem("f",F,List(S),VoidType,List(S),isStatic=true)
    for (w <- List(WildSub(),WildSub(B),WildSuper(B))) {
      val t = A.generic(List(w))
      val x = NormalLocal("x",t,isFinal=true)
      implicit val env = localEnv(A,x,F,f)
      test("f(x) // "+w,List(ExpStmt(ApplyExp(TypeApply(f,List(t),a,hide=true),List(x),a,auto=false)),
                             CommentStmt(EOLCommentTok("// "+w),r)))
    }
  }

  // Mismatched parentheses
  @Test def mismatchedParens() = {
    lazy val X: ClassItem = NormalClassItem("X",constructors=Array(cons))
    lazy val cons = NormalConstructorItem(X,Nil,Nil)
    val f = NormalMethodItem("f",X,Nil,VoidType,Nil,isStatic=false)
    implicit val env = localEnv(X,cons,f)
    val best = ApplyExp(MethodDen(ParenExp(ApplyExp(NewDen(r,None,cons,r),Nil,a,auto=false),a),f,r),Nil,a,auto=false)
    test("((X()).f()",best,margin=1)
    test("((X()).f(",best)
  }

  @Test def omittedEmptyCallParens() = {
    val X = NormalClassItem("X")
    val f = NormalMethodItem("f",X,Nil,VoidType,Nil,isStatic=false)
    implicit val env = localEnv(X,f)
    test("f", ApplyExp(LocalMethodDen(f,r),Nil,a,auto=true))
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
    test("this()", ApplyExp(ForwardDen(This,r,Xc),Nil,a,auto=false))
    test("super()", ApplyExp(ForwardDen(Super,r,Yc),Nil,a,auto=false))
    // TODO: This should only work as the first statement of a different constructor, which is not tracked by the PlaceInfo right now
  }

  @Test def illegalConstructorForward(): Unit = {
    // cannot forward to constructor outside of constructor
    val Y = NormalClassItem("Y", LocalPkg)
    val Yc = NormalConstructorItem(Y,Nil,Nil)
    val X = NormalClassItem("X", LocalPkg, Nil, Y)
    val Xc = NormalConstructorItem(X, Nil, Nil)
    val f = NormalMethodItem("f", X, Nil, VoidType, Nil, isStatic = false)
    implicit val env = Env(Array(Y,Yc,X,Xc), Map(f->2,Xc->2,X->2,Y->3,Yc->3), PlaceInfo(f))
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
    val X = env.allLocalItems.find(_.name == "X").get.asInstanceOf[NormalClassItem]
    val B = env.allLocalItems.find(_.name == "B").get.asInstanceOf[NormalClassItem]
    test("X<String,B<String>> x = null", "x", x =>
      VarStmt(X.generic(List(StringType,B.generic(List(StringType)))),r,VarDecl(x,r,Nil,Some(r,NullLit(r)))))
  }

  @Test def genericMethod(): Unit = {
    implicit val env = setupGenericClass()
    val f = env.allLocalItems.find(_.name == "f").get.asInstanceOf[NormalMethodItem]
    val This = env.allLocalItems.find(_.isInstanceOf[ThisItem]).get.asInstanceOf[ThisItem]
    test("""this.<Integer>f(7)""",ApplyExp(TypeApply(MethodDen(This,f,r),List(IntType.box),a,hide=false),List(7),a,auto=false))
    test("""<Integer>f(7)""",     ApplyExp(TypeApply(LocalMethodDen(f,r),List(IntType.box),a,hide=false),List(7),a,auto=false))
    test("""f<Integer>(7)""",     ApplyExp(TypeApply(LocalMethodDen(f,r),List(IntType.box),a,hide=false),List(7),a,auto=false))
    // These three fail because f's type argument extends Number
    def bad(s: String) = testFail(s,bound=1e-3)
    bad("""this.<String>f("test")""")
    bad("""<String>f("test")""")
    bad("""f<String>("test")""")
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
    test("X<S,T,U> x", "x", x => VarStmt(X.generic(List(S,T,U)),r,VarDecl(x,r,0,None)))
    test("A<S,S,U> x", "x", x => VarStmt(X.generic(List(S,T,U)),r,VarDecl(x,r,0,None))) // Unlikely, but should work
    def bad(s: String) = testFail(s,bound=3e-4)
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
    test("Integer x = 1","x",x => VarStmt(IntType.box,r,(x,1)))
  }

  @Test def boxByte() = {
    implicit val env = localEnvWithBase()
    test("Byte x = 1","x",x => VarStmt(ByteType.box,r,(x,1)))
  }

  @Test def fizz() = {
    val A = NormalClassItem("A",LocalPkg)
    val fizz = NormalMethodItem("fizz",A,Nil,IntType,List(StringType,IntType.box,DoubleType.box),isStatic=true)
    val x = NormalLocal("x",IntType,isFinal=true)
    val q = NormalLocal("q",DoubleType,isFinal=true)
    implicit val env = baseEnv.extend(Array(A,fizz,x,q),Map(A->1,fizz->1,x->1,q->1)).move(PlaceInfo(fizz))
    test("""fizz "s" x q""",ApplyExp(fizz,List(StringLit("s","\"s\"",r),x,q),a,auto=false))
  }

  @Test def shadowedParameter() = {
    val A = NormalClassItem("A",LocalPkg)
    val B = NormalClassItem("B",LocalPkg)
    val C = NormalClassItem("C",LocalPkg)
    val f = NormalMethodItem("f",A,Nil,VoidType,List(B),isStatic=true)
    val bx = NormalLocal("x",B,isFinal=true)
    val cx = NormalLocal("x",C,isFinal=true)
    def env(bs: Int, cs: Int) = baseEnv.extend(Array(A,B,C,f,bx,cx),Map(bx->bs,cx->cs)).move(PlaceInfo(f))
    def unit(x: Unit) = x
    unit({ implicit val bad = env(bs=2,cs=1); testFail("f x") })
    unit({ implicit val good = env(bs=1,cs=2); test("f x",ApplyExp(f,List(bx),a,auto=false)) })
  }

  @Test def memberToInfix() = {
    val A = NormalClassItem("A")
    val f = NormalMethodItem("f",A,Nil,A,List(A),isStatic=false)
    val x = NormalLocal("a",A,isFinal=true)
    implicit val env = baseEnv.extend(Array(f,x),Map(f->2,x->1)).move(PlaceInfo(f))
    def fa(y: Exp) = ApplyExp(MethodDen(y,f,r),List(x),a,auto=false)
    test("a f a",fa(x))
    test("a f a f a",fa(fa(x)))
  }

  @Test def postfix() = {
    val A = NormalClassItem("A")
    val f = NormalMethodItem("f",A,Nil,VoidType,Nil,isStatic=false)
    def M(c: ClassItem): Local = NormalLocal(c.name.toLowerCase,c,isFinal=true)
    val x = NormalLocal("a",A,isFinal=true)
    implicit val env = baseEnv.extendLocal(Array(x,f)).move(PlaceInfo(f))
    test("a f",ApplyExp(MethodDen(x,f,r),Nil,a,auto=true))
  }

  @Test def largeJuxt() = {
    val A = NormalClassItem("A")
    val X = NormalClassItem("X")
    val Y = NormalClassItem("Y")
    val B = NormalClassItem("B")
    val C = NormalClassItem("C")
    val D = NormalClassItem("D")
    val f = NormalMethodItem("f",A,Nil,B,List(X,Y),isStatic=false)
    val g = NormalFieldItem("g",C,B,isFinal=true)
    val h = NormalMethodItem("h",C,Nil,D,Nil,isStatic=false)
    def M(c: ClassItem): Local = NormalLocal(c.name.toLowerCase,c,isFinal=true)
    val ax = M(A)
    val x = M(X)
    val y = M(Y)
    implicit val env = baseEnv.extendLocal(Array(ax,x,y,f,g,h)).move(PlaceInfo(f))
    test("a f y x g h",ApplyExp(MethodDen(FieldExp(ApplyExp(MethodDen(ax,f,r),List(x,y),a,auto=false),g,r),h,r),Nil,a,auto=true))
  }

  @Test def javascriptStyleMember() = {
    val A = NormalClassItem("A")
    val f = NormalFieldItem("f",A,A,isFinal=false)
    val a = NormalLocal("a",A,isFinal=true)
    implicit val env = baseEnv.extend(Array(A,f,a), Map(A->2,f->2,a->1))
    for (s <- List("a[f] = a","a f = a","""a["f"] = a"""))
      test(s,AssignExp(None,r,FieldExp(a,f,r),a))
    test("a f f f = a",AssignExp(None,r,FieldExp(FieldExp(FieldExp(a,f,r),f,r),f,r),a))
  }

  @Test def newObject() = test("new Object()",ApplyExp(NewDen(r,None,ObjectConsItem,r),Nil,a,auto=false))
  @Test def newObjectBare() = test("new Object",ApplyExp(NewDen(r,None,ObjectConsItem,r),Nil,a,auto=true))

  @Test def newGeneric() = {
    val T = SimpleTypeVar("T")
    val S = SimpleTypeVar("S")
    val B = NormalClassItem("B",LocalPkg)
    val C = NormalClassItem("C",LocalPkg)
    lazy val A: ClassItem = NormalClassItem("A",LocalPkg,List(T),constructors=Array(cons))
    lazy val cons = NormalConstructorItem(A,List(S),Nil)
    implicit val env = localEnv().extend(Array(A,cons,B,C),Map(A->1,B->1,C->1))
    test("x = new<C>A<B>","x",x =>
      VarStmt(A.generic(List(B)),r,(x,ApplyExp(TypeApply(NewDen(r,None,cons,r,Some(Grouped(List(B),a))),List(C),a,hide=false),Nil,a,auto=true))))
  }

  @Test def classInPackage() = {
    val P = Package("P")
    lazy val A: ClassItem = NormalClassItem("A",P,constructors=Array(cons))
    lazy val cons = NormalConstructorItem(A,Nil,Nil)
    implicit val env = localEnv().extend(Array(P,A,cons),Map.empty)
    val e = ApplyExp(NewDen(r,None,cons,r),Nil,a,auto=false)
    test("P.A()",e)
    test("new P.A()",e)
  }

  @Test def noEmptyNames(): Unit = {
    val is = localEnvWithBase().exactQuery("")
    if (is.nonEmpty) throw new AssertionError(s"Unexpected empty names: ${is map (_.getClass)}")
  }

  @Test def fieldAccess() = {
    val T = NormalClassItem("T", LocalPkg)
    val x = NormalFieldItem("x", IntType, T, isFinal=false)
    val t = NormalLocal("t", T.simple,  isFinal=true)
    implicit val env = localEnv().extendLocal(Array(T,x), 3).extendLocal(Array(t), 1)
    test("t.x = 1",AssignExp(None,r,FieldExp(t,x,r),1))
  }

  @Test def fixType() = test("int x = 1L","x",x => VarStmt(LongType,r,(x,1L)))
  @Test def fixGarbageType() = test("garbageGarbageGarbage x = 1L","x",x => VarStmt(LongType,r,(x,1L)))

  @Test def fixTypeGenericRightToLeft() = {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    lazy val A: ClassItem = NormalClassItem("A",tparams=List(S))
    lazy val B: ClassItem = NormalClassItem("B",tparams=List(T),base=A.generic(List(T)),constructors=Array(cons))
    lazy val cons = DefaultConstructorItem(B)
    val pre = localEnvWithBase().extendLocal(Array(A,B))
    implicit val tl = TrackLoc(on=true)
    implicit val env = pre.move(PlaceInfo(pre.place.place,lastEdit=SLoc(22)))
    def r(lo: Int, hi: Int) = SRange(SLoc(lo),SLoc(hi))
    def a(lo: Int, hi: Int) = SGroup.approx(r(lo,hi))
    test("A<Integer> x = new B<Long>","x",x =>
      VarStmt(A.generic(List(LongType.box)),r(0,10),
              VarDecl(x,r(11,12),Nil,Some(r(13,14),
                ApplyExp(NewDen(r(19,19),None,cons,r(19,20),Some(Grouped(List(LongType.box),a(20,26)))),Nil,a(26,26),auto=true)))))
  }

  @Test def fixTypeGenericLeftToRight() = {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    lazy val A: ClassItem = NormalClassItem("A",tparams=List(S))
    lazy val B: ClassItem = NormalClassItem("B",tparams=List(T),base=A.generic(List(T)),constructors=Array(cons))
    lazy val cons = DefaultConstructorItem(B)
    val pre = localEnvWithBase().extendLocal(Array(A,B))
    implicit val tl = TrackLoc(on=true)
    implicit val env = pre.move(PlaceInfo(pre.place.place,lastEdit=SLoc(7)))
    def r(lo: Int, hi: Int) = SRange(SLoc(lo),SLoc(hi))
    def a(lo: Int, hi: Int) = SGroup.approx(r(lo,hi))
    test("A<Integer> x = new B<Long>","x",x =>
      VarStmt(A.generic(List(IntType.box)),r(0,10),
              VarDecl(x,r(11,12),Nil,Some(r(13,14),
                ApplyExp(NewDen(r(19,19),None,cons,r(19,20),Some(Grouped(List(IntType.box),a(20,20)))),Nil,a(20,20),auto=true)))))
  }

  @Test def fillTypeGeneric() = {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    lazy val A: ClassItem = NormalClassItem("A",tparams=List(S))
    lazy val B: ClassItem = NormalClassItem("B",tparams=List(T),base=A.generic(List(T)),constructors=Array(cons))
    lazy val cons = DefaultConstructorItem(B)
    implicit val env = localEnvWithBase().extendLocal(Array(A,B))
    test("A<Integer> x = new B","x",x =>
      VarStmt(A.generic(List(IntType.box)),r,(x,ApplyExp(NewDen(r,None,cons,r,Some(Grouped(List(IntType.box),a))),Nil,a,auto=true))))
  }

  @Test def fillTypeTernary() = {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    val U = SimpleTypeVar("U")
    lazy val A: ClassItem = NormalClassItem("A",tparams=List(S))
    lazy val B: ClassItem = NormalClassItem("B",tparams=List(T),base=A.generic(List(T)),constructors=Array(consB))
    lazy val C: ClassItem = NormalClassItem("C",tparams=List(U),base=A.generic(List(U)),constructors=Array(consC))
    lazy val consB = DefaultConstructorItem(B)
    lazy val consC = DefaultConstructorItem(C)
    val f = NormalLocal("f",BooleanType,isFinal=true)
    implicit val env = localEnvWithBase().extendLocal(Array(f,A,B,C))
    test("A<Integer> x = f ? new B : (new C)","x",x => {
      val is = List(IntType.box)
      val t = A.generic(is)
      VarStmt(t,r,(x,CondExp(f,r,ApplyExp(NewDen(r,None,consB,r,Some(is)),Nil,a,auto=true),
                               r,ParenExp(ApplyExp(NewDen(r,None,consC,r,Some(is)),Nil,a,auto=true),a),t)))
    })
  }

  @Test def nullaryGenericFunction() = {
    val A = NormalClassItem("A")
    val T = SimpleTypeVar("T")
    val f = NormalMethodItem("f",A,List(T),VoidType,Nil,isStatic=true)
    implicit val env = localEnv().extendLocal(Array(f))
    test("f()",ApplyExp(TypeApply(MethodDen(None,f,r),List(ObjectType),a,hide=true),Nil,a,auto=false))
  }

  @Test def autoReturn() = {
    val A = NormalClassItem("A")
    val x = NormalLocal("x",A,isFinal=true)
    val f = NormalMethodItem("f",NormalClassItem("F"),Nil,A,Nil,isStatic=true)
    implicit val env = Env(Array(A,x),Map(A->2,x->1),PlaceInfo(f))
    test("return",ReturnStmt(r,x))
  }

  @Test def noLabel() = {
    val pre = localEnv()
    def f(b: Boolean, c: Boolean): Unit = {
      implicit val env = pre.move(PlaceInfo(pre.place.place,breakable=b,continuable=c))
      if (b) test("break",BreakStmt(r,None)) else testFail("break")
      if (c) test("continue",ContinueStmt(r,None)) else testFail("continue")
    }
    f(false,false); f(false,true)
    f(true ,false); f(true ,true)
  }
  @Test def label() = {
    val pre = localEnv()
    def f(c: Boolean): Unit = {
      val lab = Label("label",continuable=c)
      implicit val env = pre.extendLocal(Array(lab)).move(PlaceInfo(pre.place.place,breakable=true,continuable=c))
      for (name <- List("label","labl")) {
        test(s"break $name",BreakStmt(r,Some(lab)))
        if (c) test(s"continue $name",ContinueStmt(r,Some(lab)))
        else testFail(s"continue $name")
      }
    }
    f(false); f(true)
  }

  @Test def ifNull() = {
    val x = NormalLocal("x",ObjectType,isFinal=true)
    implicit val env = localEnvWithBase(x)
    test("if (x == null)",IfStmt(r,BinaryExp(EqOp,r,x,NullLit(r)),a,h))
  }

  @Test def spuriousTypeArgs() = {
    lazy val A: ClassItem = NormalClassItem("A",constructors=Array(DefaultConstructorItem(A)))
    val T = SimpleTypeVar("T")
    val f = NormalMethodItem("f",A,List(T),VoidType,List(T),isStatic=true)
    val Y = NormalClassItem("Y")
    val y = NormalLocal("y",Y,isFinal=true)
    implicit val env = localEnvWithBase().extend(Array(A,f,y),Map(A->2,y->1))
    test("A.f(y)",ApplyExp(TypeApply(MethodDen(None,f,r),List(Y),a,hide=true),List(y),a,auto=false))
  }

  @Test def explicitStatic() = {
    // Give A a default constructor make sure eddy doesn't call "new A()" unnecessarily
    lazy val A: ClassItem = NormalClassItem("A",constructors=Array(cons))
    lazy val cons = DefaultConstructorItem(A)
    val f = NormalMethodItem("f",A,Nil,VoidType,Nil,isStatic=true)
    val x = NormalStaticFieldItem("f",IntType,A,isFinal=true)
    val y = NormalLocal("y",IntType,isFinal=false)
    implicit val env = localEnvWithBase().extend(Array(A,f,x,y),Map(A->2,y->1))
    test("A.f()",ApplyExp(MethodDen(None,f,r),Nil,a,auto=false))
    test("y = A.x",AssignExp(None,r,y,FieldExp(None,x,r)))
    for (auto <- List(false,true)) {
      val ap = ApplyExp(NewDen(r,None,cons,r),Nil,a,auto=auto)
      testAvoid("A.f()",ApplyExp(MethodDen(ap,f,r),Nil,a,auto=false))
      testAvoid("y = A.x",AssignExp(None,r,y,FieldExp(ap,x,r)))
    }
  }

  @Test def shadowParameter(): Unit = {
    lazy val A: ClassItem = NormalClassItem("A",constructors=Array(cons))
    lazy val cons = DefaultConstructorItem(A)
    val f = NormalMethodItem("f",A,Nil,VoidType,List(IntType),isStatic=true)
    val x = NormalLocal("x",IntType,isFinal=true)
    implicit val env = localEnvWithBase().extend(Array(A,f,x),Map(A->2,f->2,x->2))
    testFail("int x")
  }

  @Test def if0() = {
    val x = NormalLocal("x",ObjectType,isFinal=true)
    val b = NormalLocal("b",BooleanType,isFinal=true)
    implicit val env = localEnvWithBase(x,b)
    test("if (x == 0)",IfStmt(r,BinaryExp(EqOp,r,x,NullLit(r)),a,HoleStmt(r)))
    test("if (0 != x)",IfStmt(r,BinaryExp(NeOp,r,NullLit(r),x),a,HoleStmt(r)))
    test("if (b != 0)",IfStmt(r,BinaryExp(NeOp,r,b,false),a,HoleStmt(r)))
    test("if (0 == b)",IfStmt(r,BinaryExp(EqOp,r,false,b),a,HoleStmt(r)))
  }

  @Test def comment() = test("x = 1 // blah","x",x => List(VarStmt(IntType,r,(x,1)),CommentStmt(EOLCommentTok("// blah"),r)))

  @Test def finalVar() = test("final x = 1;","x",x => SemiStmt(VarStmt(IntType,r,(x,1),List(Final)),r))
  @Test def finalVarType() = test("final int x = 1;","x",x => SemiStmt(VarStmt(IntType,r,(x,1),List(Final)),r))

  @Test def instanceofTest(): Unit = notImplemented

  @Test def verboseArray() = test("int[] x = new int[]{1,2,3}","x",x =>
    VarStmt(ArrayType(IntType),r,VarDecl(x,r,0,Some(r,ArrayExp(ArrayType(IntType),List(1,2,3),a)))))
}
