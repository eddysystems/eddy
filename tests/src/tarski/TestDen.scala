package tarski

import tarski.Mods.Final
import utility.Locations._
import utility.Utility._
import tarski.AST._
import tarski.Base._
import tarski.Denotations._
import tarski.Environment._
import tarski.Items._
import tarski.JavaScores._
import tarski.Lexer._
import tarski.Operators._
import tarski.Pretty._
import tarski.Scores._
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

  // By default, we ignore locations and run a fixpoint test
  case class Flags(trackLoc: Boolean=false, // If false, locations are stripped after lexing
                   fixpoint: Boolean=true, // Run at least an approximate fixpoint test
                   locationFixpoint: Boolean=false) // Test locations in the fixpoint test
  implicit val fl = Flags()

  def fixFlags(ts: Tokens)(implicit env: Env, fl: Flags): Scored[List[Stmt]] =
    Tarski.fix(if (fl.trackLoc) ts else ts map (x => Loc(x.x,r)))

  def fp(p: Prob) = pp(p) + (if (!trackProbabilities) "" else s"\n${ppretty(p).prefixed("  ")}")
  def locs(s: List[Stmt]): Map[Name,Local] = s.flatMap(locals).map(l => (l.name,l)).toMap

  def testHelper[A](input: String, best: List[Stmt] => A, margin: Double = .9)
                   (implicit env: Env, convert: A => List[Stmt], fl: Flags): Unit = {
    val fixes = fixFlags(lex(input))
    fixes.strict match {
      case e:EmptyOrBad => throw new RuntimeException(s"Denotation test failed:\ninput: $input\n"+e.error.prefixed("error: "))
      case Best(p,s,rest) =>
        val b = convert(best(s))
        def sh(s: List[Stmt]) = show(s)(prettyStmts,showFlags)
        val sb = sh(b)
        if (b != s) {
          val ep = probOf(fixes,s => convert(best(s)))
          throw new AssertionError(s"Denotation test failed:\ninput: $input" +
                                   s"\nexpected: $sb\nactual  : ${sh(s)}" +
                                   s"\nexpected p = ${fp(ep)}" +
                                   s"\nactual   p = ${fp(p)}" +
                                   s"\nexpected (full): $b\nactual   (full): $s")
        } else if (!rest.below(margin*pp(p))) {
          val n = rest.stream.head
          throw new AssertionError(s"Denotation test failed:\ninput: $input" +
                                   s"\nwanted margin $margin, got ${pp(n.dp)} / ${pp(p)} = ${pp(n.dp) / pp(p)}" +
                                   s"\nbest: $sb\nnext: ${sh(n.x)}" +
                                   s"\nbest p = ${fp(p)}" +
                                   s"\nnext p = ${fp(n.dp)}")
        }
        if (fl.fixpoint) {
          // Verify that locations are correctly threaded, by rerunning fix with full locations
          val ts2 = lex(sb)
          def ignore(x: Loc[Token]): Boolean = isSpace(x) || x.x==HoleTok
          Tarski.fix(ts2)(env).strict match {
            case e:EmptyOrBad => throw new RuntimeException(s"Fixpoint test failed:\n"
                                                          + s"input: ${print(ts2 map (_.x))}\n"
                                                          + s"ts2: ${ts2 mkString " "}\n"
                                                          + s"ts2: ${ts2 filterNot ignore mkString " "}\n"
                                                          + e.error.prefixed("error: "))
            case Best(_,s3,_) =>
              val ts3 = tokens(s3)
              val i2 = ts2 filterNot ignore
              val i3 = ts3 filterNot ignore
              val si2 = i2 map (_.x)
              val si3 = i3 map (_.x)
              if (si2 != si3)
                throw new RuntimeException("Fixpoint not reached:\n\n"
                  +s"input = $sb\n"
                  +s"expected = ${si2 mkString " "}\n"
                  +s"actual   = ${si3 mkString " "}\n")
              else if (fl.locationFixpoint && i2 != i3)
                throw new RuntimeException("Location fixpoint not reached:\n\n"
                  +s"input    = $sb\n"
                  +s"redone   = $s3\n"
                  +s"expected = ${i2 mkString " "}\n"
                  +s"actual   = ${i3 mkString " "}\n")
          }
        }
    }
  }

  def test[A](input: String, best: A, margin: Double = .9)(implicit env: Env, c: A => List[Stmt], fl: Flags): Unit =
    testHelper(input, s => best, margin=margin)
  def test[A](input: String, x: Name, best: Local => A)(implicit env: Env, c: A => List[Stmt], fl: Flags): Unit =
    testHelper(input, s => best(locs(s)(x)))
  def test[A](input: String, x: Name, y: Name, best: (Local,Local) => A)(implicit env: Env, c: A => List[Stmt], fl: Flags): Unit =
    testHelper(input, s => { val l = locs(s); best(l(x),l(y)) })

  // Ensure that all options have probability at most bound
  def testFail(input: String, bound: Double = -1)(implicit env: Env, fl: Flags): Unit = {
    @tailrec def checkFail[A,B](s: Scored[A], bound: Double = -1)(f: A => B): Unit = if (s.p > bound) s match {
      case s:LazyScored[A] => checkFail(s force bound,bound)(f)
      case Best(p,x,_) => throw new AssertionError(s"\nExpected probability <= $bound for $input, got\n  $p : ${f(x)}")
      case _:EmptyOrBad => ()
    }
    checkFail(fixFlags(lex(input)),bound)(x => s"${show(x)} ($x)")
  }

  // Ensure that an option doesn't appear
  def testAvoid[A](input: String, bad: A)(implicit env: Env, convert: A => List[Stmt], fl: Flags): Unit = {
    val b = convert(bad)
    val p = probOf(fixFlags(lex(input)),b)
    if (pp(p) >= 0)
      throw new AssertionError(s"Avoidance test failed:\ninput: $input" +
                               s"\nbad: ${show(b)}" +
                               s"\nbad p = ${fp(p)}" +
                               s"\nbad (full): $b")
  }

  def probOf(s: Scored[List[Stmt]], a: List[Stmt] => List[Stmt]): Prob = {
    def loop(s: Stream[Alt[List[Stmt]]]): Prob =
      if (s.isEmpty) Prob("not found",-1)
      else {
        val ss = s.head.x
        if (a(ss) == ss) s.head.dp
        else loop(s.tail)
      }
    loop(s.stream)
  }
  def probOf(s: Scored[List[Stmt]], a: List[Stmt]): Prob =
    probOf(s,s => a)
  
  def testSpace(input: String, output: String)(implicit env: Env): Unit = {
    val ts = lex(input)
    val sp = spaces(ts)
    Tarski.fix(ts).strict match {
      case e:EmptyOrBad => throw new AssertionError(s"Space test failed:\ninput: $input\n"+e.error.prefixed("error: "))
      case Best(_,ss,_) =>
        val ts2 = tokens(ss)
        val raw = insertSpaces(ts2,sp) map (_.x)
        val got = print(raw)(fullShowFlags)
        if (got != output)
          throw new AssertionError(s"Space test failed:\n" +
                                   s"input:  $input\n" +
                                   s"output: $output\n" +
                                   s"got:    $got\n" +
                                   s"tokens: ${raw mkString " "}\n" +
                                   s"spaces: ${sp mkString " "}")
    }
  }

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
    test("x = 1", "x", x => VarStmt(Nil,IntType,r,(x,1),env))

  @Test
  def arrayVariableStmtCurly() =
    test("x = {1,2,3,4}", "x", x => VarStmt(Nil,ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3,4),a)),env))

  @Test
  def arrayVariableStmtParen() =
    test("x = (1,2,3,4)", "x", x => VarStmt(Nil,ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3,4),a)),env))

  @Test
  def arrayVariableStmtBare() =
    test("x = 1,2,3,4", "x", x => VarStmt(Nil,ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3,4),a)),env))

  @Test
  def arrayVariableStmtBrack() =
    test("x = [1,2,3,4]", "x", x => VarStmt(Nil,ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3,4),a)),env))

  @Test
  def arrayLiteralAssign(): Unit = {
    val x = NormalLocal("x",ArrayType(IntType),isFinal=false)
    implicit val env = localEnvWithBase(x)
    test("x = {1,2,3}", AssignExp(None,r,x,ArrayExp(IntType,List(1,2,3),a)))
  }

  @Test
  def arrayLiteral(): Unit = {
    val Main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = NormalMethodItem("f",Main,Nil,VoidType,List(ArrayType(IntType)),isStatic=true)
    implicit val env = localEnvWithBase().extend(Array(Main,f),Map(Main->2,f->2)).move(PlaceInfo(f))
    test("f({1,2,3,4})", ApplyExp(f,List(ArrayExp(IntType,List(1,2,3,4),a)),a,auto=false))
  }

  @Test def arrayType() = {
    implicit val env = localEnvWithBase()
    test("int[] x = {1,2,3}", "x", x => VarStmt(Nil,ArrayType(IntType),r,(x,ArrayExp(IntType,List(1,2,3),a)),env))
  }

  @Test def noIndex() = testFail("x = 1[]")

  @Test
  def makeAndSet() =
    test("x = 1; x = 2", "x", x =>
      List(SemiStmt(VarStmt(Nil,IntType,r,(x,1),env),r),
           ExpStmt(AssignExp(None,r,x,2),env)))

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
    return // TODO: Re-enable (see https://github.com/eddysystems/eddy/issues/78)
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = NormalMethodItem("f",main,Nil,FloatType,List(ArrayType(IntType)),isStatic=true)
    val x = NormalLocal("x",ArrayType(DoubleType),isFinal=true)
    val y = NormalLocal("y",ArrayType(DoubleType),isFinal=false)
    implicit val env = Env(Array(main,f,x,y), Map((main,2),(f,2),(x,1),(y,1)),PlaceInfo(f))
    test("y = f(x)", Nil)
  }

  @Test
  def cons(): Unit = {
    implicit val env = localEnvWithBase()
    test("x = Object()", "x", x => VarStmt(Nil,ObjectType,r,(x,ApplyExp(NewDen(r,None,ObjectConsItem,r),Nil,a,auto=false)),env))
  }

  @Test
  def genericConsObject(): Unit = {
    val T = SimpleTypeVar("T")
    lazy val A: ClassItem = NormalClassItem("A",LocalPkg,List(T),constructors=Array(AC))
    lazy val AC = NormalConstructorItem(A,Nil,List(T))
    implicit val env = localEnvWithBase().extend(Array(A,AC),Map((A,3),(AC,3)))
    // should result in A<Object> x = new A<Object>(new Object());
    test("x = A(Object())", "x", x => VarStmt(Nil,A.generic(List(ObjectType)),r,
      (x,ApplyExp(NewDen(r,None,AC,r,Some(Grouped(List(ObjectType),a))),
                  List(ApplyExp(NewDen(r,None,ObjectConsItem,r),Nil,a,auto=false)),a,auto=false)),env))
  }

  @Test
  def varArray() =
    test("int x[]", "x", x => VarStmt(Nil,IntType,r,VarDecl(x,r,1,None,env),env))

  @Test
  def varArrayInit() =
    test("int x[] = {1,2,3}", "x", x => VarStmt(Nil,IntType,r,VarDecl(x,r,1,Some(r,ArrayExp(IntType,List(1,2,3),a)),env),env))

  @Test
  def nullInit() =
    test("x = null", "x", x => VarStmt(Nil,ObjectType,r,(x,NullLit(r)),env))

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

    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil, false, false, Set("f"))
    val Xf = NormalFieldItem("f",Q.simple,X,true)
    val Y = NormalClassItem("Y", LocalPkg, Nil, X.simple, Nil, false, false, Set("f"))
    val Yf = NormalFieldItem("f",R.simple,Y,true)

    val Z = NormalClassItem("Z", LocalPkg, Nil, ObjectType, Nil)
    val m = NormalMethodItem("m", Z, Nil, VoidType, List(Q.simple), false)
    val y = NormalLocal("y",Y.simple,isFinal=true)
    implicit val env = Env(Array(X,Y,Z,Xf,Yf,m,y), Map((y,1),(m,2)))

    test("m(f)", ApplyExp(MethodDen(None,m,r),List(FieldExp(CastExp(X.simple,a,y),Xf,r)),a,auto=false))
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

    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil, false, false, Set("f"))
    val Xf = NormalFieldItem("f",Q.simple,X,true)
    val Y = NormalClassItem("Y", LocalPkg, Nil, X.simple, Nil, false, false, Set("f"))
    val Yf = NormalFieldItem("f",R.simple,Y,true)

    val m = NormalMethodItem("m", Y, Nil, VoidType, List(Q.simple), false)
    val This = ThisItem(Y)
    val Super = SuperItem(Y.base)
    implicit val env = Env(Array(X,Y,Xf,Yf,m,This,Super), Map((This,2),(Super,2),(m,2),(Y,2),(Yf,2),(X,3),(Xf,3)))

    test("m(f)", ApplyExp(MethodDen(None,m,r),List(FieldExp(SuperExp(Super,r),Xf,r)),a,auto=false))
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
  def byteLiteral() = test("byte x = 3", "x", x => VarStmt(Nil,ByteType,r,(x,IntLit(3,"3",r)),env))

  @Test
  def intLiteral() = test("int x = 3", "x", x => VarStmt(Nil,IntType,r,(x,IntLit(3,"3",r)),env))

  @Test
  def parens() = test("x = (1)", "x", x => VarStmt(Nil,IntType,r,(x,ParenExp(1,a)),env))

  // If statements
  val t = BooleanLit(true,r)
  val f = BooleanLit(false,r)
  val e = EmptyStmt(r,env)
  val es = SemiStmt(e,r)
  val h = HoleStmt(r,env)
  val he = HoleStmt(SRange.empty,env)
  def testX(input: String, best: (Stmt,Stmt) => Stmt) = {
    val x = NormalLocal("x",IntType,isFinal=false)
    implicit val env = extraEnv.extendLocal(Array(x))
    test(input,best(AssignExp(None,r,x,1),AssignExp(None,r,x,2)))
  }
  @Test def ifStmt()       = test ("if (true);", IfStmt(r,t,a,es))
  @Test def ifHole()       = test ("if (true)", IfStmt(r,t,a,h))
  @Test def ifBare()       = test ("if true;", IfStmt(r,t,a,SemiStmt(e,r)), margin=.99)
  @Test def ifBareHole()   = test ("if true", IfStmt(r,t,a,h))
  @Test def ifThen()       = test ("if true then;", IfStmt(r,t,a,es))
  @Test def ifThenHole()   = test ("if true then", IfStmt(r,t,a,h))
  @Test def ifThenParens() = test ("if (true) then", IfStmt(r,t,a,h))
  @Test def ifElse()       = testX("if (true) x=1 else x=2", (x,y) => IfElseStmt(r,t,a,x,r,y))
  @Test def ifElseHole()   = test ("if (true) else", IfElseStmt(r,t,a,he,r,h))
  @Test def ifThenElse()   = testX("if true then x=1 else x=2", (x,y) => IfElseStmt(r,t,a,x,r,y))
  @Test def elif()         = testX("if (true) x=1 elif (false) x=2", (x,y) => IfElseStmt(r,t,a,x,r,IfStmt(r,f,a,y)))
  @Test def elifBraces()   = testX("if (true) { x=1; } elif false { x=2 }", (x,y) =>
    IfElseStmt(r,t,a,BlockStmt(SemiStmt(x,r),a,env),r,IfStmt(r,f,a,BlockStmt(y,a,env))))
  @Test def ifBraces()     = testX("if true { x=1 }", (x,y) => IfStmt(r,t,a,BlockStmt(x,a,env)))

  // While and do
  @Test def whileStmt()       = test("while (true);", WhileStmt(r,t,a,es))
  @Test def whileHole()       = test("while (true)", WhileStmt(r,t,a,h))
  @Test def whileBare()       = test("while true;", WhileStmt(r,t,a,SemiStmt(e,r)))
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
    ForStmt(r,VarStmt(Nil,IntType,r,(x,7),env),Some(t),r,ImpExp(PostIncOp,r,x),a,h))
  @Test def forTwo()      = test("for (x=7,y=8.1;true;)", "x", "y", (x,y) =>
    BlockStmt(List(VarStmt(Nil,IntType,r,(x,7),env),
                   VarStmt(Nil,DoubleType,r,(y,8.1),env),
                   ForStmt(r,Nil,Some(t),r,Nil,a,h)),a,env))
  @Test def foreach()     = test("for (x : 1,2)", "x", x => {
    assertFinal(x); ForeachStmt(r,Nil,IntType,r,x,r,ArrayExp(IntType,List(1,2),a),a,h,env) })

  @Test def sideEffects() = {
    lazy val X: ClassItem = NormalClassItem("X",LocalPkg,constructors=Array(cons))
    lazy val cons = NormalConstructorItem(X,Nil,Nil)
    val f = NormalMethodItem("f",X,Nil,VoidType,Nil,true)

    implicit val env = Env(Array(X,cons,f),Map((f,2),(X,3)),PlaceInfo(f))
    // We are not allowed to discard the possible side effects in the X constructor.
    test("X().f();", SemiStmt(ExpStmt(ApplyExp(MethodDen(ApplyExp(NewDen(r,None,cons,r),Nil,a,auto=false),f,r),Nil,a,auto=false),env),r))
  }

  @Test def sideEffectsCons() = {
    lazy val X: ClassItem = NormalClassItem("X",LocalPkg,constructors=Array(cons))
    lazy val cons = NormalConstructorItem(X,Nil,Nil)
    val Y = NormalClassItem("Y",X,Nil)
    implicit val env = localEnv(X,cons,Y)
    test("X().Y y", "y", y => VarStmt(Nil,Y,r,VarDecl(y,r,Nil,None,env),env))
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
    test("x = tru", "x", x => VarStmt(Nil,BooleanType,r,(x,true),env))
  }

  // Synchronized
  @Test def sync () = test("synchronized null", SyncStmt(r,NullLit(r),a,BlockStmt(List(h),a,env)))
  @Test def sync2() = test("synchronized null {}", SyncStmt(r,NullLit(r),a,BlockStmt(Nil,a,env)))
  @Test def sync3() = test("synchronized (null) {}", SyncStmt(r,NullLit(r),a,BlockStmt(Nil,a,env)))

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
    val fixes = fixFlags(lex("x = true"))
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
      test("f(x) // "+w,ExpStmt(ApplyExp(TypeApply(f,List(t),a,hide=true),List(x),a,auto=false),env))
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
    test("f", ApplyExp(MethodDen(None,f,r),Nil,a,auto=true))
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

  // Nonnullary functions are treated differently, so constructorForward is not enough
  @Test def constructorForwardSome(): Unit = {
    val A = NormalClassItem("A")
    lazy val Y: ClassItem = NormalClassItem("Y", LocalPkg, constructors = Array(Yc))
    lazy val Yc = NormalConstructorItem(Y,Nil,List(A))
    lazy val X: ClassItem = NormalClassItem("X", LocalPkg, Nil, Y, constructors = Array(Xc,Xc2))
    lazy val Xc = NormalConstructorItem(X, Nil, List(A))
    lazy val Xc2 = NormalConstructorItem(X, Nil, List(IntType))
    val x = NormalLocal("x",A)
    val This = ThisItem(X)
    val Super = SuperItem(Y)
    implicit val env = Env(Array(Y,Yc,X,Xc,This,Super,x),
                           Map((Xc,2),(Xc2,2),(X,2),(Y,3),(Yc,3),(This,2),(Super,2),(x,1)),
                           PlaceInfo(Xc2))
    test("this(x)", ApplyExp(ForwardDen(This,r,Xc),List(x),a,auto=false))
    test("super(x)", ApplyExp(ForwardDen(Super,r,Yc),List(x),a,auto=false))
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
    implicit val fl = Flags(fixpoint=false) // Fixpoint test fails due to GtTok GtTok vs. RShiftTok
    implicit val env = setupGenericClass()
    val X = env.exactQuery("X").head.asInstanceOf[NormalClassItem]
    val B = env.exactQuery("B").head.asInstanceOf[NormalClassItem]
    test("X<String,B<String>> x = null", "x", x =>
      VarStmt(Nil,X.generic(List(StringType,B.generic(List(StringType)))),r,VarDecl(x,r,Nil,Some(r,NullLit(r)),env),env))
  }

  @Test def genericMethod(): Unit = {
    implicit val env = setupGenericClass()
    val f = env.exactQuery("f").head.asInstanceOf[NormalMethodItem]
    val This = env.exactQuery("this").head.asInstanceOf[ThisItem]
    test("""this.<Integer>f(7)""",ApplyExp(TypeApply(MethodDen(This,f,r),List(IntType.box),a,hide=false),List(7),a,auto=false))
    test("""<Integer>f(7)""",     ApplyExp(TypeApply(MethodDen(None,f,r),List(IntType.box),a,hide=false),List(7),a,auto=false))
    test("""f<Integer>(7)""",     ApplyExp(TypeApply(MethodDen(None,f,r),List(IntType.box),a,hide=false),List(7),a,auto=false))
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
    test("X<S,T,U> x", "x", x => VarStmt(Nil,X.generic(List(S,T,U)),r,VarDecl(x,r,0,None,env),env))
    test("A<S,S,U> x", "x", x => VarStmt(Nil,X.generic(List(S,T,U)),r,VarDecl(x,r,0,None,env),env)) // Unlikely, but should work
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
    test("Integer x = 1","x",x => VarStmt(Nil,IntType.box,r,(x,1),env))
  }

  @Test def boxByte() = {
    implicit val env = localEnvWithBase()
    test("Byte x = 1","x",x => VarStmt(Nil,ByteType.box,r,(x,1),env))
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
      VarStmt(Nil,A.generic(List(B)),r,(x,ApplyExp(TypeApply(NewDen(r,None,cons,r,Some(Grouped(List(B),a))),List(C),a,hide=false),Nil,a,auto=true)),env))
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

  @Test def fixType() = test("int x = 1L","x",x => VarStmt(Nil,LongType,r,(x,1L),env))
  @Test def fixGarbageType() = test("garbageGarbageGarbage x = 1L","x",x => VarStmt(Nil,LongType,r,(x,1L),env))

  @Test def fixTypeGenericRightToLeft() = {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    lazy val A: ClassItem = NormalClassItem("A",tparams=List(S))
    lazy val B: ClassItem = NormalClassItem("B",tparams=List(T),base=A.generic(List(T)),constructors=Array(cons))
    lazy val cons = DefaultConstructorItem(B)
    val pre = localEnvWithBase().extendLocal(Array(A,B))
    implicit val fl = Flags(trackLoc=true)
    implicit val env = pre.move(PlaceInfo(pre.place.place,lastEdit=SLoc(22)))
    def r(lo: Int, hi: Int) = SRange(SLoc(lo),SLoc(hi))
    def a(lo: Int, hi: Int) = SGroup.approx(r(lo,hi))
    test("A<Integer> x = new B<Long>","x",x =>
      VarStmt(Nil,A.generic(List(LongType.box)),r(0,10),
              VarDecl(x,r(11,12),Nil,Some(r(13,14),
                ApplyExp(NewDen(r(19,19),None,cons,r(19,20),Some(Grouped(List(LongType.box),a(20,26)))),Nil,a(26,26),auto=true)),env),env))
  }

  @Test def fixTypeGenericLeftToRight() = {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    lazy val A: ClassItem = NormalClassItem("A",tparams=List(S))
    lazy val B: ClassItem = NormalClassItem("B",tparams=List(T),base=A.generic(List(T)),constructors=Array(cons))
    lazy val cons = DefaultConstructorItem(B)
    val pre = localEnvWithBase().extendLocal(Array(A,B))
    implicit val fl = Flags(trackLoc=true)
    implicit val env = pre.move(PlaceInfo(pre.place.place,lastEdit=SLoc(7)))
    def r(lo: Int, hi: Int) = SRange(SLoc(lo),SLoc(hi))
    def a(lo: Int, hi: Int) = SGroup.approx(r(lo,hi))
    test("A<Integer> x = new B<Long>","x",x =>
      VarStmt(Nil,A.generic(List(IntType.box)),r(0,10),
              VarDecl(x,r(11,12),Nil,Some(r(13,14),
                ApplyExp(NewDen(r(19,19),None,cons,r(19,20),Some(Grouped(List(IntType.box),a(20,20)))),Nil,a(20,20),auto=true)),env),env))
  }

  @Test def fillTypeGeneric() = {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    lazy val A: ClassItem = NormalClassItem("A",tparams=List(S))
    lazy val B: ClassItem = NormalClassItem("B",tparams=List(T),base=A.generic(List(T)),constructors=Array(cons))
    lazy val cons = DefaultConstructorItem(B)
    implicit val env = localEnvWithBase().extendLocal(Array(A,B))
    test("A<Integer> x = new B","x",x =>
      VarStmt(Nil,A.generic(List(IntType.box)),r,(x,ApplyExp(NewDen(r,None,cons,r,Some(Grouped(List(IntType.box),a))),Nil,a,auto=true)),env))
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
      VarStmt(Nil,t,r,(x,CondExp(f,r,ApplyExp(NewDen(r,None,consB,r,Some(is)),Nil,a,auto=true),
                               r,ParenExp(ApplyExp(NewDen(r,None,consC,r,Some(is)),Nil,a,auto=true),a),t)),env)
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
    test("return",ReturnStmt(r,x,env))
  }

  @Test def noLabel() = {
    val pre = localEnv()
    def f(b: Boolean, c: Boolean): Unit = {
      implicit val env = pre.move(PlaceInfo(pre.place.place,breakable=b,continuable=c))
      if (b) test("break",BreakStmt(r,None,env)) else testFail("break")
      if (c) test("continue",ContinueStmt(r,None,env)) else testFail("continue")
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
        test(s"break $name",BreakStmt(r,Some(lab),env))
        if (c) test(s"continue $name",ContinueStmt(r,Some(lab),env))
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
    test("if (x == 0)",IfStmt(r,BinaryExp(EqOp,r,x,NullLit(r)),a,HoleStmt(r,env)))
    test("if (0 != x)",IfStmt(r,BinaryExp(NeOp,r,NullLit(r),x),a,HoleStmt(r,env)))
    test("if (b != 0)",IfStmt(r,BinaryExp(NeOp,r,b,false),a,HoleStmt(r,env)))
    test("if (0 == b)",IfStmt(r,BinaryExp(EqOp,r,false,b),a,HoleStmt(r,env)))
  }

  @Test def comment() = testSpace("x = 1 // blah","int x = 1; // blah")
  @Test def comments() = testSpace("/*0*/ x /*1*/ = /*2*/ 1 // 3","/*0*/ int x /*1*/ = /*2*/ 1; // 3")

  @Test def finalVar() = test("final x = 1;","x",x => SemiStmt(VarStmt(List(Final),IntType,r,(x,1),env),r))
  @Test def finalVarType() = test("final int x = 1;","x",x => SemiStmt(VarStmt(List(Final),IntType,r,(x,1),env),r))

  @Test def instanceofTest(): Unit = {
    val a = NormalLocal("a",ObjectType,isFinal=true)
    val x = NormalLocal("x",BooleanType,isFinal=false)
    implicit val env = localEnvWithBase(a,x)
    test("x = a instanceof Object",AssignExp(None, r, x, InstanceofExp(a,r,ObjectType,r)))
  }

  @Test def verboseArray() = test("int[] x = new int[]{1,2,3}","x",x =>
    VarStmt(Nil,ArrayType(IntType),r,VarDecl(x,r,0,Some(r,ApplyExp(NewArrayDen(r,IntType,r,Nil,List(a)),List(1,2,3),a,auto=false)),env),env))

  @Test def testTest(): Unit = {
    val X = NormalClassItem("X",LocalPkg)
    lazy val Y: ClassItem = NormalClassItem("Y",X,isStatic=false,constructors=Array(Yc))
    lazy val Yc = NormalConstructorItem(Y,Nil,Nil)
    val f = NormalMethodItem("f",X,Nil,VoidType,Nil,isStatic=true)
    val x = NormalLocal("x",X.inside,isFinal=false)
    implicit val env = Env(Array(X,Y,f,x),Map(X->3,Y->3,f->3,x->1),PlaceInfo(f))

    // one of these must fail, but not both
    val input = "new Y()"
    val exp = ApplyExp(NewDen(r,None,Yc,r),Nil,a,auto=false)
    var count = 0
    try {
      test(input, exp)
    } catch {
      case e:AssertionError => { println(e.getMessage); count += 1 }
    }
    try {
      testAvoid(input, exp)
    } catch {
      case e:AssertionError => { println(e.getMessage); count += 1 }
    }
    assert(count == 1, "one of test and testAvoid must fail, but not " + (if (count == 0) "neither" else "both"))
  }

  @Test def qualifiedNew(): Unit = {
    val X = NormalClassItem("X",LocalPkg)
    lazy val Y: ClassItem = NormalClassItem("Y",X,isStatic=false,constructors=Array(Yc))
    lazy val Yc = NormalConstructorItem(Y,Nil,Nil)
    val f = NormalMethodItem("f",X,Nil,VoidType,Nil,isStatic=true)
    val x = NormalLocal("x",X.inside,isFinal=false)
    implicit val env = Env(Array(X,Y,f,x),Map(X->3,Y->3,f->3,x->1),PlaceInfo(f))
    testAvoid("new Y()", ApplyExp(NewDen(r,None,Yc,r),Nil,a,auto=false))
    val den = ApplyExp(NewDen(r,Some(x),Yc,r),Nil,a,auto=false)
    test("x.new Y()",den) // this is the only actual Java
    test("new x.Y()",den)
    test("new Y()",den)
    test("new X.Y()",den)
  }

  @Test def tryCatch(): Unit = {
    val x = NormalLocal("x",IntType,isFinal=false)
    implicit val env = localEnvWithBase(x)
    test("try x=1 catch e: Exception", "e", e =>
      TryStmt(r,BlockStmt(AssignExp(None,r,x,1),a,env),
        List(CatchBlock(Nil,r,e,r,a,BlockStmt(he,SGroup.empty,env))),None))
  }

  @Test def tryCatches(): Unit = {
    val B = NormalClassItem("B",base=ThrowableItem)
    val C = NormalClassItem("C",base=ThrowableItem)
    val x = NormalLocal("x",IntType,isFinal=false)
    implicit val env = localEnvWithBase(B,C,x)
    def set(n: Int): BlockStmt = BlockStmt(AssignExp(None,r,x,n),a,env)
    test("try x=1 catch (B b) x=2 catch (c: C) x=3 finally x=4", "b", "c", (b,c) => {
      assert(b.ty == B.simple,b.ty)
      assert(c.ty == C.simple,c.ty)
      TryStmt(r,set(1),
        List(CatchBlock(Nil,r,b,r,a,set(2)),
             CatchBlock(Nil,r,c,r,a,set(3))),
        Some(r,set(4)))
    })
  }

  @Test def tryCatchIgnore(): Unit = {
    val x = NormalLocal("x",IntType,isFinal=false)
    implicit val env = localEnvWithBase(x)
    test("try x=1 catch ...", "$$$eddy_ignored_exception$$$", e =>
      TryStmt(r,BlockStmt(AssignExp(None,r,x,1),a,env),
        List(CatchBlock(Nil,r,e,r,a,BlockStmt(he,SGroup.empty,env))),None))
  }

  @Test def returnAssign() = {
    val A = NormalClassItem("A")
    val f = NormalMethodItem("f",A,Nil,IntType,Nil,isStatic=true)
    val x = NormalLocal("x",IntType,isFinal=false)
    implicit val env = localEnvWithBase(x).move(PlaceInfo(f))
    test("return x = 7",ReturnStmt(r,AssignExp(None,r,x,7),env))
  }

  @Test def notInt() = test("x = !7","x",x => VarStmt(Nil,BooleanType,r,
    (x,BinaryExp(EqOp,r,7,0)),env))
  @Test def andFix() = test("x = \"s\" && 7","x",x => VarStmt(Nil,BooleanType,r,
    (x,BinaryExp(AndAndOp,r,BinaryExp(NeOp,r,"s",NullLit(r)),BinaryExp(NeOp,r,7,0))),env))

  @Test def finalInConstructor() = {
    lazy val A: ClassItem = NormalClassItem("A",constructors=Array(cons))
    lazy val cons = DefaultConstructorItem(A)
    val x = NormalFieldItem("x",IntType,A,isFinal=true) // We should be able to write to this even though it is final
    val This = ThisItem(A)
    implicit val env = baseEnv.extendLocal(Array(x,This)).move(PlaceInfo(cons))
    test("x = 7",AssignExp(None,r,FieldExp(None,x,r),7))
    test("this.x = 7",AssignExp(None,r,FieldExp(This,x,r),7))
  }

  @Test def outOfScopeMethod() = {
    val A = NormalClassItem("A",Package("PA"))
    val B = NormalClassItem("B",Package("PB"))
    val logA = NormalMethodItem("log",A,Nil,VoidType,Nil,isStatic=true)
    val logB = NormalMethodItem("log",B,Nil,VoidType,Nil,isStatic=true)
    implicit val env = localEnvWithBase().extend(Array(logA,logB),Map(logA->1))
    test("log",ApplyExp(MethodDen(None,logA,r),Nil,a,auto=true),margin=.2)
  }

  @Test def avoidVoid() = {
    val A = NormalClassItem("A")
    val f = NormalMethodItem("f",A,Nil,VoidType,Nil,isStatic=true)
    implicit val env = localEnvWithBase(f)
    testFail("void x = f()")
    testFail("int x = f()")
  }
}
