package tarski

import tarski.AST._
import tarski.Denotations._
import tarski.Pretty._

import scala.language.implicitConversions
import org.testng.annotations.Test
import org.testng.AssertJUnit._

import tarski.Tarski.fix
import tarski.Environment.Env
import tarski.Base._
import tarski.Items._
import tarski.Lexer._
import tarski.Tokens._
import tarski.Types._
import tarski.TestUtils._
import ambiguity.Utility._

class TestDen {

  def testDen(input: String, best: Env => List[Stmt])(implicit env: Env): Unit = {
    val fixes = fix(lex(input).filterNot(isSpace))
    //println(fixes)
    fixes.best match {
      case Left(e) => throw new RuntimeException("\n"+e.prefixed("error: "))
      case Right((env,s)) => assertEquals(best(env),s)
    }
  }

  def testDen(input: String, best: ExpStmt)(implicit env: Env): Unit =
    testDen(input, env => List(best))

  def testDen(input: String, best: Exp)(implicit env: Env): Unit =
    testDen(input, ExpStmt(best))

  def testOnlyDenotation(input: String, best: Env => List[Stmt])(implicit env: Env) = {
    val fixes = fix(lex(input).filterNot(isSpace))
    assertEquals(fixes.all.right.get.length, 1)
    val (env2,stmt) = fixes.best.right.get
    assertEquals(stmt, best(env2))
  }

  @Test
  def assignExp(): Unit = {
    val x = LocalVariableItem("x",IntType)
    implicit val env = new Env(List(x)).makeAllLocal
    testDen("x = 1", AssignExp(None,x,1))
  }

  @Test
  def variableStmt(): Unit = {
    implicit val env = baseEnv
    testDen("x = 1", env => List(VarStmt(IntType, List((env.exactLocal("x"),0,Some(toExp(1)))))))
  }

  @Test
  def arrayVariableStmtCurly(): Unit = {
    implicit val env = baseEnv
    testDen("x = {1,2,3,4}", env => List(VarStmt(ArrayType(IntType), List((env.exactLocal("x"),0,
      Some(ArrayExp(IntType,List(1,2,3,4))))))))
  }

  @Test
  def arrayVariableStmtParen(): Unit = {
    implicit val env = baseEnv
    testDen("x = (1,2,3,4)", env => List(VarStmt(ArrayType(IntType), List((env.exactLocal("x"),0,
      Some(ArrayExp(IntType,List(1,2,3,4))))))))
  }

  @Test
  def arrayVariableStmtBare(): Unit = {
    implicit val env = baseEnv
    testDen("x = 1,2,3,4", env => List(VarStmt(ArrayType(IntType), List((env.exactLocal("x"),0,
      Some(ArrayExp(IntType,List(1,2,3,4))))))))
  }

  @Test
  def arrayVariableStmtBrack(): Unit = {
    implicit val env = baseEnv
    testDen("x = [1,2,3,4]", env => List(VarStmt(ArrayType(IntType), List((env.exactLocal("x"),0,
      Some(ArrayExp(IntType,List(1,2,3,4))))))))
  }

  @Test
  def arrayLiteralAssign(): Unit = {
    val x = LocalVariableItem("x",ArrayType(IntType))
    implicit val env = Env(List(x)).makeAllLocal
    testDen("x = {1,2,3}", AssignExp(None,x,ArrayExp(IntType,List(1,2,3))))
  }

  @Test
  def arrayLiteral(): Unit = {
    val Main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = StaticMethodItem("f",Main,Nil,VoidType,List(ArrayType(IntType)))
    implicit val env = Env(List(Main,f)).makeAllLocal
    testDen("f({1,2,3,4})", ApplyExp(StaticMethodDen(f),Nil,List(ArrayExp(IntType,List(1,2,3,4)))))
  }

  @Test
  def makeAndSet(): Unit = {
    implicit val env = baseEnv
    testDen("x = 1; x = 2", env => {
      val x = env.exactLocal("x")
      List(VarStmt(IntType, List((x,0,Some(toExp(1))))),
           ExpStmt(AssignExp(None,x,2)))
    })
  }

  @Test
  def indexExp(): Unit = {
    val x = LocalVariableItem("x", ArrayType(CharType))
    implicit val env = Env(List(x)).makeAllLocal
    testDen("""x[4] = '\n'""", AssignExp(None,IndexExp(x,4),'\n'))
  }

  @Test
  def nestedIndexExpBrack(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = Env(List(x)).makeAllLocal
    testDen("""x[4,5] = x[2][5]""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def nestedIndexExpJuxt(): Unit = {
    val x = new LocalVariableItem("x", ArrayType(ArrayType(CharType)))
    implicit val env = Env(List(x)).makeAllLocal
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
    implicit val env = Env(List(x)).makeAllLocal
    testDen("""x(4,5) = x(2)(5)""", AssignExp(None, IndexExp(IndexExp(x,4),5), IndexExp(IndexExp(x,2),5)))
  }

  @Test
  def indexOpExp(): Unit = {
    val x = LocalVariableItem("x", ArrayType(CharType))
    implicit val env = Env(List(x)).makeAllLocal
    testDen("""x[4] *= '\n'""", AssignExp(Some(MulOp()), IndexExp(x,4), '\n'))
  }

  @Test
  def mapExp(): Unit = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val f = StaticMethodItem("f",main,Nil,FloatType,List(ArrayType(IntType)))
    val x = LocalVariableItem("x",ArrayType(DoubleType))
    val y = LocalVariableItem("y",ArrayType(DoubleType))
    implicit val env = Env(List(main,f)).makeAllLocal
    testDen("y = f(x)", env => Nil)
    notImplemented
  }

  @Test
  def cons(): Unit = {
    implicit val env = baseEnv
    testDen("x = Object()", env => List(VarStmt(ObjectType,
      List((env.exactLocal("x"),0,Some(ApplyExp(NewDen(ObjectConsItem),Nil,Nil)))))))
  }

  @Test
  def genericConsObject(): Unit = {
    val T = TypeParamItem("T")
    val A = NormalClassItem("A",LocalPkg,List(T))
    val AC = ConstructorItem(A,Nil,List(ParamType(T)))
    implicit val env = baseEnv.addObjects(List(A,AC),Map((A,2),(AC,1)))
    testDen("x = A(Object())", env => List(VarStmt(GenericClassType(A,List(ObjectType)),
      List((env.exactLocal("x"),0,Some(ApplyExp(NewDen(AC),List(ObjectType),List(ApplyExp(NewDen(ObjectConsItem),Nil,Nil)))))))))
  }

  @Test
  def varArray(): Unit = {
    implicit val env = Env(Nil)
    testDen("int x[]", env => List(VarStmt(IntType,List((env.exactLocal("x"),1,None)))))
  }

  @Test
  def varArrayInit(): Unit = {
    implicit val env = Env(Nil)
    testDen("int x[] = {1,2,3}", env =>
      List(VarStmt(IntType,List((env.exactLocal("x"),1,Some(ArrayExp(IntType,List(1,2,3))))))))
  }
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
    val Xf = FieldItem("f", SimpleClassType(Q), X)
    val Y = NormalClassItem("Y", LocalPkg, Nil, SimpleClassType(X), Nil)
    val Yf = FieldItem("f", SimpleClassType(R), Y)

    val Z = NormalClassItem("Z", LocalPkg, Nil, ObjectType, Nil)
    val m = MethodItem("m", Z, Nil, VoidType, List(SimpleClassType(Q)))
    val y = LocalVariableItem("y", SimpleClassType(Y))
    implicit val env = Env(List(X,Y,Z,Xf,Yf,m,y), Map((y,1),(m,2)))

    testDen("m(f)", ApplyExp(LocalMethodDen(m),Nil,List(FieldExp(CastExp(SimpleClassType(X), y), Xf))))
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
    val Xf = FieldItem("f", SimpleClassType(Q), X)
    val Y = NormalClassItem("Y", LocalPkg, Nil, SimpleClassType(X), Nil)
    val Yf = FieldItem("f", SimpleClassType(R), Y)

    val m = MethodItem("m", Y, Nil, VoidType, List(SimpleClassType(Q)))
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
}
