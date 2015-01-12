package tarski

import tarski.Base._
import tarski.Denotations._
import tarski.Environment.{Env, PlaceInfo}
import tarski.Items._
import tarski.Lexer._
import tarski.Pretty._
import tarski.Tokens._
import tarski.Types._
import tarski.TestUtils._
import org.testng.annotations.Test
import org.testng.AssertJUnit._

class TestPretty {
  implicit val env = testEnv

  def test[A](s: String, x: A)(implicit p: Pretty[A]) =
    assertEquals(s"$s -> ${show(x)}", lex(s) map (_.x) filterNot isSpace, tokens(x))

  @Test def obj() = test("Object", ObjectType)
  @Test def objMethod() = test("Object.f",NormalMethodItem("f",ObjectItem,Nil,VoidType,Nil,true))
  @Test def clsMethod() = test("A.f",NormalMethodItem("f",NormalClassItem("A",LocalPkg,Nil),Nil,VoidType,Nil,true))
  @Test def genericMethod() = test("B.a.<Object>f()", {
    val A = NormalClassItem("A", LocalPkg)
    val B = NormalClassItem("B", LocalPkg)
    ApplyExp(TypeApply(MethodDen(FieldExp(None,NormalStaticFieldItem("a",A.simple,B,isFinal=false)),
                                 NormalMethodItem("f",A,List(SimpleTypeVar("T")),IntType,Nil,isStatic=false)),
                       List(ObjectType),hide=false),
             Nil)
  })
  @Test def genericStaticMethod() = test("A.<Object>f()", {
    val A = NormalClassItem("A", LocalPkg)
    ApplyExp(TypeApply(NormalMethodItem("f",A,List(SimpleTypeVar("T")),IntType,Nil,isStatic=true),
                       List(ObjectType),hide=false),Nil)
  })
  @Test def genericPlacement() = test("pkg.L.<String>fill(1,\"s\")", {
    val P = Package("pkg")
    val L = NormalClassItem("L", P, List(SimpleTypeVar("A")))
    val fill = NormalMethodItem("fill", L, List(SimpleTypeVar("A")), VoidType, List(IntType, StringItem.simple), isStatic=true)
    ApplyExp(TypeApply(fill,List(StringItem.simple),hide=false), List(IntLit(1, "1"), StringLit("s","\"s\"")))
  })
  @Test def genericNew() = test("new<String>A<Integer>(\"s\")", {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    val A = NormalClassItem("A",LocalPkg,List(S))
    val cons = NormalConstructorItem(A,List(T),List(T))
    ApplyExp(TypeApply(NewDen(None,cons,Some(List(IntegerItem.simple))),List(StringItem.simple),hide=false),
             List(StringLit("s",""""s"""")))
  })

  @Test def qualifiedType() = {
    val A = NormalClassItem("A",LocalPkg)
    val B = NormalClassItem("B",A)
    val C = NormalClassItem("C",LocalPkg)
    val D = NormalClassItem("D",C)
    val f = NormalMethodItem("f",C,Nil,VoidType,Nil,isStatic=false)
    implicit val env = Env(Array(A,B,C,D,f), Map((A,3),(C,2),(D,2),(f,2)),PlaceInfo(f))
    test("A.B", B.simple)
    test("D", D.simple)
  }

  @Test def wild() = test("?",WildSub())
  @Test def wildSub() = test("? extends Number",WildSub(NumberItem.simple))
  @Test def wildSuper() = test("? super Number",WildSuper(NumberItem.simple))

  @Test def scopeMethod() = {
    val A = NormalClassItem("A", LocalPkg)
    val f = NormalMethodItem("f",A,Nil,VoidType,Nil,isStatic=true)
    val B = NormalClassItem("B", LocalPkg)
    val g = NormalMethodItem("g",A,Nil,VoidType,Nil,isStatic=false)
    implicit val env = Env(Array(A,f,B,g), Map((f,3)),PlaceInfo(g))
    test("f", MethodDen(None,f))
  }

  @Test def applyFixity() = {
    val A = NormalClassItem("A", LocalPkg)
    val a = Local("a", A.simple, isFinal=false);
    val f = NormalMethodItem("f",A,Nil,A.simple,Nil,isStatic=true)
    implicit val env = Env(Array(A,f,a), Map((a,1),(f,2)),PlaceInfo(f))
    test("a.f().f()", ApplyExp(MethodDen(Some(ApplyExp(MethodDen(Some(LocalExp(a)),f),List())),f),List()))
    test("f().f()", ApplyExp(MethodDen(Some(ApplyExp(MethodDen(None,f),List())),f),List()))
  }
}
