package tarski

import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.Base._
import tarski.Denotations._
import tarski.Items._
import tarski.Lexer._
import tarski.Pretty._
import tarski.Tokens._
import tarski.Types._

import scala.language.implicitConversions

class TestPretty {
  implicit val env = baseEnv

  def test[A](s: String, x: A)(implicit p: Pretty[A]) =
    assertEquals(s"$s -> ${show(tokens(x))}", lex(s).filterNot(isSpace), tokens(x))

  @Test def obj() = test("Object", ObjectType)
  @Test def objMethod() = test("Object.f",NormalMethodItem("f",ObjectItem,Nil,VoidType,Nil,true))
  @Test def clsMethod() = test("A.f",NormalMethodItem("f",NormalClassItem("A",LocalPkg,Nil),Nil,VoidType,Nil,true))
  @Test def genericMethod() = test("B.a.<Object>f()", {
    val A = NormalClassItem("A", LocalPkg)
    val B = NormalClassItem("B", LocalPkg)
    ApplyExp(MethodDen(StaticFieldExp(None, NormalStaticFieldItem("a", A.simple, B, isFinal=false)), NormalMethodItem("f", A, List(SimpleTypeVar("T")), IntType, Nil, isStatic=false)), List(ObjectType), Nil)
  })
  @Test def genericStaticMethod() = test("A.<Object>f()", {
    val A = NormalClassItem("A", LocalPkg)
    ApplyExp(StaticMethodDen(None, NormalMethodItem("f", A, List(SimpleTypeVar("T")), IntType, Nil, isStatic=true)), List(ObjectType), Nil)
  })
  @Test def genericPlacement() = test("pkg.L.<String>fill(1,\"s\")", {
    val P = PackageItem("pkg", "pkg")
    val L = NormalClassItem("L", P, List(SimpleTypeVar("A")))
    val fill = NormalMethodItem("fill", L, List(SimpleTypeVar("A")), VoidType, List(IntType, StringItem.simple), isStatic=true)
    ApplyExp(StaticMethodDen(None, fill), List(StringItem.simple), List(IntLit(1, "1"), StringLit("s","\"s\"")))
  })
  @Test def genericNew() = test("new<String>A<Integer>(\"s\")", {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    val A = NormalClassItem("A",LocalPkg,List(S))
    val cons = NormalConstructorItem(A,List(T),List(T))
    ApplyExp(NewDen(cons),List(IntegerItem.simple,StringItem.simple),List(StringLit("s",""""s"""")))
  })
}
