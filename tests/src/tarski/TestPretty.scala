package tarski

import tarski.Arounds._
import tarski.Base._
import tarski.Denotations._
import tarski.Environment.{Env, PlaceInfo}
import tarski.Items._
import tarski.Lexer._
import tarski.Operators._
import tarski.Pretty._
import tarski.Tokens._
import tarski.Types._
import tarski.TestUtils._
import tarski.Mods._
import org.testng.annotations.Test
import org.testng.AssertJUnit._
import utility.Locations._

class TestPretty {
  implicit val env = testEnv
  implicit val showFlags = abbrevShowFlags
  implicit val r = SRange.unknown
  val a = SGroup.unknown

  def test[A](s: String, x: A)(implicit p: Pretty[A]) =
    assertEquals(s"$s -> ${show(x)}", lex(s) map (_.x) filterNot isSpace, tokens(x) map (_.x) filterNot isSpace)

  @Test def obj() = test("Object", ObjectType)
  @Test def objMethod() = test("Object.f",NormalMethodItem("f",ObjectItem,Nil,VoidType,Nil,true))
  @Test def clsMethod() = test("A.f",NormalMethodItem("f",NormalClassItem("A",LocalPkg,Nil),Nil,VoidType,Nil,true))
  @Test def genericMethod() = test("B.a.<Object>f()", {
    val A = NormalClassItem("A", LocalPkg)
    val B = NormalClassItem("B", LocalPkg)
    ApplyExp(TypeApply(MethodDen(FieldExp(None,NormalStaticFieldItem("a",A.simple,B,isFinal=false),r),
                                 NormalMethodItem("f",A,List(SimpleTypeVar("T")),IntType,Nil,isStatic=false),r),
                       List(ObjectType),a,hide=false),
             Nil,a,auto=false)
  })
  @Test def genericStaticMethod() = test("A.<Object>f()", {
    val A = NormalClassItem("A", LocalPkg)
    ApplyExp(TypeApply(NormalMethodItem("f",A,List(SimpleTypeVar("T")),IntType,Nil,isStatic=true),
                       List(ObjectType),a,hide=false),Nil,a,auto=false)
  })
  @Test def genericPlacement() = test("pkg.L.<String>fill(1,\"s\")", {
    val P = Package("pkg")
    val L = NormalClassItem("L", P, List(SimpleTypeVar("A")))
    val fill = NormalMethodItem("fill", L, List(SimpleTypeVar("A")), VoidType, List(IntType, StringItem.simple), isStatic=true)
    ApplyExp(TypeApply(fill,List(StringItem.simple),a,hide=false), List(IntLit(1,"1",r), StringLit("s","\"s\"",r)), a, auto=false)
  })
  @Test def genericNew() = test("new<String>A<Integer>(\"s\")", {
    val S = SimpleTypeVar("S")
    val T = SimpleTypeVar("T")
    val A = NormalClassItem("A",LocalPkg,List(S))
    val cons = NormalConstructorItem(A,List(T),List(T))
    ApplyExp(TypeApply(NewDen(r,None,cons,r,Some(Grouped(List(IntegerItem.simple),a))),List(StringItem.simple),a,hide=false),
             List(StringLit("s",""""s"""",r)),a,auto=false)
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
    test("f", MethodDen(None,f,r))
  }

  @Test def applyFixity() = {
    val A = NormalClassItem("A", LocalPkg)
    val y = NormalFieldItem("y",IntType,A)
    val x = NormalLocal("a", A.simple, isFinal=false)
    val f = NormalMethodItem("f",A,Nil,A.simple,Nil,isStatic=true)
    implicit val env = Env(Array(A,f,x), Map((x,1),(f,2)),PlaceInfo(f))
    test("a.f().f()", ApplyExp(MethodDen(ApplyExp(MethodDen(x,f,r),Nil,a,auto=false),f,r),Nil,a,auto=false))
    test("f().f()", ApplyExp(MethodDen(ApplyExp(f,Nil,a,auto=false),f,r),Nil,a,auto=false))
    test("a.f().f().y", FieldExp(ApplyExp(MethodDen(ApplyExp(MethodDen(x,f,r),Nil,a,auto=false),f,r),Nil,a,auto=false),y,r))
  }

  @Test def cond() = test("true ? 1 : 0",CondExp(true,r,1,r,0,IntType))

  @Test def finalVar() = test("final int x = 1;",VarStmt(List(Final),IntType,r,(NormalLocal("x",IntType),1),env))

  @Test def prefix() = test("!x",NonImpExp(NotOp,r,NormalLocal("x",BooleanType)))
  @Test def postfix() = test("x++",ImpExp(PostIncOp,r,NormalLocal("x",IntType)))

  @Test def newArray() = test("new int[]{1,2}",ArrayExp(r,IntType,r,List(1,2),a))
  @Test def newArrayN() = test("new int[2][]",EmptyArrayExp(r,ArrayType(IntType),r,List(Grouped(2:Exp,a))))
  @Test def newArrayNested() = test("new int[][]{{1,2},{3,4}}",
    ArrayExp(r,ArrayType(IntType),r,List(ArrayExp(r,IntType,r,List(1,2),a), ArrayExp(r,IntType,r,List(3,4),a)),a))

  @Test def fieldIndex() = {
    val A = NormalClassItem("A")
    val x = NormalLocal("x",A)
    val y = NormalFieldItem("y",ArrayType(IntType),A)
    test("x.y[0]",IndexExp(FieldExp(x,y,r),0,a))
  }
}
