package tarski

import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.Base._
import tarski.Inference.{InferError, Var, infer, looseBounds}
import tarski.Items._
import tarski.Types._
import tarski.TestUtils._

import scala.language.implicitConversions

class TestInfer {
  val T = SimpleTypeVar("T")
  implicit def toVarType[A](x: (Var,A))(implicit f: A => RefType): (Var,RefType) = (x._1,f(x._2))

  def testInfer(goal: (Var,TypeArg)*)(ts: Type*)(as: Type*): Unit = {
    val (vs,rts) = goal.toList.unzip
    assertEquals(Some(rts),infer(vs,ts.toList,as.toList)(looseBounds))
  }

  def testInferFail(vs: Var*)(ts: Type*)(as: Type*): Unit = {
    try assertEquals(None,infer(vs.toList,ts.toList,as.toList)(looseBounds))
    catch { case _:InferError => () }
  }

  @Test def simpleObject() = testInfer(T -> ObjectType)(T)(ObjectType)

  @Test def simpleInteger() = testInfer(T -> IntType.box)(T)(IntType.box)

  @Test def simpleInt() = testInfer(T -> IntType.box)(T)(IntType)

  @Test def lubIntegerFloat() = testInfer(T -> NumberItem)(T,T)(IntType.box,FloatType.box)

  @Test def lubIntFloat() = testInfer(T -> NumberItem)(T,T)(IntType,FloatType.box)

  @Test def intFail() = testInferFail()(IntType.box)(NumberItem)

  @Test def simpleNull() = testInfer(T -> ObjectType)(T)(NullType)

  @Test def wildcard() = {
    val S = SimpleTypeVar("S")
    val A = NormalClassItem("A",LocalPkg,List(S))
    def check(t: TypeArg): Unit = t match {
      case t:TypeVar => assert(t.lo==NullType && t.hi==NumberItem.simple,s"t $t, lo ${t.lo}, hi ${t.hi}")
      case _ => throw new AssertionError(s"Expected a fresh type variable, got $t")
    }
    testInfer(T -> WildSub(NumberItem))(A.generic(List(T)))(A.generic(List(WildSub(NumberItem))))
  }

  // Warn if debugging is left on
  @Test def noDebugInfer() = assertEquals(false,Inference.debug)
}
