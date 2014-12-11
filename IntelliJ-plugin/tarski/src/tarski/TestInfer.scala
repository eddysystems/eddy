package tarski

import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.Base._
import tarski.Inference.{Var, infer, looseBounds}
import tarski.Items._
import tarski.Types._
import tarski.TestUtils._

import scala.language.implicitConversions

class TestInfer {
  def v(s: String): Var = SimpleTypeVar(s)
  val T = v("T")
  implicit def toVarType[A](x: (Var,A))(implicit f: A => RefType): (Var,RefType) = (x._1,f(x._2))

  def testInfer(goal: (Var,RefType)*)(ts: Type*)(as: Type*): Unit = {
    val (vs,rts) = goal.toList.unzip
    assertEquals(Some(rts),infer(vs,ts.toList,as.toList)(looseBounds))
  }

  def testInferFail(vs: Var*)(ts: Type*)(as: Type*): Unit = {
    assertEquals(None,infer(vs.toList,ts.toList,as.toList)(looseBounds))
  }

  @Test
  def simpleObject() = testInfer(T -> ObjectType)(T)(ObjectType)

  @Test
  def simpleInteger() = testInfer(T -> IntType.box)(T)(IntType.box)

  @Test
  def simpleInt() = testInfer(T -> IntType.box)(T)(IntType)

  @Test
  def lubIntegerFloat() = testInfer(T -> NumberItem)(T,T)(IntType.box,FloatType.box)

  @Test
  def lubIntFloat() = testInfer(T -> NumberItem)(T,T)(IntType,FloatType.box)

  @Test
  def intFail() = testInferFail()(IntType.box)(NumberItem)

  @Test
  def simpleNull() = testInfer(T -> ObjectType)(T)(NullType)
}
