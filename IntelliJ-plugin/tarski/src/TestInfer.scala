package tarski

import org.testng.annotations.Test
import org.testng.AssertJUnit._
import tarski.Base._
import tarski.Items._
import tarski.Types._
import tarski.TestUtils.{toType,_}
import tarski.Inference.{Var,Bound,Fixed,Bounded,infer,looseBounds}
import scala.language.implicitConversions

class TestInfer {
  def v(s: String): Var = TypeParamItem(s)
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
  def simpleInteger() = testInfer(T -> IntRefType)(T)(IntRefType)

  @Test
  def simpleInt() = testInfer(T -> IntRefType)(T)(IntType)

  @Test
  def lubIntegerFloat() = testInfer(T -> NumberItem)(T,T)(IntRefType,FloatRefType)

  @Test
  def lubIntFloat() = testInfer(T -> NumberItem)(T,T)(IntType,FloatRefType)

  @Test
  def intFail() = testInferFail()(IntRefType)(NumberItem)

  @Test
  def simpleNull() = testInfer(T -> ObjectType)(T)(NullType)
}
