package tarski

import org.testng.annotations.Test
import org.testng.AssertJUnit._
import tarski.Items._
import tarski.Types._
import tarski.TestUtils._
import tarski.Inference.{Var,Bound,Fixed,Bounded,infer,looseBounds}

class TestInfer {
  def v(s: String): Var = TypeParamItem(s)

  def testInfer(goal: (Var,RefType)*)(ts: Type*)(as: Type*): Unit = {
    val (vs,rts) = goal.toList.unzip
    assertEquals(Some(rts),infer(vs,ts.toList,as.toList)(looseBounds))
  }

  @Test
  def simpleObject(): Unit = {
    val T = v("T")
    testInfer(T -> ObjectType)(ParamType(T))(ObjectType)
  }
}
