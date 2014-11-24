package tarski

import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.Constants._
import tarski.Denotations._
import tarski.Operators._
import tarski.Types._
import tarski.TestUtils._

import scala.language.implicitConversions

class TestConstants {
  def test(c: OptionCon, e: Exp) = {
    assertEquals(c,toCon(e))
  }

  def sub(x: Exp, y: Exp) = BinaryExp(SubOp,x,y)

  @Test
  def arith() = test(IntCon(4-7),sub(CastExp(ByteType,4),7))
}
