package tarski

import tarski.Arounds._
import tarski.Constants._
import tarski.Denotations._
import tarski.Operators._
import tarski.Types._
import tarski.TestUtils._
import org.testng.annotations.Test
import org.testng.AssertJUnit._
import utility.Locations._

class TestConstants {
  val r = SRange.unknown
  val a = SGroup.unknown

  def test(c: OptionCon, e: Exp) = {
    assertEquals(c,toCon(e))
  }

  def sub(x: Exp, y: Exp) = BinaryExp(SubOp,r,x,y)

  @Test
  def arith() = test(IntCon(4-7),sub(CastExp(ByteType,a,4),7))
}
