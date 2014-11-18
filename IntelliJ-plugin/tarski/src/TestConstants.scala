package tarski

import tarski.Constants._
import tarski.Denotations._
import tarski.AST._
import tarski.TestUtils._
import tarski.Types._
import scala.language.implicitConversions
import org.testng.annotations.Test
import org.testng.AssertJUnit._

class TestConstants {
  def test(c: OptionCon, e: Exp) = {
    assertEquals(c,toCon(e))
  }

  def sub(x: Exp, y: Exp) = BinaryExp(SubOp,x,y)

  @Test
  def arith() = test(IntCon(4-7),sub(CastExp(ByteType,4),7))
}
