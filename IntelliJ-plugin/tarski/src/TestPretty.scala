package tarski

import tarski.Items._
import tarski.Pretty._
import tarski.Lexer._
import tarski.Tokens._
import tarski.Base._
import tarski.Types._
import scala.language.implicitConversions
import org.testng.annotations.Test
import org.testng.AssertJUnit._

class TestPretty {
  implicit val env = baseEnv

  def test[A](s: String, x: A)(implicit p: Pretty[A]) =
    assertEquals(lex(s).filterNot(isSpace),tokens(x))

  @Test def lang() = test("java.lang", JavaLangPkg)
  @Test def obj() = test("Object", ObjectType)
  @Test def objMethod() = test("Object.f",StaticMethodItem("f",ObjectItem,Nil,VoidType,Nil))
  @Test def clsMethod() = test("A.f",StaticMethodItem("f",NormalClassItem("A",LocalPkg,Nil),Nil,VoidType,Nil))
}
