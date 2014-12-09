package tarski

import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.Base._
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
}
