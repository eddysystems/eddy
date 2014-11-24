package tarski

import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.Base._
import tarski.Denotations.ThisExp
import tarski.Environment._
import tarski.Items._
import tarski.Pretty._
import tarski.Tokens._
import tarski.Types._

class TestEnvironment {

  @Test
  def testStaticShadowedByLocal() = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val yf = StaticFieldItem("y",FloatType,main,true)
    val f = StaticMethodItem("f",main,Nil,FloatType,List(ArrayType(IntType)))
    val y = LocalVariableItem("y",ArrayType(DoubleType),true)
    val scope = Map[Item,Int]((LocalPkg,4),(main,3),(yf,2),(f,2),(y,1))
    implicit val env = Env(List(main,f),scope)
    assertEquals(tokens(y), List(IdentTok("y")))
    assertEquals(tokens(yf), List(IdentTok("Main"),DotTok(), IdentTok("y")))
  }

  @Test
  def nestedThisExp(): Unit = {
    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val Y = NormalClassItem("Y", X, Nil, ObjectType, Nil)
    val tX = ThisItem(X)
    val tY = ThisItem(Y)
    implicit val env = Env(List(X,Y,tX,tY), Map((tX,2),(X,2),(tY,1),(Y,1)))
    assertEquals(tokens(ThisExp(tX)), List(IdentTok("X"),DotTok(),ThisTok()))
    assertEquals(tokens(ThisExp(tY)), List(ThisTok()))
  }

  @Test
  def levenshteinDistance(): Unit = {
    val meant = "isInstanceOf"
    val typed = "isInstnaceof"
    val d = StringMatching.levenshteinDistance(meant, typed)
    println(s"Levenshtein Distance $meant -> $typed = $d")
  }
}
