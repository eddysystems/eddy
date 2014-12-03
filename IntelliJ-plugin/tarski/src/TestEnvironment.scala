package tarski

import org.testng.AssertJUnit._
import org.testng.annotations.Test
import tarski.Base._
import tarski.Denotations.ThisExp
import tarski.Environment._
import tarski.Items._
import tarski.Pretty._
import tarski.Scores.Alt
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
    implicit val env = new Env(List(main,f),scope)
    assertEquals(tokens(y), List(IdentTok("y")))
    assertEquals(tokens(yf), List(IdentTok("Main"),DotTok(), IdentTok("y")))
  }

  @Test
  def nestedThisExp(): Unit = {
    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val Y = NormalClassItem("Y", X, Nil, ObjectType, Nil)
    val tX = ThisItem(X)
    val tY = ThisItem(Y)
    implicit val env = new Env(List(X,Y,tX,tY), Map((tX,2),(X,2),(tY,1),(Y,1)))
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

  @Test
  def trieQuery(): Unit = {
    val typed = "test"
    val things = List(NormalClassItem("test", LocalPkg, Nil, ObjectType, Nil),
                       NormalClassItem("tset", LocalPkg, Nil, ObjectType, Nil),
                       NormalClassItem("verylongName", LocalPkg, Nil, ObjectType, Nil),
                       NormalClassItem("LongLongName", LocalPkg, Nil, ObjectType, Nil),
                       NormalClassItem("TestName", LocalPkg, Nil, ObjectType, Nil),
                       NormalClassItem("testName", LocalPkg, Nil, ObjectType, Nil),
                       NormalClassItem("NameTest", LocalPkg, Nil, ObjectType, Nil),
                       NormalClassItem("iTest", LocalPkg, Nil, ObjectType, Nil)
                       )
    val env = new Env(things)

    val qr = env.query(typed)
    val lr = things.collect( Function.unlift((item:Item) => {
      val p = Pr.typoProbability(item.name, typed)
      if (p > env.minimumProbability) Some(Alt(p,item)) else None
    }))

    println(s"exact match for query $typed -> ${env.exactQuery(typed)}")
    println("trie query: " + qr)
    println("list query: " + lr)

    assertEquals("Trie query failed to find high probability item(s).", qr, lr)
  }
}
