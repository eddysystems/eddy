package tarski

import org.testng.AssertJUnit._
import org.testng.annotations.Test
import ambiguity.Utility._
import tarski.Base._
import tarski.Denotations.ThisExp
import tarski.Environment._
import tarski.Items._
import tarski.Pretty._
import tarski.Scores.Alt
import tarski.Tokens._
import tarski.Tries._
import tarski.Types._

class TestEnvironment {

  @Test
  def testStaticShadowedByLocal() = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val yf = NormalStaticFieldItem("y",FloatType,main,true)
    val f = NormalMethodItem("f",main,Nil,FloatType,List(ArrayType(IntType)),true)
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

  @Test def trieExactQuery(): Unit = {
    val typed = List("garbage","tes","LongLongNameTest")
    val things = List("test","tset","verylongName","LongLongName","TestName","testName","NameTest","iTest")
      .map(NormalClassItem(_,LocalPkg))
    val env = new Env(things)

    // these should all return nothing
    for (name <- typed) {
      val alts = env.exactQuery(name)
      assertEquals(s"found $name in trie as $alts", alts, Nil)
    }

    // these should all return exactly one
    for (i <- things) {
      val e = i.name.length * Pr.typingErrorRate
      val p = Pr.poissonPDF(e,0)
      val x = env.exactQuery(i.name)
      assertEquals(s"found $x, expected exactly $i", List(Alt(p,i)), x)
    }
  }

  @Test def trie() = {
    case class Name(name: String) extends Named
    def W(s: String) = splitWhitespace(s) map Name
    def S(s: Int*) = s.toList
    val st = List(
      W("")                  -> S(0,0, 0),
      List(Name(""))         -> S(0,0, 1),
      W("a")                 -> S(0,1,'a',4, 0,0,1),
      W("a b")               -> S(0,2,'a',6,'b',8, 0,0, 1,0, 2),
      W("a ab")              -> S(0,1,'a',4, 0,1,'b',8, 1,0, 2),
      W("ab a")              -> S(0,1,'a',4, 0,1,'b',8, 1,0, 2),
      W("ab a a")            -> S(0,1,'a',4, 0,1,'b',8, 2,0, 3),
      W("a bc")              -> S(0,2,'a',6,'b',8, 0,0, 1,1,'c',12, 1,0, 2),
      W("a ab abc")          -> S(0,1,'a',4, 0,1,'b',8, 1,1,'c',12, 2,0, 3),
      W("a ab bc")           -> S(0,2,'a',6,'b',12, 0,1,'b',10, 1,0, 2,1,'c',16, 2,0, 3),
      W("a ab abc bc")       -> S(0,2,'a',6,'b',16, 0,1,'b',10, 1,1,'c',14, 2,0, 3,1,'c',20, 3,0, 4),
      (Name("")::W("a abc ab bc")) -> S(0,2,'a',6,'b',16, 1,1,'b',10, 2,1,'c',14, 3,0, 4,1,'c',20, 4,0, 5)
    )
    for ((w,st) <- st) {
      println(s"\ntrie: $w")
      val t = Trie(w)
      assertEquals(st,t.structure.toList)
      for ((s,ss) <- w groupBy (_.name))
        assertEquals(ss,t.exact(s))
      assertEquals(Nil,t.exact("blah"))
    }
    for ((w0,_) <- st; (w1,_) <- st) {
      println(s"\nmerge: $w0, $w1")
      val t0 = Trie(w0)
      val t1 = Trie(w1)
      for (t <- List(t0++t1,t0++w1))
        assertEquals((w0++w1).sorted(math.Ordering.by[Named,String](_.name)),t.values.toList)
    }
  }

  @Test
  def trieQuery(): Unit = {
    val typed = "test"
    val things = List("test","tset","verylongName","LongLongName","TestName","testName","NameTest","iTest")
      .map(NormalClassItem(_,LocalPkg))
    val env = new Env(things)

    val qr = env.query(typed).toSet
    val lr = things.collect( Function.unlift((item:Item) => {
      val p = Pr.typoProbability(item.name, typed)
      if (p > env.minimumProbability) Some(Alt(p,item)) else None
    })).toSet

    println(s"exact match for query $typed -> ${env.exactQuery(typed)}")
    println("trie query: " + qr)
    println("list query: " + lr)

    assertEquals("Trie query failed to find high probability item(s).", qr, lr)
  }
}
