package tarski

import utility.Utility._
import tarski.TestUtils._
import tarski.Denotations.ThisExp
import tarski.Environment._
import tarski.Items._
import tarski.JavaItems._
import tarski.JavaScores.pp
import tarski.Pretty._
import tarski.Scores.Alt
import tarski.Tokens._
import tarski.Tries._
import tarski.Types._
import org.testng.annotations.Test
import org.testng.AssertJUnit._
import scala.collection.JavaConverters._
import scala.util.Random

class TestEnvironment {

  @Test def testStaticShadowedByLocal() = {
    val main = NormalClassItem("Main",LocalPkg,Nil,ObjectType,Nil)
    val yf = NormalStaticFieldItem("y",FloatType,main,isFinal=true)
    val f = NormalMethodItem("f",main,Nil,FloatType,List(ArrayType(IntType)),isStatic=true)
    val y = NormalLocal("y",ArrayType(DoubleType),isParameter=false,isFinal=true)
    val scope = Map[Item,Int]((LocalPkg,4),(main,3),(yf,2),(f,2),(y,1))
    implicit val env = Env(Array(main,f),scope)
    assertEquals(tokens(y), List(IdentTok("y")))
    assertEquals(tokens(yf), List(IdentTok("Main"),DotTok, IdentTok("y")))
  }

  @Test def nestedThisExp(): Unit = {
    val X = NormalClassItem("X", LocalPkg, Nil, ObjectType, Nil)
    val Y = NormalClassItem("Y", X, Nil, ObjectType, Nil)
    val tX = ThisItem(X)
    val tY = ThisItem(Y)
    implicit val env = Env(Array(X,Y,tX,tY), Map((tX,2),(X,2),(tY,1),(Y,1)))
    assertEquals(tokens(ThisExp(tX)), List(IdentTok("X"),DotTok,ThisTok))
    assertEquals(tokens(ThisExp(tY)), List(ThisTok))
  }

  @Test def levenshteinDistance(): Unit = {
    val meant = "isInstanceOf"
    val typed = "isInstnaceof"
    val d = StringMatching.levenshteinDistance(meant, typed)
    println(s"Levenshtein Distance $meant -> $typed = $d")
  }

  @Test def trieExactQuery(): Unit = {
    val typed = List("garbage","tes","LongLongNameTest")
    val things = Array("test","tset","verylongName","LongLongName","TestName","testName","NameTest","iTest")
      .map(s => NormalClassItem(s,LocalPkg) : Item)
    val env = Env(things)

    // these should all return nothing
    for (name <- typed) {
      val alts = env.exactQuery(name)
      assertEquals(s"found $name in trie as $alts", alts, Nil)
    }

    // these should all return exactly one
    for (i <- things) {
      val x = env.exactQuery(i.name)
      assertEquals(s"found $x, expected exactly $i",List(i),x)
    }
  }

  @Test def poisson() = {
    assertTrue(math.abs(0.213763017249736 - utility.JavaUtils.poissonPDF(2.5, 3)) < 1e-6)
    assertTrue(math.abs(0.037833274802070 - utility.JavaUtils.poissonPDF(10, 5)) < 1e-6)
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
        assertEquals(ss,t.exact(s.toCharArray))
      assertEquals(Nil,t.exact("blah".toCharArray))
    }
    for ((w0,_) <- st; (w1,_) <- st) {
      //println(s"\nmerge: $w0, $w1")
      val t0 = Trie(w0)
      val t1 = Trie(w1)
      for (t <- List(t0++t1,t0++w1))
        assertEquals((w0++w1).sorted(math.Ordering.by[Named,String](_.name)),t.values.toList)
    }
  }

  @Test def trieQuery(): Unit = {
    val typed = "test"
    val things = Array("test","tset","verylongName","LongLongName","TestName","testName","NameTest","iTest")
      .map(s => NormalClassItem(s,LocalPkg) : Item)
    val env = Env(things)

    val qr = env.typoQuery(typed).toSet
    val lr = things.collect( Function.unlift((item:Item) => {
      if (item.name == typed) None // Exact matches are not typos
      else {
        val p = Pr.typoProbability(item.name, typed)
        if (pp(p) >= Pr.minimumProbability) Some(Alt(p,item)) else None
      }
    })).toSet

    println(s"exact match for query $typed -> ${env.exactQuery(typed)}")
    println("trie query: " + qr)
    println("list query: " + lr)

    assertEquals("Trie query failed to find high probability item(s).", qr, lr)
  }

  // Warn if exactOnly is true
  @Test def noExactOnly(): Unit = assertEquals(false,exactOnly)

  @Test def valuesByItemTest(): Unit = {
    val A = NormalInterfaceItem("A")
    val B = NormalInterfaceItem("B")
    val C = NormalInterfaceItem("C",interfaces=List(A.simple,B.simple))
    val D = NormalInterfaceItem("D",interfaces=List(B.simple))
    val E = NormalInterfaceItem("E",interfaces=List(D.simple))
    val types = Array(A,B,C,D,E)
    val random = new Random(17311)
    for (i <- 0 until 100; n <- 0 until 100) {
      def randomItem(k: Int): Item = {
        if (random.nextDouble < .1) SimpleTypeVar(s"T$k")
        else NormalLocal(s"x$k",types(random.nextInt(types.length)))
      }
      val items = Array.tabulate(n)(randomItem)
      val values = items collect {case v:Local => v}
      val by = valuesByItem(items).asScala

      // All values should be included
      assertSetsEqual(values,by.values.flatten)
      // Entries should be unique and valid
      by foreach {case (i,vs) =>
        assert(vs.size == vs.toSet.size)
        vs foreach (v => assert(isSubitem(v.item,i)))
      }
      // Every super of a value should be included, except for Object
      values foreach (v => v.item.superItems foreach (i => if (i != ObjectItem) assert(by(i) contains v)))
    }
  }
}
