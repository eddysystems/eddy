package ambiguity

import ambiguity.Graphs._
import utility.Utility._
import org.testng.annotations.Test
import org.testng.AssertJUnit._

class TestGraph {
  // Example from https://en.wikipedia.org/wiki/Strongly_connected_component
  val G = Map('a' -> "b",
              'b' -> "cfe",
              'c' -> "dg",
              'd' -> "ch",
              'e' -> "fa",
              'f' -> "g",
              'g' -> "f",
              'h' -> "dg").mapValues(_.toList)

  @Test def depth() = {
    val order = "abcdhgfe".zipWithIndex.toMap
    assertEquals(order,depthFirst('a',G))
  }

  @Test def strong() = {
    val scc = splitWhitespace("abe cdh gf").map(_.toList)
    assertEquals(scc,strongComponents('a',G))
  }
}
