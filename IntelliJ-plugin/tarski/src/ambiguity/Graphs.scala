package ambiguity

import scala.collection.mutable

object Graphs {
  // Preorder all reachable nodes in directed graph
  def depthFirst[N](root: N, out: N => Traversable[N]): Map[N,Int] = {
    var next = 0
    val label = mutable.Map[N,Int]()
    def visit(v: N): Unit = if (!label.contains(v)) {
      label(v) = next
      next += 1
      out(v) foreach visit
    }
    visit(root)
    label.toMap
  }

  // Find all reachable strong components of a directed graph
  def strongComponents[N](root: N, out: N => Traversable[N]): List[List[N]] = {
    // See https://en.wikipedia.org/wiki/Path-based_strong_component_algorithm
    val S = mutable.Stack[N]()
    val P = mutable.Stack[N]()
    var next = 0
    val index = mutable.Map[N,Int]()
    var comps: List[List[N]] = Nil
    def visit(v: N): Unit = {
      index(v) = next
      next += 1
      S.push(v)
      P.push(v)
      out(v) foreach (w => index get w match {
        case None => visit(w)
        case Some(wn) => while (P.nonEmpty && index(P.top)>wn) P.pop()
      })
      if (P.nonEmpty && P.top==v) {
        def comp(c: List[N]): List[N] = {
          val s = S.pop()
          val sc = s :: c
          if (s == v) sc else comp(sc)
        }
        comps = comp(Nil) :: comps
        P.pop()
      }
    }
    visit(root)
    comps
  }
}
