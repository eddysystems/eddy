package tarski

import tarski.AST.CommaList
import tarski.Denotations.{ApplyExp, Exp, Callable}
import tarski.Environment.Env
import tarski.Pretty._
import tarski.Scores._
import tarski.Tokens._
import tarski.Types._

object ArgMatching {
  def permuteHelper[A](prefix: Seq[A], length: Int, end: Map[A,Int], prefixLegal: Seq[A] => Boolean): Seq[Seq[A]] = {
    if (prefix.size == length)
      Seq(prefix)
    else {
      // choose a member of the set and add to prefix
      val perms = for {(c,mult) <- end
                       newprefix = prefix :+ c
                       if prefixLegal(newprefix)
                       newend = if (mult == 1) end - c else end + ((c,mult-1))
                      }
                    yield permuteHelper(newprefix, length, newend, prefixLegal)
      perms.flatten.toSeq
    }
  }

  def permute[A](in: Seq[A], prefixLegal: Seq[A] => Boolean): Seq[Seq[A]] = {
    // make in into a map
    var map = Nil.toMap[A,Int]
    for (a <- in) {
      map.get(a) match {
        case None => map += ((a,1))
        case Some(mult) => map += ((a,mult+1))
      }
    }

    permuteHelper[A](Nil, in.size, map, prefixLegal)
  }

  def fiddleArgs(f: Callable, args: List[Exp])(implicit env: Env): Scored[Exp] = {
    // TODO: allow inserting or dropping arguments
    val fn = f.params.size
    if (fn != args.size)
      fail(show(pretty(f))+s": expected $fn arguments (${show(CommaList(f.params))}), got ${args.size} ($args)")
    else {
      // if the original order of arguments fits, don't bother trying something else
      resolve(List(f), args.toList map (_.ty)).map({case (f,ts) => single(ApplyExp(f, ts, args), Pr.certain)}).getOrElse {
        // if not, fiddle!
        val validPermutations = permute[Exp](args, xs => resolveOptions(List(f), xs.toList map (_.ty)).nonEmpty ).toList
        val scores = multiple(validPermutations flatMap { p => resolve(List(f), p.toList map (_.ty)) match {
          case None => Nil
          case Some((_,ts)) => List(Alt(Pr.certain, ApplyExp(f, ts, p.toList)))
        }}, show(f)+": params "+show(tokensSig(f))+" don't match arguments "+show(CommaList(args))+" with types "+show(CommaList(args map (_.ty))))
        scores.flatMap( x => single(x, Pr.permuteArgs(f, args, x.args)) )
      }
    }
  }
}
