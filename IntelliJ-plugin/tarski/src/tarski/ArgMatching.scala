package tarski

import tarski.Denotations.{ApplyExp, Callable, Exp}
import tarski.Environment.Env
import tarski.Scores._
import tarski.Types._
import ambiguity.Utility._

import scala.annotation.tailrec

object ArgMatching {
  def fiddleCall(f: Callable, args: List[Scored[Exp]])(implicit env: Env): Scored[ApplyExp] = {
    // Incrementally add parameters and check whether the function still resolves
    val n = f.params.size
    type Args = (List[RefType],List[Exp])
    def process(k: Int, targs: List[RefType], used: List[Exp], unused: List[Scored[Exp]]): Scored[Args] = {
      if (k == n)
        known((targs,used))
      else {
        def add(p: Prob, x: Exp, xs: List[Scored[Exp]]): Scored[Args] = {
          val args = used :+ x
          resolveOptions(List(f),args map (_.ty)) match {
            case Nil => empty
            case List((f0,targs)) if f eq f0 => process(k+1,targs,args,xs) bias p
          }
        }
        type Opts = List[Alt[() => Scored[Args]]]
        val options0: Opts = unused match {
          case Nil => Nil
          case x::xs => {
            // Use the next argument
            val first = Alt(Pr.certain,() => x flatMap (add(Pr.certain,_,xs)))
            // Use a different argument
            @tailrec
            def shuffle(prev: List[Scored[Exp]], next: List[Scored[Exp]], opts: Opts): Opts = next match {
              case Nil => opts
              case x::next => {
                val xs = revAppend(prev,next)
                shuffle(x::prev,next,Alt(Pr.shuffleArgs,() => x flatMap (add(Pr.shuffleArgs,_,xs))) :: opts)
              }
            }
            shuffle(Nil,xs,List(first))
          }
        }
        /*
        // use a value from the scope that fits
        options = Alt(Pr.addArg, () => {{ env.byItem(f.params.apply(pos).item)
        } flatMap {
          Semantics.denoteValue(_,0)
        } flatMap {
          check_and_add(_, remaining, Pr.addArg)
        }}) :: options
        */
        multiple(options0)
      }
    }
    orError(process(0,Nil,Nil,args) bias Pr.dropArgs(math.max(0,args.size - f.params.size)) map { case (ts,xs) => ApplyExp(f,ts,xs) },
            s"Can't match arguments for function $f.")
  }
}
