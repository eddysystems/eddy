package tarski

import tarski.Denotations.{ApplyExp, Callable, Exp}
import tarski.Environment.Env
import tarski.Scores._
import tarski.Types._
import tarski.Semantics.denoteValue
import ambiguity.Utility._

import scala.annotation.tailrec

object ArgMatching {
  def fiddleCall(f: Callable, args: List[Scored[Exp]])(implicit env: Env): Scored[ApplyExp] = {
    // Should we find missing arguments in the environment?
    val useEnv = false
    // Incrementally add parameters and check whether the function still resolves
    val n = f.params.size
    val na = args.size
    type Args = (List[RefType],List[Exp])
    def process(k: Int, targs: List[RefType], used: List[Exp], unused: List[Scored[Exp]]): Scored[Args] = {
      if (k == n)
        known((targs,used))
      else {
        def add(p: Prob, x: Exp, xs: List[Scored[Exp]]): Scored[Args] = {
          val args = used :+ x
          resolveOptions(List(f),args map (_.ty)) match {
            case Nil => empty
            case List((f0,ts)) if f eq f0 => process(k+1,ts,args,xs) bias p
            case _ => impossible
          }
        }
        type Opts = List[Alt[() => Scored[Args]]]
        val options0: Opts = unused match {
          case Nil => if (useEnv) Nil else impossible
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
            shuffle(List(x),xs,List(first))
          }
        }
        // If desired, find values from the scope that fit
        val options1: Opts = if (!useEnv) options0 else Alt(Pr.addArg,() =>
          env.byItem(f.params(k).item) flatMap (denoteValue(_,0)) flatMap (add(Pr.addArg,_,unused))) :: options0
        multiple(options1)
      }
    }
    if (!useEnv && n > na) fail(s"Too few arguments for function $f: $na < $n")
    else orError(process(0,Nil,Nil,args) bias Pr.dropArgs(math.max(0,na-n)) map { case (ts,xs) => ApplyExp(f,ts,xs) },
                 s"Can't match arguments for function $f.")
  }
}
