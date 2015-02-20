package tarski

import tarski.Denotations.{ApplyExp,Callable,Exp}
import tarski.Environment.Env
import tarski.Scores._
import tarski.JavaScores._
import tarski.Types._
import tarski.Semantics.denoteValue
import tarski.Tokens._
import tarski.Arounds._
import utility.Utility._
import utility.Locations._
import scala.annotation.tailrec

object ArgMatching {
  type Exps = List[Scored[Exp]]
  private implicit val showFlags = abbrevShowFlags

  def useAll(e: ApplyExp, unused: Exps)(implicit env: Env): Scored[Exp] = unused match {
    case Nil => known(e)
    case _ => fail(s"${show(e)}: ${unused.size} unused arguments")
  }

  // If specified, expects constraints the return type, but only if there are no unused arguments.
  def fiddleCall[A](f: Callable, args: Exps, a: SGroup, expects: Option[Type], auto: Boolean, cont: (ApplyExp,Exps) => Scored[A])(implicit env: Env): Scored[A] = {
    // Should we find missing arguments in the environment?
    val useEnv = false
    // Incrementally add parameters and check whether the function still resolves
    val np = f.params.size
    val na = args.size
    def process(k: Int, targs: List[TypeArg], used: List[Exp], unused: Exps): Scored[A] = {
      if (k == np)
        cont(ApplyExp(Denotations.uncheckedAddTypeArgs(f,targs,a,hide=true),used,a,auto),unused)
      else {
        def add(x: Exp, xs: List[Scored[Exp]]): Scored[A] = {
          val args = used :+ x
          val tys = args map (_.ty)
          resolveOption(f,tys,if (xs.isEmpty) expects else None) match {
            case None => fail(s"Can't apply $f to prefix ${tys mkString ", "}")
            case Some(ts) => process(k+1,ts,args,xs)
          }
        }
        type Opts = List[Scored[A]]
        val options0: Opts = unused match {
          case Nil => if (useEnv) Nil else impossible
          case x::xs => {
            // Use the next argument
            val first = x flatMap (add(_,xs))
            // Use a different argument
            @tailrec
            def shuffle(prev: Exps, next: Exps, opts: Opts): Opts = next match {
              case Nil => opts
              case x::next => {
                val xs = revAppend(prev,next)
                shuffle(x::prev,next,biased(Pr.shuffleArgs,x flatMap (add(_,xs))) :: opts)
              }
            }
            shuffle(List(x),xs,List(first))
          }
        }
        // If desired, find values from the scope that fit
        val options1: Opts = if (!useEnv) options0 else biased(Pr.addArg,
          env.byItem(f.params(k).item) flatMap (denoteValue(_,a.r,qualifiers=Nil)) flatMap (add(_,unused))) :: options0
        multiple(options1)
      }
    }
    def processNullary: Scored[A] = // Special case nullary functions to make sure we do at least one inference round
      if (f.tparams.size == 0) cont(ApplyExp(f,Nil,a,auto),args)
      else resolveOption(f,Nil,if (args.isEmpty) expects else None) match {
        case None => fail(s"Can't apply $f to no arguments")
        case Some(ts) => cont(ApplyExp(Denotations.uncheckedAddTypeArgs(f,ts,a,hide=true),Nil,a,auto),args)
      }
    if (!useEnv && np > na) fail(s"Too few arguments for function $f: $na < $np")
    else orError(biased(Pr.dropArgs(math.max(0,na-np)),if (np>0) process(0,null,Nil,args) else processNullary),
                 s"Can't match arguments for function $f")
  }
}
