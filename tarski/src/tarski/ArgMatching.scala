package tarski

import tarski.Denotations._
import tarski.Environment.Env
import tarski.Items.ArrayItem
import tarski.JavaScores.multiple
import tarski.Scores._
import tarski.Types._
import tarski.Semantics.valuesOfItem
import tarski.Tokens._
import utility.Utility._
import utility.Locations._
import scala.annotation.tailrec
import tarski.Pretty._

object ArgMatching {
  type Exps = List[Scored[Exp]]
  private implicit val showFlags = abbrevShowFlags

  def useAll(e: Exp, unused: Exps)(implicit env: Env): Scored[Exp] = unused match {
    case Nil => known(e)
    case _ => fail(s"${show(e)}: ${unused.size} unused arguments")
  }

  // If specified, expects constraints the return type, but only if there are no unused arguments.
  def fiddleCall[A](f: Callable, args: Exps, a: SGroup, expects: Option[Type], auto: Boolean, checkExpectedEarly: Boolean, cont: (Exp,Exps) => Scored[A])(implicit env: Env): Scored[A] = {
    // Should we find missing arguments in the environment?
    val useEnv = false
    // Incrementally add parameters and check whether the function still resolves
    val np = f.params.size
    val na = args.size
    def process(k: Int, targs: List[TypeArg], used: List[Exp], unused: Exps): Scored[A] = {
      if (k == np)
        cont(makeApply(Denotations.uncheckedAddTypeArgs(f,targs,a,hide=true),used,a,auto),unused)
      else {
        def add(x: Exp, prev: List[Scored[Exp]], next: List[Scored[Exp]]): Scored[A] = {

          def processNext(args: List[Exp], xs: List[Scored[Exp]]) = {
            val tys = args map (_.ty)
            val effectiveExpects = if (checkExpectedEarly || xs.isEmpty) expects else None
            resolveOption(f,tys,effectiveExpects) match {
              case None => fail(s"Can't apply ${show(f)}: ${f.params} to prefix ${tys mkString ", "}")
              case Some(ts) => process(k+1,ts,args,xs)
            }
          }

          // add x to the list of arguments
          val straightAdd = processNext(used :+ x, revAppend(prev,next))

          // TODO: use looseCompatible(f,tys,effectiveExpects) earlier to check whether it's legal to avoid generating tons of useless options
          val arrayAdd = if (f.params(k).item != ArrayItem) Empty else {
            val variadicParam = f.variadic && k == np-1
            val effectiveExpects = if (checkExpectedEarly || prev.isEmpty && next.isEmpty) expects else None

            // add new x.ty[]{x} to the list of arguments
            val singleton = ArrayExp(x.r.before,x.ty,x.r.before,List(x),SGroup(x.r.before,x.r.after))
            val args = used :+ singleton
            if (looseCompatible(f,args map (_.ty),effectiveExpects).isEmpty) Empty
            else {
              val singletonArray = biased(if (variadicParam) Pr.reasonable else Pr.convertToArray, processNext(args,revAppend(prev,next)))

              // try to use as many arguments as possible for the array we made (but don't use more than we can afford to lose)
              val minLeft: Int = if (useEnv) 0 else np - prev.size - k - 1 // we have to have at least this many things left in next: np - k - prev - 1
              val multipleArray = biased(if (variadicParam) Pr.reasonable else Pr.arrayContract, {
                  val usedtys = used map (_.ty)
                  def useMore(lastArray: ArrayExp, next: List[Scored[Exp]]): Scored[A] = if (next.size>minLeft) next match {
                    case Nil => Empty
                    case xden::next => xden flatMap { x =>
                      val ty = commonType(lastArray.t, x.ty)
                      val effectiveExpects = if (checkExpectedEarly || prev.isEmpty && next.isEmpty) expects else None
                      if (looseCompatible(f,usedtys:+ArrayType(ty),effectiveExpects).isEmpty) Empty
                      else {
                        val array = ArrayExp(lastArray.nr,ty,lastArray.tr,lastArray.i :+ x,SGroup(lastArray.a.l,x.r.after))
                        // if we have to resort to making Object[], penalize some more
                        biased(if (ty==ObjectType && lastArray.t != ObjectType) Pr.contractToObjectArray else Pr.reasonable,
                          processNext(used :+ array, revAppend(prev,next)) ++ useMore(array,next))
                      }
                    }
                  } else Empty
                  useMore(singleton,next)
                })

              singletonArray ++ multipleArray
            }
          }

          straightAdd ++ arrayAdd
        }
        type Opts = List[Scored[A]]
        val options0: Opts = unused match {
          case Nil => if (useEnv) Nil else impossible
          case x::xs => {
            // Use the next argument
            val first = x flatMap (add(_,Nil,xs))
            // Use a different argument
            @tailrec
            def shuffle(prev: Exps, next: Exps, opts: Opts): Opts = next match {
              case Nil => opts
              case x::next => {
                shuffle(x::prev,next,biased(Pr.shuffleArgs,x flatMap (add(_,prev,next))) :: opts)
              }
            }
            shuffle(List(x),xs,List(first))
          }
        }
        // If desired, find values from the scope that fit
        val options1: Opts = if (!useEnv) options0 else biased(Pr.addArg,
          // TODO: if f.params(k).item is ArrayItem, early discard everything returned by byItem which is not of the right sort of array
          // TODO: if f.params(k).item is ArrayItem, also look for its inner type to pass to add (which converts)
          valuesOfItem(f.params(k).item,a.r,qualifiers=Nil) flatMap (add(_,Nil,unused))) :: options0
        multiple(options1)
      }
    }
    def processNullary: Scored[A] = // Special case nullary functions to make sure we do at least one inference round
      if (f.tparams.size == 0) cont(makeApply(f,Nil,a,auto),args)
      else resolveOption(f,Nil,if (args.isEmpty) expects else None) match {
        case None => fail(s"Can't apply $f to no arguments")
        case Some(ts) => cont(makeApply(Denotations.uncheckedAddTypeArgs(f,ts,a,hide=true),Nil,a,auto),args)
      }
    if (!useEnv && np > na) fail(s"Too few arguments for function $f: $na < $np")
    else orError(biased(if (f.variadic) Pr.variadicCall else Pr.dropArgs(math.max(0,na-np)), if (np>0) process(0,null,Nil,args) else processNullary),
                 s"Can't match arguments for function ${show(f)}: ${f.params}")
  }
}
