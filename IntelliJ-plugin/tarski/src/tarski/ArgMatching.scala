package tarski

import tarski.Denotations.{ApplyExp, Callable, Exp}
import tarski.Environment.Env
import tarski.Scores._
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

  def fiddleCall(f: Callable, args: List[Scored[Exp]])(implicit env: Env): Scored[ApplyExp] = {

    // incrementally add parameters and check whether the function still resolves
    def add(f: Callable, pos: Int, args_and_remaining: Scored[(List[Exp], List[Scored[Exp]])]): Scored[List[Exp]] = {
      if (pos == f.params.size)
        return args_and_remaining map (_._1)

      def nextarg(in: (List[Exp], List[Scored[Exp]])): Scored[(List[Exp], List[Scored[Exp]])] = in match { case (args, remaining) =>
        def check_and_add(r: Exp, remaining: List[Scored[Exp]], p: Prob): Scored[(List[Exp], List[Scored[Exp]])] = r match {
            case x if resolveOptions(List(f), (args :+ x) map (_.ty) ).isEmpty => empty
            case x => single((args :+ x, remaining), p)
        }

        var options: List[Alt[() => Scored[(List[Exp], List[Scored[Exp]])]]] = Nil
        if (remaining.nonEmpty) {
          // use the next argument in line
          options = Alt(Pr.certain, () => remaining.head.flatMap(check_and_add(_, remaining.tail, Pr.certain))) :: options

          // use another yet unused argument
          options = options ::: ((1 until remaining.size).toList map { idx =>
            Alt(Pr.shuffleArgs, () => remaining.apply(idx).flatMap(check_and_add(_, remaining.patch(idx,Nil,1), Pr.shuffleArgs)) )
          })
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

        multiple(options)
      }

      // try to fill in the next argument
      add(f, pos+1, args_and_remaining flatMap nextarg)
    }

    orError(add(f, 0, single((Nil,args), Pr.dropArgs(math.max(0,args.size - f.params.size)))) map { x => ApplyExp(f, resolve(List(f), x map (_.ty)).get._2, x) },
            s"Can't match arguments for function $f.")

    /*
    // TODO: Allow inserting or dropping arguments
    product(args) flatMap { args => {
      val fn = f.params.size
      if (fn != args.size)
        fail(show(pretty(f))+s": expected $fn arguments (${show(CommaList(f.params))}), got ${args.size} ($args)")
      else {
        // If the original order of arguments fits, don't bother trying something else
        val tys = args map (_.ty)
        resolve(List(f),tys) match {
          case Some((f,ts)) => single(ApplyExp(f,ts,args),Pr.certain)
          case None => // If not, fiddle!
            val validPermutations = permute[Exp](args, xs => resolveOptions(List(f), xs.toList map (_.ty)).nonEmpty ).toList
            val scores = listScored(validPermutations flatMap { p => resolve(List(f), p.toList map (_.ty)) match {
              case None => Nil
              case Some((_,ts)) => List(Alt(Pr.certain, ApplyExp(f,ts,p.toList)))
            }}, show(f)+": params "+show(tokensSig(f))+" don't match arguments "+show(CommaList(args))+" with types "+show(CommaList(args map (_.ty))))
            scores flatMap (x => single(x, Pr.permuteArgs(f, args, x.args)))
        }
      }
    }}
    */

  }
}
