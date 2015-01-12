package tarski

import utility.Utility._
import tarski.Pretty._
import tarski.Types._
import tarski.Tokens._
import tarski.Environment.Env
import scala.annotation.tailrec
import scala.language.implicitConversions

object Inference {
  // Bounds: 18.1.3.  Each of these refers to an ambient type variable s
  type Var = TypeVar
  type Bounds = Map[Var,Bound] // If false is allowed, use Option[Bounds]
  sealed abstract class Bound
  case class Fixed(t: RefType) extends Bound // s == t
  case class Bounded(lo: Set[RefType], hi: Set[RefType]) extends Bound // lo <: s <: hi
  case class Capture(vs: List[Var], t: GenericType) // TODO: captures
  // TODO: throws

  // Pretty printing support
  private implicit val emptyEnv = Env(Array(),Map.empty)
  implicit def prettyBounds(bs: Bounds): (Fixity,Tokens) = (HighestFix, IdentTok("Bounds") :: LParenTok ::
    separate(bs.toList map (tokens(_)),List(CommaTok)) ::: List(RParenTok))
  implicit def prettyBound(vb: (Var,Bound)): (Fixity,Tokens) = (HighestFix, vb match {
    case (v,Fixed(t)) => tokens(v) ::: EqTok :: tokens(t)
    case (v,Bounded(lo,hi)) =>
      val tlo = if (lo.isEmpty) Nil else separate(lo.toList map (tokens(_)),List(CommaTok)) ::: List(LeTok)
      val thi = if (hi.isEmpty) Nil else LeTok :: separate(hi.toList map (tokens(_)),List(CommaTok))
      tlo ::: tokens(v) ::: thi
    //case (v,Capture(vs,t)) => tokens(v) ::: EqTok :: IdentTok("capture(") :: separate(vs.map(tokens) ::: List(tokens(t)),List(CommaTok))
  })
  def shows[A](x: List[A])(implicit p: Pretty[A]) = x map (show(_)) mkString ", "

  // Thrown only when debugging is on
  class InferError(s: String) extends RuntimeException(s)

  // Debugging support
  val debug = false
  if (debug)
    println("DEBUGGING WARNING: Inference debugging enabled")
  private def log(s: => String): Unit =
    if (debug && true) println(s)
  private def fail(s: => String): Option[Bounds] = {
    if (debug) {
      if (true) println("fail: "+s)
      if (true) throw new InferError(s)
    }
    None
  }

  // Construct initial bounds for some type parameters
  // TODO: Handle bounds on type parameters
  def startBounds(vs: List[Var]): Bounds =
    vs.map(v => (v,Bounded(Set(),Set(ObjectType)))).toMap

  // TODO: Not sure what I'm actually supposed to do if occurs checks fail
  def occurs(s: Var, t: TypeArg): Boolean = t match {
    case t:ClassType => t.args exists (occurs(s,_))
    case ArrayType(t) => t match {
      case t:RefType => occurs(s,t)
      case _ => false
    }
    case IntersectType(ts) => ts exists (occurs(s,_))
    case t:TypeVar => s == t
    case t:Wildcard => occurs(s,t.t)
    case NullType => false
  }

  // Is an inference variable fixed yet?
  def isFixed(bs: Bounds, v: Var): Boolean = bs(v) match {
    case Fixed(_) => true
    case Bounded(_,_) => false
  }

  // All inference variables mentioned in a type
  def vars(bs: Bounds, t: TypeArg): Set[Var] = t match {
    case t:ClassType => t.args.toSet flatMap ((a: TypeArg) => vars(bs,a))
    case ArrayType(t) => t match {
      case t:RefType => vars(bs,t)
      case _ => Set()
    }
    case IntersectType(ts) => ts flatMap (vars(bs,_))
    case v:TypeVar => if (bs contains v) Set(v) else Set()
    case t:Wildcard => vars(bs,t.t)
    case NullType => Set()
  }

  // Incorporate bounds: 18.3
  def matchSupers(bs: Bounds, s: RefType, t: RefType): Option[Bounds] = {
    val sg = supers(s).toList collect {case g:GenericType => g}
    val tg = supers(t).toList collect {case g:GenericType => g}
    log(s"matchSupers: bs $bs, s $s, t $t, sg $sg, tg $tg")
    forms(bs,sg)((bs,sg) => forms(bs,tg)((bs,tg) =>
      if (sg.item == tg.item) forms(bs,sg.args,tg.args)(equalForm)
      else Some(bs)))
  }
  def incorporateSub(bs: Bounds, s: Var, t: RefType): Option[Bounds] = bs(s) match {
    case Fixed(st) => if (isSubtype(st,t)) Some(bs) else subForm(bs,st,t)
    case Bounded(lo,hi) =>
      if (occurs(s,t)) throw new NotImplementedError("not sure what to do about occurs checks")
      else if (hi contains t) Some(bs)
      else {
        log(s"incorporateSub: bs $bs, s $s, t $t")
        val bs2 = bs+((s,Bounded(lo,hi+t)))
        forms(bs2,lo.toList)(subForm(_,_,t))
          .flatMap(forms(_,hi.toList)(matchSupers(_,_,t)))
      }
  }
  def incorporateSub(bs: Bounds, s: RefType, t: Var): Option[Bounds] = bs(t) match {
    case Fixed(tt) => if (isSubtype(s,tt)) Some(bs) else subForm(bs,s,tt)
    case Bounded(lo,hi) =>
      if (occurs(t,s)) throw new NotImplementedError("not sure what to do about occurs checks")
      else if (lo contains s) Some(bs)
      else {
        val bs2 = bs+((t,Bounded(lo+s,hi)))
        forms(bs2,hi.toList)(subForm(_,s,_))
      }
  }
  def incorporateEqual(bs: Bounds, s: Var, t: RefType): Option[Bounds] = bs(s) match {
    case Fixed(u) => if (t == u) Some(bs) else equalForm(bs,t,u)
    case Bounded(lo,hi) =>
      if (occurs(s,t))
        throw new NotImplementedError("not sure what to do about occurs checks")
      else {
        implicit val env = Map(s -> Some(t))
        log(s"incorporateEqual: fixing ${show(s)} = ${show(t)}")
        val bs2 = bs+((s,Fixed(t)))
        def sub(bs: Bounds, ub: (Var,Bound)): Option[Bounds] = {
          val (u,b) = ub
          b match {
            case Fixed(v) => incorporateEqual(bs,u,v.substitute)
            case Bounded(lo,hi) => forms(bs,lo.toList)((bs,t) => incorporateSub(bs,t.substitute,u))
              .flatMap(forms(_ ,hi.toList)((bs,t) => incorporateSub(bs,u,t.substitute)))
          }
        }
        forms(bs2,bs2.toList)(sub)
          .flatMap(forms(_,lo.toList)(subForm(_,_,t)))
          .flatMap(forms(_,hi.toList)(subForm(_,t,_)))
      }
  }

  // Does t contain any inference variables?
  def isProper(bs: Bounds, t: RefType): Boolean = t match {
    case t:ClassType => t.args.forall(isProper(bs,_)) && isProper(bs,t.parent)
    case ArrayType(t) => isProper(bs,t)
    case IntersectType(ts) => ts forall (isProper(bs,_))
    case v:TypeVar => !bs.contains(v)
    case NullType => true
  }
  def isProper(bs: Bounds, t: Type): Boolean = t match {
    case t:RefType => isProper(bs,t)
    case _:LangType => true
  }
  def isProper(bs: Bounds, t: Parent): Boolean = t match {
    case _:SimpleParent => true
    case t:ClassType => t.args.forall(isProper(bs,_)) && isProper(bs,t.parent)
  }
  def isProper(bs: Bounds, t: TypeArg): Boolean = t match {
    case t:RefType => isProper(bs,t)
    case _:Wildcard => false
  }

  // Turn a compatibility constraint s -> t into bounds: 18.2.2
  def compatForm(bs: Bounds, s: Type, t: Type): Option[Bounds] = {
    log(s"compatForm: bs ${show(bs)}, s ${show(s)}, t ${show(t)}, proper(s) ${isProper(bs,s)}, proper(t) ${isProper(bs,t)}")
    if (isProper(bs,s) && isProper(bs,t))
      if (looseInvokeContext(s,t)) Some(bs) else fail(s"compatForm: proper ${show(s)} -> ${show(t)} invalid")
    else (s,t) match {
      case (VoidType,_)|(_,VoidType) => fail("compatForm: void invalid")
      case (s:PrimType,t) => compatForm(bs,s.box,t)
      case (s:RefType,t:PrimType) => equalForm(bs,s,t.box)
      // TODO: Handle raw type cases (bullets 4 and 5 in 18.2.2)
      case (s:RefType,t:RefType) => subForm(bs,s,t)
    }
  }

  // Turn a subtyping constraint s <: t into bounds: 18.2.3
  def subForm(bs: Bounds, s: RefType, t: RefType): Option[Bounds] = {
    log(s"subForm: bs ${show(bs)}, s ${show(s)}, t ${show(t)}")
    if (isProper(bs,s) && isProper(bs,t))
      if (isSubtype(s,t)) Some(bs) else fail(s"subForm: proper ${show(s)} !<: proper ${show(t)}")
    else (s,t) match {
      case (NullType,_) => Some(bs)
      case (_,NullType) => fail(s"subForm: ${show(s)} !<: nulltype")
      case (s:TypeVar,t) if bs contains s => incorporateSub(bs,s,t)
      case (s,t:TypeVar) if bs contains t => incorporateSub(bs,s,t)
      case (s,t:GenericType) => supers(s)
        .collect({case ss: GenericType if ss.parent == t.parent => ss})
        .headOption
        .flatMap(ss => forms(bs,ss.args,t.args)(containForm))
      case (s,_:ClassType) =>
        if (supers(s) contains t) Some(bs) else fail(s"subForm: supers(${show(s)} lacks ${show(t)}")
      case (s,ArrayType(t)) => s match {
        case ArrayType(s) => (s,t) match {
          case (s:RefType,t:RefType) => subForm(bs,s,t) // Java arrays are covariant, even though they shouldn't be
          case _ => if (s == t) Some(bs) else fail(s"subForm: different primitive array types ${show(s)} and ${show(t)}")
        }
        case _ => fail(s"subForm: ${show(s)} is not an array type, ArrayType(${show(t)} is")
      }
      case (s,_:TypeVar) => s match {
        case IntersectType(ss) if ss contains t => Some(bs)
        // TODO: Handle case where t has a lower bound
        case _ => fail(s"subForm: ${show(s)} does not contain ${show(t)}")
      }
      case (s,IntersectType(ts)) => forms(bs,ts.toList)(subForm(_,s,_))
    }
  }

  // Turn a containment constraint s <= t into bounds: 18.2.3 (second half)
  def containForm(bs: Bounds, s: TypeArg, t: TypeArg): Option[Bounds] = {
    log(s"containForm: bs ${show(bs)}, s ${show(s)}, t ${show(t)}")
    (s,t) match {
      case (s:RefType,   t:RefType) => equalForm(bs,s,t)
      case (s:Wildcard,  t:RefType) => fail(s"Types do not contain wildcards: ${show(s)} !<= ${show(t)}")
      case (s:RefType,   WildSub(t)) => subForm(bs,s,t)
      case (WildSub(s),  WildSub(t)) => subForm(bs,s,t)
      case (WildSuper(s),WildSub(t)) => equalForm(bs,ObjectType,t)
      case (s:RefType,   WildSuper(t)) => subForm(bs,t,s)
      case (WildSuper(s),WildSuper(t)) => subForm(bs,t,s)
      case (WildSub(s),  WildSuper(t)) => fail(s"Incompatible wildcards: ${show(s)} !<= ${show(t)}")
    }
  }

  def equalForm(bs: Bounds, s: RefType, t: RefType): Option[Bounds] = {
    log(s"equalForm: bs ${show(bs)}, s ${show(s)}, t ${show(t)}")
    if (isProper(bs,s) && isProper(bs,t))
      if (s == t) Some(bs) else fail(s"equalForm: proper ${show(s)} != ${show(t)}")
    else (s,t) match {
      case (s:TypeVar,t) if bs contains s => incorporateEqual(bs,s,t)
      case (s,t:TypeVar) if bs contains t => incorporateEqual(bs,t,s)
      case (s:GenericType,t:GenericType) =>
        if (s.parent == t.parent) forms(bs,s.args,t.args)(equalForm)
        else fail(s"equalForm: class ${show(s.parent)} != ${show(t.parent)}")
      case (ArrayType(s),ArrayType(t)) => equalForm(bs,s,t)
      case _ => fail(s"equalForm: skipping ${show(s)}, ${show(t)}") // IntersectType intentionally skipped as per spec
    }
  }
  def equalForm(bs: Bounds, s: Type, t: Type): Option[Bounds] = (s,t) match {
    case (s:RefType,t:RefType) => equalForm(bs,s,t)
    case (s,t) => if (s == t) Some(bs) else fail(s"equalForm: nonref ${show(s)} != ${show(t)}")
  }
  def equalForm(bs: Bounds, s: TypeArg, t: TypeArg): Option[Bounds] = (s,t) match {
    case (s:RefType,t:RefType) => equalForm(bs,s,t)
    case (_:Wildcard,_)|(_,_:Wildcard) => impossible // Wildcards should have been eliminated before inference
  }

  // Resolution: 18.4
  def resolve(bs: Bounds, vs: List[Var]): Option[Bounds] = {
    log(s"resolve: bs ${show(bs)}, vs ${shows(vs)}")
    // Determine dependencies, without ensuring reflexivity or transitivity
    val depends = bs mapValues {
      case Fixed(t) => vars(bs,t)
      case Bounded(lo,hi) => (lo++hi).toSet.flatMap((t: RefType) => vars(bs,t)) // TODO: Respect capture constraints
    }
    // Iteratively resolve
    def iterate(bs: Bounds, vs: List[Var]): Option[Bounds] = vs match {
      case Nil => Some(bs)
      case v::rest =>
        // Generate a candidate set
        def expand(vs: Set[Var], v: Var): Set[Var] =
          if (vs.contains(v) || isFixed(bs,v)) vs
          else depends(v).foldLeft(vs+v)(expand)
        val alpha = expand(Set(),v)
        // Add new bounds for each alpha.  We compute bounds via bs, not bs2; I believe this matches the spec.
        def freeze(bs2: Bounds, a: Var): Option[Bounds] = bs(a) match {
          case Fixed(_) => Some(bs2)
          case Bounded(lo,hi) => incorporateEqual(bs2, a, lo.toList filter (isProper(bs,_)) match {
            case lop@(_::_) => lub(lop)
            // TODO: Handle throws bounds
            case Nil => glb(hi.toList filter (isProper(bs,_)))
          })
        }
        forms(bs,alpha.toList)(freeze) flatMap (iterate(_,rest))
      }
    iterate(bs,vs)
  }

  // Extract proper types for inference variables
  // TODO: This probably needs to change for infinite types
  def extract(bs: Bounds, ps: List[Var]): List[TypeArg] = {
    def cleanV(t: TypeVar): TypeArg =
      if (!t.isFresh) { assert(!bs.contains(t)); t }
      else if (t.lo == NullType) WildSub(t.hi)
      else if (t.hi == ObjectType) WildSuper(t.lo)
      else WildSub()
    def cleanR(t: RefType): TypeArg = t match {
      case GenericType(c,ts,p) => GenericType(c,ts map cleanA,p)
      case t:TypeVar => cleanV(t)
      case IntersectType(ts) =>
        @tailrec def loop(prev: List[RefType], next: List[TypeArg], wild: Boolean): TypeArg = next match {
          case Nil =>
            val int = IntersectType(prev.toSet)
            if (wild) WildSub(int) else int
          case (t:RefType)::ts => loop(t::prev,ts,wild)
          case WildSub(t)::ts => loop(t::prev,ts,wild=true)
          case WildSuper(t)::_ => WildSub()
        }
        loop(Nil,ts.toList map cleanR,wild=false)
      case ArrayType(t) => t match {
        case t:RefType => cleanR(t) match {
          case t:RefType => ArrayType(t)
          case WildSub(t) => WildSub(ArrayType(t))
          case WildSuper(t) => WildSuper(ArrayType(t))
        }
        case t:LangType => ArrayType(t)
      }
      case NullType|ObjectType|_:RawType|_:SimpleType => t
    }
    def cleanA(t: TypeArg): TypeArg = t match {
      case t:TypeVar => cleanV(t)
      case t:RefType => cleanR(t)
      case t:Wildcard => impossible
    }
    ps.map(v => bs(v) match {
      case Fixed(t) => cleanR(t)
      case Bounded(_,_) => throw new RuntimeException("can't extract unresolved inference variable")
    })
  }

  // List combinators
  def forms[A](bs: Bounds, ts: List[A])(f: (Bounds,A) => Option[Bounds]): Option[Bounds] = ts match {
    case Nil => Some(bs)
    case t::ts => f(bs,t) flatMap (bs => forms(bs,ts)(f))
  }
  def forms[A,B](bs: Bounds, ss: List[A], ts: List[B])(f: (Bounds,A,B) => Option[Bounds]): Option[Bounds] = (ss,ts) match {
    case (Nil,Nil) => Some(bs)
    case (s::ss,t::ts) => f(bs,s,t) flatMap (bs => forms(bs,ss,ts)(f))
    case _ => throw new RuntimeException("arity mismatch")
  }

  // Can type s be used in a type t context (loose or strict)?
  // TODO: Replace s with an expression to handle poly expressions
  def strictBounds(bs: Bounds, s: Type, t: Type): Option[Bounds] = (s,t) match {
    case (VoidType,_)|(_,VoidType) => fail(s"strictBounds: void")
    case (_:PrimType,_:RefType)|(_:RefType|_:PrimType,_:PrimType) => fail(s"strictBounds: prim vs. ref: $s, $t")
    case (s:RefType,t:RefType) => subForm(bs,s,t)
  }
  def looseBounds(bs: Bounds, s: Type, t: Type): Option[Bounds] = compatForm(bs,s,t)

  // Given a function with type parameters ps and parameter types ts, called with argument types as,
  // is there a satisfying assignment?
  // TODO: Handle variable arity
  // TODO: We do not incorporate return type information as described in 18.5.2.
  type Form = (Bounds,Type,Type) => Option[Bounds]
  def infer(ps: List[Var], ts: List[Type], as: List[Type])(form: Form): Option[List[TypeArg]] = {
    log(s"infer:\n  ps ${shows(ps)}\n  ts ${shows(ts)}\n  as ${shows(as)}")
    // TODO: Freshen variables ps to handle recursion correctly
    log(s"start: ${show(startBounds(ps))}")
    forms(startBounds(ps),as map (_.captureAll),ts)(form)
      .flatMap(resolve(_,ps))
      .map(extract(_,ps))
  }
}
