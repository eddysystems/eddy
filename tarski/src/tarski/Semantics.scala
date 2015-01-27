package tarski

import tarski.Arounds._
import utility.Utility._
import utility.Locations._
import org.apache.commons.lang.StringEscapeUtils._
import tarski.AST._
import tarski.Mods._
import tarski.Base.VoidItem
import tarski.Denotations._
import tarski.Environment._
import tarski.Items._
import tarski.Operators._
import tarski.Pretty._
import tarski.Scores._
import tarski.JavaScores.{pp,pmul}
import tarski.Tokens._
import tarski.Types._
import java.util.IdentityHashMap
import scala.annotation.tailrec

object Semantics {
  // Pretty printing implicits
  private implicit val showFlags = abbrevShowFlags
  private implicit val showRange = SRange.unknown

  // Literals
  def denoteLit(x: ALit): Scored[Lit] = {
    val r = x.r
    def under(v: String): String = v.replaceAllLiterally("_","")
    def f[A,B](v: String, c: String => A)(t: (A,String,SRange) => B) = t(c(under(v)),v,r)
    x match {
      case IntALit(v,_) =>
        val n = under(v).toLong
        val i = n.toInt
        if (i == n) single(IntLit(i,v,r),Pr.intLit) else single(LongLit(n,v+'L',r),Pr.longIntLit)
      case LongALit(v,_) =>   single(f(v,_.dropRight(1).toLong)(LongLit), Pr.longLit)
      case FloatALit(v,_) =>  single(f(v,_.toFloat)(FloatLit), Pr.floatLit)
      case DoubleALit(v,_) => single(f(v,_.toDouble)(DoubleLit), Pr.doubleLit)
      case CharALit(v,_) =>   single(CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0),v,r), Pr.charLit)
      case StringALit(v,_) => single(StringLit(denoteStringLit(v),v,r), Pr.stringLit)
    }
  }
  def denoteStringLit(v: String): String = unescapeJava(v.slice(1,v.size-1))

  def denoteTypeArg(e: AExp)(implicit env: Env): Scored[Above[TypeArg]] = {
    def fix(t: Type): Scored[RefType] = t match {
      case t:RefType => known(t)
      case t:PrimType => single(t.box,Pr.boxType)
      case VoidType => fail("Can't use void as a type argument")
    }
    e match {
      // TODO: passing _, *, or a name should probably also work just as well as '?' (names at least for extends).
      // TODO: Fix List<int> into List<Integer>, but not List<void> into List<Void>.
      // The name would be made into a class definition like so:
      //   Collection<A extends Integer>
      // turns into
      //   [abstract] class A extends Integer {};
      //   Collection<A> ...
      // this does not work for ? super Integer.
      case WildAExp(_,b) => b match {
        case None => known(Above(Nil,WildSub(ObjectType)))
        case Some(WildBound(AST.Extends,_,t)) => denoteType(t) flatMap (_ flatMap (fix(_) map WildSub))
        case Some(WildBound(AST.Super,  _,t)) => denoteType(t) flatMap (_ flatMap (fix(_) map WildSuper))
      }
      case ParenAExp(e,_) => denoteTypeArg(e)
      case _ => denoteType(e) flatMap (_ flatMap fix)
    }
  }

  // Prepare to check n type arguments, producing a function which consumes that many type arguments.
  def prepareTypeArgs(n: Int, a: SGroup, f: Den)(implicit env: Env): Scored[Above[List[TypeArg]] => Scored[Den]] = {
    def absorb(vs: List[TypeVar], f: List[TypeArg] => TypeOrCallable)(above: Above[List[TypeArg]]): Scored[TypeOrCallable] =
      above match { case Above(ds,ts) =>
        if (couldMatch(vs,ts)) known(f(ts).discard(ds))
        else fail(s"Arguments ${ts map (a => show(a)) mkString ", "} don't fit type variables ${vs map details mkString ", "}")
      }
    f match {
      case _ if n==0 => known((ts: Above[List[TypeArg]]) => known(f))
      case _:Exp|_:PackageDen => fail(s"${show(f)}: expressions and packages take no type arguments")
      case f:TypeApply => fail(s"${show(f)} expects no more type arguments, got $n")
      case NewDen(nr,p,f,fr,None) =>
        val v0 = f.parent.tparams
        val v1 = f.tparams
        val n0 = v0.size
        val n1 = v1.size
        if (n == n0) known(absorb(v0,ts => NewDen(nr,p,f,fr,Some(Grouped(ts,a)))))
        else if (n == n0+n1) known(absorb(v0++v1,ts => {
          val (ts0,ts1) = ts splitAt n0
          TypeApply(NewDen(nr,p,f,fr,Some(Grouped(ts0,a))),ts1,a,hide=false)
        })) else fail(s"${show(f)} expects $n0 or ${n0+n1} type arguments, got $n")
      case f:NotTypeApply =>
        val vs = f.tparams
        if (n == vs.size) known(absorb(vs,TypeApply(f,_,a,hide=false)))
        else fail(s"${show(f)}: expects ${vs.size} type arguments, got $n")
      case TypeDen(ds,RawType(c,p)) =>
        val vs = c.tparams
        if (n == vs.size) known(absorb(vs,ts => TypeDen(ds,GenericType(c,ts,p))))
        else fail(s"${show(f)}: expects ${vs.size} type arguments, got $n")
      case TypeDen(_,t) => fail(s"${show(t)}: can't add type arguments to a non-raw type")
    }
  }
  def addTypeArgs(fs: Scored[Den], ts: KList[AExp], a: SGroup)(implicit env: Env): Scored[Den] = ts match {
    case EmptyList => fs // Ignore empty type parameter lists
    case ts =>
      val n = ts.list.size
      val use = product(fs flatMap (prepareTypeArgs(n,a,_)),product(ts.list map denoteTypeArg) map aboves) flatMap { case (f,ts) => f(ts) }
      use ++ biased(Pr.ignoreTypeArgs(env.place.lastEditIn(a.lr)),fs)
  }
  def addTypeArgs(fs: Scored[Den], ts: Option[Grouped[KList[AExp]]])(implicit env: Env): Scored[Den] = ts match {
    case None => fs
    case Some(Grouped(ts,a)) => addTypeArgs(fs,ts,a)
  }

  // Are we contained in the given type, or in something contained in the given type?
  @tailrec
  def containedIn(i: Item, t: TypeItem): Boolean = i match {
    case f: Member => f.parent == t || containedIn(f.parent,t)
    case _ => false
  }

  def valuesOfItem(c: TypeItem, cr: SRange, depth: Int, error: => String)(implicit env: Env): Scored[Exp] =
    if (depth >= 3) fail("Automatic field depth exceeded")
    else objectsOfItem(c) flatMap { x =>
      if (containedIn(x,c)) fail(s"$error: all objects of item ${show(c)} contained in ${show(c)}")
      else denoteValue(x,cr,depth+1) // Increase depth to avoid infinite loops
    }

  def denoteFieldItem(i: FieldItem, ir: SRange, depth: Int)(implicit env: Env): Scored[FieldExp] = {
    val c = i.parent
    val objs = valuesOfItem(c,ir,depth,s"Field ${show(i)}")
    objs flatMap { xd => {
      if (shadowedInSubType(i,xd.item.asInstanceOf[ClassItem])) {
        xd match {
          case ThisExp(tt:ThisItem,_) if tt.item.base.item == c => fail("We'll use super instead of this")
          case _ => single(FieldExp(Some(CastExp(c.raw,SGroup.approx(ir),xd)),i,ir), Pr.shadowedFieldValue(objs, xd,c,i))
        }
      } else
        single(FieldExp(Some(xd),i,ir), Pr.fieldValue(objs, xd, i))
    }}
  }

  def denoteMethod(i: MethodItem, ir: SRange, depth: Int)(implicit env: Env): Scored[MethodDen] = {
    val objs = valuesOfItem(i.parent,ir,depth,s"Method ${show(i)}")
    objs flatMap (xd => single(MethodDen(Some(xd),i,ir), Pr.methodCallable(objs,xd,i)))
  }

  def denoteValue(i: Value, ir: SRange, depth: Int)(implicit env: Env): Scored[Exp] = {
    @inline def penalize(e: Exp) = single(e,if (env.inScope(i)) Pr.inScope else Pr.outOfScope)
    i match {
      case i:Local => if (env.inScope(i)) known(LocalExp(i,ir))
                      else fail(s"Local $i is shadowed")

      // We can always access this, static fields, or enums.
      // Pretty-printing takes care of finding a proper name, but we reduce score for out of scope items.
      case LitValue(f) => known(f(ir))
      case i:FieldItem => if (env.inScope(i)) known(FieldExp(None,i,ir))
                          else if (i.isStatic) single(FieldExp(None,i,ir),
                            if (inClass(env.place.place,i.parent)) Pr.outOfScope
                            else if (pkg(env.place.place) == pkg(i.parent)) Pr.outOfScopeOtherClass
                            else Pr.outOfScopeOtherPackage)
                          else denoteFieldItem(i,ir,depth)
      case i:ThisItem => penalize(ThisExp(i,ir))
      case i:SuperItem => penalize(SuperExp(i,ir))
    }
  }

  case class Mode(m: Int) extends AnyVal {
    def exp:     Boolean = (m&1)!=0
    def ty:      Boolean = (m&2)!=0
    def call:    Boolean = (m&4)!=0
    def inNew:   Boolean = (m&8)!=0
    def pack:    Boolean = (m&16)!=0

    def callExp: Boolean = (m&(1|4))!=0
    def onlyCall:    Mode = Mode(m&(4|8))
    def onlyCallExp: Mode = Mode(m&(1|4|8))
    def onlyTyCall:  Mode = Mode(m&(2|4|8))

    def |(n: Mode):  Mode = Mode(m|n.m)

    override def toString = {
      def f(s: String, b: Boolean) = if (b) List(s) else Nil
      s"Mode(${f("exp",exp)++f("ty",ty)++f("call",call)++f("new",inNew) mkString "|"})"
    }
  }
  val NoMode   = Mode(0)
  val ExpMode  = Mode(1)
  val TypeMode = Mode(2)
  val CallMode = Mode(4)
  val NewMode  = Mode(4|8)
  val PackMode = Mode(16)

  @inline def denoteExp   (e: AExp, expects: Option[Type] = None)(implicit env: Env): Scored[Exp] = denote(e,ExpMode,expects).asInstanceOf[Scored[Exp]]
  @inline def denoteType  (e: AExp)(implicit env: Env): Scored[TypeDen]   = denote(e,TypeMode).asInstanceOf[Scored[TypeDen]]
  @inline def denoteParent(e: AExp)(implicit env: Env): Scored[ParentDen] = denote(e,ExpMode|TypeMode|PackMode).asInstanceOf[Scored[ParentDen]]
  @inline def denoteNew   (e: AExp)(implicit env: Env): Scored[Callable]  = denote(e,NewMode).asInstanceOf[Scored[Callable]]

  @inline def knownNotNew[A](m: Mode, x: A): Scored[A] = single(x,if (m.inNew) Pr.dropNew else Pr.notDropNew)
  @inline def biasedNotNew[A](m: Mode, x: => Scored[A]): Scored[A] = if (m.inNew) biased(Pr.dropNew,x) else x
  @inline def dropNew(m: Mode, p: Prob) = if (m.inNew) pmul(p,Pr.dropNew) else p

  // Turn f into f(), etc.
  // TODO: Make Pr.missingArgList much higher for explicit new
  def bareCall(f: Callable, expects: Option[Type])(implicit env: Env): Scored[Exp] =
    biased(Pr.missingArgList,ArgMatching.fiddleCall(f,Nil,SGroup.approx(f.r),expects,auto=true,ArgMatching.useAll))
  def fixCall(m: Mode, expects: Option[Type], f: => Scored[Den])(implicit env: Env): Scored[Den] =
    if (m.call) f
    else f flatMap {
      case f:Callable => biased(Pr.missingArgList, ArgMatching.fiddleCall(f,Nil,SGroup.approx(f.r.after),expects,auto=true,ArgMatching.useAll))
      case f => known(f)
    }

  def denote(e: AExp, m: Mode, expects: Option[Type] = None)(implicit env: Env): Scored[Den] = e match {
    case x:ALit if m.exp => denoteLit(x)
    case NameAExp(n,r) => denoteName(n,r,m,expects)

    // Fields
    case FieldAExp(x,_,None|Some(Grouped(EmptyList,_)),f,fr) => denoteField(denoteParent(x),x.r,f,fr,m,expects,e)
    case FieldAExp(x,_,Some(Grouped(ts,a)),f,fr) =>
      if (!m.callExp) fail(s"${show(e)}: Unexpected type arguments in mode $m")
      else fixCall(m,expects,addTypeArgs(denoteField(denoteParent(x),x.r,f,fr,m.onlyCall,None,e),ts,a))

    // Parentheses.  Java doesn't allow parentheses around types or callables, but we do.
    case ParenAExp(x,a) if m==ExpMode => denoteExp(x,expects) map (ParenExp(_,a.a))
    case ParenAExp(x,a) if m.exp => denote(x,m,expects) flatMap {
      case x:Exp => known(ParenExp(x,a.a))
      case x:TypeOrPackage => single(x,Pr.weirdParens)
      case x:Callable => if (m.call) single(x,Pr.weirdParens)
                         else bareCall(x,expects) map (ParenExp(_,a.a))
    }
    case ParenAExp(x,_) => biased(Pr.weirdParens,denote(x,m))

    // Type application.  TODO: add around to TypeApplyAExp
    // For callables, this is C++-style application of type arguments to a generic method
    case TypeApplyAExp(x,ts,tr,after) => {
      def n = ts.size
      if (n==0) denote(x,m,expects)
      else {
        val mx = m.onlyTyCall | (if (m.exp) CallMode else NoMode)
        val p = x match {
          case x:TypeApplyAExp if after && !x.after => Pr.badNestedTypeArgs
          case _:NewAExp if after => Pr.badNewInsideTypeArgs
          case _ => Pr.reasonable
        }
        biased(p,fixCall(m,expects,addTypeArgs(denote(x,mx),ts,tr)))
      }
    }

    // Explicit new
    case NewAExp(_,Some(_),_,_::_) => fail(s"${show(e)}: Array creation doesn't take type arguments")
    case NewAExp(nr,ts,x,Nil) if m.callExp =>
      fixCall(m,expects,biasedNotNew(m,addTypeArgs(denoteNew(x),ts).asInstanceOf[Scored[Callable]]))
    case NewAExp(nr,None,x,ns) if m.callExp =>
      // Split ns into [e] and [] parts
      val (is,ds) = takeCollect(ns){case Grouped(Some(i),a) => denoteExp(i) flatMap denoteIndex map (Grouped(_,a))}
      // The rest of ds should be expression free
      if (ds exists (_.x.nonEmpty)) fail(s"${show(e)}: In array creation, [size] should come before []")
      else {
        val as = ds map (_.a)
        fixCall(m,expects,product(denoteType(x),product(is)) map {case (at,is) =>
          NewArrayDen(nr,at.beneath,x.r,is,as)
        })
      }

    // Application
    case ApplyAExp(f,EmptyList,a) if a.isBracks && m==TypeMode =>
      denoteType(f) map (_.array) // This case also shows up below
    case ApplyAExp(f,xsn,around) if m.exp =>
      val n = xsn.list.size
      val args = xsn.list map (denoteExp(_))
      val fs = denote(f,CallMode | (if (m.ty && around.isBracks) TypeMode else NoMode)
                                 | (if (n > 0) ExpMode else NoMode))
      // Either array index or call
      val call = biased(Pr.callExp(xsn,around), fs flatMap {
        case f:TypeDen => known(f.array) // t[]
        case f:Callable =>
          def array(t: Type): Scored[ApplyExp] = {
            def error = s"Expected array element type $t"
            product(args map (_ filter (assignsTo(_,t),error))) map (ApplyExp(f,_,around.a,auto=false))
          }
          f match {
            case f:NewArrayDen => array(f.t)
            case DiscardCallableDen(_,f:NewArrayDen) => array(f.t)
            case _ => ArgMatching.fiddleCall(f,args,around.a,expects,auto=false,ArgMatching.useAll)
          }
        case f:Exp => fail(s"Expressions are not callable, f = $f")
        case f:PackageDen => fail(s"Packages are not callable, f = $f")
      })
      if (n == 0) call // No arguments is never array access
      else {
        val ci = call ++ biased(Pr.indexCallExp(xsn,around),
          productWith(fs.collect({case f:Exp if hasDims(f.ty,n) => f},show(e)+s": expected >= $n dimensions"),
            product(args map (_ flatMap denoteIndex)))((a,is) => is.foldLeft(a)(IndexExp(_,_,around.a))))
        // Handle Javascript-style field access, Scala-style infix method calls, etc.
        def special(a: Scored[Den], ar: SRange, x: Name, xr: SRange, ys: List[Scored[Exp]], names: IdentityHashMap[Scored[Exp],NameAExp]): Scored[Den] = {
          val ax = denoteField(a,ar,x,xr,m|CallMode,None,e)
          def apply: Scored[Den] = ax flatMap {
            case ax:Callable => ArgMatching.fiddleCall(ax,ys,around.a,expects,auto=false,(axy,zs) => zs match {
              case Nil => known(axy)
              case z::zs => names.get(z) match {
                case null => fail("Not a field name")
                case NameAExp(z,zr) => special(known(axy),axy.r,z,zr,zs,names)
              }
            })
            case _ => fail("Not applicable")
          }
          ys match {
            case Nil => fixCall(m,expects,ax)
            case y::ys => names.get(y) match {
              case null => fail("Not a field name")
              case NameAExp(y,yr) => special(ax,ar union xr,y,yr,ys,names) ++ apply
            }
            case _ => apply
          }
        }
        def start(x: Name, xr: SRange): Scored[Den] = ci ++ biased(Pr.specialCall,{
          val names = new IdentityHashMap[Scored[Exp],NameAExp]
          (xsn.list.tail,args.tail).zipped foreach {
            case (x:NameAExp,s) => names.put(s,x)
            case _ => ()
          }
          special(fs,f.r,x,xr,args.tail,names)
        })
        xsn.list.head match {
          case NameAExp(x,r) if n==1 || around.isNo => start(x,r)
          case StringALit(v,r) if n==1 => start(denoteStringLit(v),r)
          case _ => ci
        }
      }

    case UnaryAExp(op,opr,x) if m.exp => denoteExp(x) flatMap {
      case x if unaryLegal(op,x.ty) => single(op match {
        case op:ImpOp => ImpExp(op,opr,x)
        case op:NonImpOp => NonImpExp(op,opr,x)
      }, Pr.unaryExp)
      case x => fail(s"${show(e)}: invalid unary ${token(op).show} on type ${show(x.ty)}")
    }

    // TODO: handle final for classes, and implicitly final classes such as enums
    case InstanceofAExp(e,ir,t) if m.exp => product(denoteRef(e),denoteType(t)) flatMap { case (x,TypeDen(ds,y)) => {
      val den = InstanceofExp(x,ir,y,t.r).discard(ds)
      if (isSubtype(x.ty,y)) single(den, Pr.trueInstanceofExp) // x is subtype of y => always true
      else if (isProperSubtype(y,x.ty)) known(den) // y is subtype of x => proper test
      else (x.ty,y) match { // no relationship between x and y
        case (_:ClassType,_:ClassType) => single(den, Pr.falseInstanceofExp) // x and y both classes => always false
        case _ => known(den) // x or y is an interface => proper test
      }
    }}

    case BinaryAExp(op,opr,ax,ay) if m.exp => product(denoteExp(ax),denoteExp(ay)) flatMap {case (x,y) => {
      val tx = x.ty
      val ty = y.ty
      @tailrec def isZero(e: AExp): Boolean = e match {
        case IntALit("0",_) => true
        case ParenAExp(x,_) => isZero(x)
        case _ => false
      }
      def castZero(t: Type, r: SRange): Exp = t match {
        case BooleanType => BooleanLit(false,r)
        case _:RefType => NullLit(r)
        case _:NumType => IntLit(0,"0",r)
        case VoidType => impossible
      }
      if (binaryLegal(op,tx,ty)) known(BinaryExp(op,opr,x,y))
      else if (isZero(ax) && ty!=VoidType) single(BinaryExp(op,opr,castZero(ty,ay.r),y),Pr.binaryExpCastZero)
      else if (isZero(ay) && tx!=VoidType) single(BinaryExp(op,opr,x,castZero(tx,ax.r)),Pr.binaryExpCastZero)
      else fail(s"${show(e)}: invalid binary op ${show(tx)} ${show(Loc(op,opr))} ${show(ty)}")
    }}

    case CastAExp(t,a,x) if m.exp => product(denoteType(t),denoteExp(x)) flatMap {case (TypeDen(ds,t),x) => {
      val tx = x.ty
      if (castsTo(tx,t)) single(CastExp(t,a.a,x).discard(ds),Pr.castExp)
      else fail(s"${show(e)}: can't cast ${show(tx)} to ${show(t)}")
    }}

    case CondAExp(c,qr,x,cr,y) if m.exp =>
      biased(Pr.condExp,product(denoteBool(c),denoteExp(x,expects),denoteExp(y,expects)) map {case (c,x,y) =>
        CondExp(c,qr,x,cr,y,condType(x.ty,y.ty))})

    case AssignAExp(None,opr,x,y) if m.exp =>
      denoteVariable(x) flatMap (x => denoteAssignsTo(y,x.ty) map (AssignExp(None,opr,x,_)))
    case AssignAExp(Some(op),opr,x,y) if m.exp => {
      product(denoteVariable(x),denoteExp(y)) flatMap {case (x,y) => {
        assignOpType(op,x.ty,y.ty) match {
          case None => fail(s"${show(e)}: invalid assignop ${show(x.ty)} ${show(Loc(op,opr))} ${show(y.ty)}")
          case Some(t) => known(AssignExp(Some(op),opr,x,y))
        }
      }}
    }

    case ArrayAExp(xs,a) if m.exp =>
      biased(Pr.arrayExp,product(xs.list map (denoteExp(_))) map (is => ArrayExp(condTypes(is map (_.ty)),is,a.a)))

    case _ => fail(s"${show(e)}: doesn't match mode $m ($e)")
  }

  def denoteAssignsTo(e: AExp, to: Type)(implicit env: Env): Scored[Exp] =
    denoteExp(e,Some(to)) filter (assignsTo(_,to),s"Can't assign anything available to type ${show(to)}")

  /*
    // Optional check that e assigns to a type
  def expect(e: Exp, expects: Type): Scored[Exp] =
    if (!assignsTo(e,expects)) fail(s"Can't assign ${show(e)} to type ${show(expects)}")
    else known(e)
  def expect(e: Exp, expects: Option[Type]): Scored[Exp] = expects match {
    case Some(t) if !assignsTo(e,t) => fail(s"Can't assign ${show(e)} to type ${show(t)}")
    case _ => known(e)
  }
  def expect(e: Scored[Exp], expects: Option[Type]): Scored[Exp] = expects match {
    case Some(t) => e filter (assignsTo(_,t),s"Can't assign anything to type ${show(t)}")
    case None => e
  }
  */

  def denoteTypeItem(t: TypeItem, omittedNestedClass: Boolean = false)(implicit env: Env): Scored[TypeDen] = {
    // this function must only be called for TypeItems that are _.accessible!
    if (env.inScope(t))
      known(TypeDen(Nil,t.raw))
    else t match {
      case t:ClassItem => t.parent match {
        case p:ClassItem => biased(Pr.omitNestedClass(t,p,!omittedNestedClass), denoteTypeItem(p,omittedNestedClass=true))
        case p:Package => single(TypeDen(Nil,t.raw),Pr.omitPackage(t,p)) // need to import a package or qualify with a package name to avoid shadowing
        case _:CallableItem|_:UnknownContainerItemBase => impossible // t is accessible, so local classes are not ok.
      }
      case _:TypeVar => fail("out of scope typevar cannot be qualified to be in scope")
      case t:LangTypeItem => impossible // can't be out of scope and accessible if it's a builtin
      case ArrayItem => impossible // we come from the environment, so we're definitely not the special array base class
      case NoTypeItem => impossible // hopefully not.
      case _:RefTypeItem => impossible // RefTypeItem is only not sealed so TypeVar can inherit from it.
    }
  }

  def denoteName(n: Name, nr: SRange, m: Mode, expects: Option[Type])(implicit env: Env): Scored[Den] = env.flatMap(n,s"Name $n not found",{
    case v:Value if m.exp => denoteValue(v,nr,depth=0)
    case t:TypeItem =>
      // add probabilities for omitted qualifiers
      denoteTypeItem(t) flatMap { tden =>
        val s = if (!m.callExp) fail("Not in call mode") else tden.item match {
          case t:ClassItem if t.constructors(env.place).length == 0 => fail(s"$t has no accessible constructors")
          case t:ClassItem => fixCall(m,expects,uniformGood(Pr.constructor,t.constructors(env.place)) map (NewDen(nr.before,None,_,nr)))
          case t => fail(s"$t is not a class, and therefore has no constructors")
        }
        if (m.ty) knownThen(tden,s) else s
      }
    case c:PseudoCallableItem if m.callExp => fixCall(m,expects,c match {
      case i:MethodItem if i.isStatic => knownNotNew(m,MethodDen(None,i,nr))
      case i:MethodItem if env.inScope(i) => knownNotNew(m,LocalMethodDen(i,nr))
      case i:MethodItem => biasedNotNew(m,denoteMethod(i,nr,0))
      case i:ThisItem if env.place.forwardThisPossible(i.item) =>
        biasedNotNew(m,uniformGood(Pr.forwardThis,i.item.constructors(env.place)) flatMap {
          case cons if cons == env.place.place => fail("Can't forward to current constructor")
          case cons => known(ForwardDen(i,nr,cons))
        })
      case i:SuperItem if env.place.forwardSuperPossible(i.item) => biasedNotNew(m,{
        uniformGood(Pr.forwardSuper,i.item.constructors(env.place)) map (ForwardDen(i,nr,_))
      })
      case _ => fail(s"Unusable callable $c")
    })
    case p:Package if m.pack => known(p)
    case i => fail(s"Name $n, item $i (${i.getClass}) doesn't match mode $m")
  })

  def denoteField(xs: Scored[Den], xr: SRange, f: Name, fr: SRange, mc: Mode, expects: Option[Type], error: AExp)(implicit env: Env): Scored[Den] = {
    // Is f a field of x?
    def memberIn(f: Member, x: ParentDen): Boolean = (x,f.parent) match {
      case (x:ExpOrType,p:ClassItem) => isSubitem(x.item,p)
      case (x:Package,p) => x.p eq p
      case _ => false
    }
    def maybeMemberIn(f: Member): Boolean = f.parent.isInstanceOf[ClassItem]
    val fs = env.collect(f,s"$f doesn't look like a field (mode $mc)",{
      case f:Value with Member if mc.exp && maybeMemberIn(f) => f
      case f:TypeItem with Member => f
      case f:MethodItem if mc.callExp && maybeMemberIn(f) => f
      case f:MethodItem => throw new RuntimeException(s"f $f, mc $mc, maybe ${maybeMemberIn(f)}")
      case f:ChildPackage if mc.pack => f
    })
    @tailrec def automatic(e: Exp): Boolean = e match {
      case e:ApplyExp => e.auto
      case ParenExp(x,_) => automatic(x)
      case _ => false
    }
    product(xs,fs) flatMap {case (x,f) => x match {
      case _:Callable => fail(s"${show(x)}: Callables do not have fields (such as $f)")
      case x:ParentDen if !memberIn(f,x) => fail(x match {
        case x:ExpOrType => s"${show(x)}: Item ${show(x.item)} does not contain $f"
        case x:PackageDen => s"${show(x)}: Package does not contain $f"
      })
      case x:ParentDen => f match {
        case f:Value => if (!mc.exp) fail(s"Value $f doesn't match mode $mc") else (x,f) match {
          case (x:PackageDen,_) => fail("Values aren't members of packages")
          case (x:Exp,    f:FieldItem) => if (f.isStatic && automatic(x)) fail(s"${show(error)}: Implicit call . static field is silly")
                                          else single(FieldExp(Some(x),f,fr),
                                                      if (f.isStatic) Pr.staticFieldExpWithObject else Pr.fieldExp)
          case (t:TypeDen,f:FieldItem) => if (f.isStatic) known(FieldExp(None,f,fr).discard(t.discards))
                                          else fail(s"Can't access non-static field $f without object")
        }
        case f:TypeItem =>
          val types = if (!mc.ty) fail(s"${show(error)}: Unexpected or invalid type field") else x match {
            case _:PackageDen => known(TypeDen(Nil,f.raw))
            case TypeDen(ds,t) => known(TypeDen(ds,typeIn(f,t)))
            case x:Exp => if (automatic(x)) fail(s"${show(error)}: Implicit call . type is silly")
                          else single(TypeDen(effects(x),typeIn(f,x.ty)),Pr.typeFieldOfExp)
          }
          val cons = if (!mc.callExp) fail(s"${show(error)}: Not in call or exp mode") else f match {
            // TODO: if f is not static, it requires an object to qualify the new (which can be one of our ThisItems)
            case f:ClassItem if f.constructors(env.place).length>0 =>
              val cons = uniformGood(Pr.constructor,f.constructors(env.place))
              fixCall(mc,expects,x match {
                // TODO: Also try applying the type arguments to the class (not the constructor)
                // Only Classes have constructors, so t or x.ty below must be a ClassType
                case _:PackageDen => cons map (NewDen(xr.before,None,_,fr))
                case TypeDen(ds,tp) =>
                  val t = Some(tp.asInstanceOf[ClassType])
                  biased(Pr.constructorFieldCallable,cons map (NewDen(xr.before,t,_,fr).discard(ds)))
                case x:Exp =>
                  val t = Some(x.ty.asInstanceOf[ClassType])
                  val ds = effects(x)
                  biased(Pr.constructorFieldCallableWithObject,cons map (NewDen(xr.before,t,_,fr).discard(ds)))
              })
            case _ => fail(s"$f has no constructors")
          }
          types++cons
        case f:MethodItem => fixCall(mc,expects,x match {
          case x:Exp     if f.isStatic => if (automatic(x)) fail(s"${show(error)}: Implicit call . static method is silly")
                                          else single(MethodDen(Some(x),f,fr),dropNew(mc,Pr.staticFieldCallableWithObject))
          case x:TypeDen if f.isStatic => knownNotNew(mc,MethodDen(None,f,fr).discard(x.discards))
          case x:Exp     => knownNotNew(mc,MethodDen(Some(x),f,fr))
          case x:TypeDen => fail(s"${show(error)}: Can't call non-static $f without object")
        })
        case _ => fail(s"Invalid field ${show(x)}  .  ${show(f)}")
      }
    }}
  }

  // Expressions with type restrictions
  private def zero(r: SRange) = IntLit(0,"0",r)
  def denoteBool(n: AExp)(implicit env: Env): Scored[Exp] = { val nr = n.r; denoteExp(n) flatMap {e =>
    val t = e.ty
    if (t.unboxesToBoolean) known(e)
    else if (t.unboxesToNumeric) single(BinaryExp(NeOp,nr,e,zero(nr)),Pr.insertComparison(t))
    // TODO: all sequences should probably check whether they're empty (or null)
    else if (t.isInstanceOf[RefType]) single(BinaryExp(NeOp,nr,e,NullLit(nr)), Pr.insertComparison(t))
    else fail(s"${show(n)}: can't convert type ${show(t)} to boolean")
  }}
  def denoteIndex(e: Exp)(implicit env: Env): Scored[Exp] = {
    e.ty.unboxIntegral match {
      case Some(p) if promote(p) == IntType => known(e)
      case _ if castsTo(e.ty, IntType) => single(CastExp(IntType,SGroup.approx(e.r),e), Pr.insertedCastIndexExp)
      case _ => fail(s"Index ${show(e)} doesn't convert or cast to int")
    }
  }

  def denoteNonVoid(n: AExp)(implicit env: Env): Scored[Exp] = denoteExp(n) flatMap {e =>
    if (e.item != VoidItem) known(e)
    else fail(s"${show(n)}: expected non-void expression")
  }
  def denoteRef(e: AExp)(implicit env: Env): Scored[Exp] = denoteExp(e) flatMap {e =>
    if (e.item.isInstanceOf[RefTypeItem]) known(e)
    else fail(s"${show(e)} has non-reference type ${show(e.ty)}")
  }
  def denoteVariable(e: AExp)(implicit env: Env): Scored[Exp] = denoteExp(e) flatMap { x =>
    if (isVariable(x)) known(x)
    else fail(s"${show(e)}: ${show(x)} cannot be assigned to")
  }

  @tailrec
  def isVariable(e: Exp): Boolean = e match {
    // In Java, we can only assign to actual variables, never to values returned by functions or expressions.
    case _:Lit|_:ThisExp|_:SuperExp|_:BinaryExp|_:AssignExp|_:ApplyExp|_:ArrayExp|_:EmptyArrayExp|_:InstanceofExp => false
    case LocalExp(i,_) => !i.isFinal
    case _:CastExp => false // TODO: java doesn't allow this, but I don't see why we shouldn't
    case _:UnaryExp => false // TODO: java doesn't allow this, but we should. Easy for ++,--, and -x = 5 should translate to x = -5
    case ParenExp(x,_) => isVariable(x)
    case FieldExp(_,f,_) => !f.isFinal
    case _:IndexExp => true // Java arrays are always mutable
    case _:CondExp => false // TODO: java doesn't allow this, but (x==5?x:y)=10 should be turned into an if statement
    case DiscardExp(_,e) => isVariable(e)
  }

  // Guess the item name referred to by e.  Used only for approximate purposes.
  @tailrec def guessItem(e: AExp): Option[Name] = e match {
    case NameAExp(n,_) => Some(n)
    case ParenAExp(e,_) => guessItem(e)
    case FieldAExp(_,_,_,f,_) => Some(f)
    case TypeApplyAExp(e,_,_,_) => guessItem(e)
    case ApplyAExp(e,_,_) => guessItem(e)
    case _ => None
  }

  // Find a base type of t as similar to goal as possible.  For now, similar means _.item.name ~ goal.
  def similarBase(t: Type, goal: Option[Name]): Type = goal match {
    case None => t
    case Some(goal) => t match {
      case t:ClassType =>
        @tailrec def best(px: Double, x: ClassType, ys: List[RefType]): ClassType = ys match {
          case Nil => x
          case (y:ClassType)::ys =>
            val py = pp(Pr.typoProbability(y.item.name,goal))
            if (py > px) best(py,y,ys)
            else best(px,x,ys)
          case _::ys => best(px,x,ys)
        }
        best(0,t,supers(t).toList)
      case _ => t
    }
  }

  def safe[A](t: Type)(f: Type => Scored[A]): Scored[A] = t.safe match {
    case None => fail(s"Cannot make variables of type $t")
    case Some(t) => f(t)
  }

  // Statements
  def denoteStmt(s: AStmt)(env: Env): Scored[Stmt] = {
    implicit val imp = env
    s match {
      case SemiAStmt(x,sr) => denoteStmt(x)(env) map (addSemi(_,sr))
      case EmptyAStmt(r) => single(EmptyStmt(r,env),Pr.emptyStmt)
      case HoleAStmt(r) => single(HoleStmt(r,env),Pr.holeStmt)
      case TokAStmt(t,r) => known(TokStmt(t,r,env))
      case VarAStmt(m,t,ds) => modifiers(m,Final) flatMap (isFinal => {
        def process(d: AVarDecl)(env: Env, x: Local): Scored[VarDecl] = d match {
          case AVarDecl(_,xr,k,None) => known(VarDecl(x,xr,k,None,env))
          case AVarDecl(_,xr,k,Some((eq,i))) => denoteAssignsTo(i,x.ty)(env) map (i => VarDecl(x,xr,k,Some(eq,i),env))
        }
        val useType = t match {
          case None => Empty
          case Some(t) =>
            val tr = t.r
            env.newVariables(ds.list map (_.x),isFinal,ds.list map process) flatMap (f =>
              above(denoteType(t)(env) flatMap (at => safe(at.beneath)(t => {
                val (after,dss) = f(ds.list map {case AVarDecl(_,_,k,_) => arrays(t,k.size)})
                product(dss) map (ds => VarStmt(m,t,tr,ds,after).discard(at.discards))
              }))))
        }
        ds.list match {
          case List(AVarDecl(v,vr,Nil,Some((eq,e)))) => // For T v = i, allow T to change
            val (p,tr) = t match {
              case None => (Pr.ignoreMissingType,vr)
              case Some(t) => val tr = t.r
                              (Pr.ignoreVarType(env.place.lastEditIn(tr)),tr)
            }
            useType ++ biased(p,{
              val goal = t flatMap guessItem
              product(env.newVariable(v,isFinal),denoteExp(e)(env)) flatMap {case (f,e) =>
                safe(similarBase(e.ty,goal))(t => {
                  val (after,x) = f(t)
                  known(VarStmt(m,t,tr,List(VarDecl(x,vr,Nil,Some(eq,e),env)),after))
                })
              }
            })
          case _ => useType
        }
      })
      case ExpAStmt(e) => {
        val er = e.r
        val exps = denoteExp(e) flatMap {
          case e:StmtExp => known(ExpStmt(e,env))
          case e => effects(e) match {
            case Nil => fail(s"${show(e)}: has no side effects")
            case ss => single(multiple(ss),Pr.expStmtsSplit)
          }
        }
        above(e match {
          case AssignAExp(None,opr,NameAExp(x,xr),y) => exps ++ biased(Pr.assignmentAsVarStmt,
            product(env.newVariable(x,isFinal=false),denoteExp(y)) flatMap {case (f,y) => safe(y.ty)(t => {
              val (after,x) = f(t)
              known(VarStmt(Nil,t,xr.before,List(VarDecl(x,xr,Nil,Some(opr,y),env)),after))
            })})
          case _ => exps
        })
      }
      case BlockAStmt(Nil,a) => known(BlockStmt(Nil,a,env))
      case BlockAStmt(b,a) => denoteStmts(b)(env) flatMap (ss => single(BlockStmt(ss,a,ss.head.env),Pr.blockStmt))
      case AssertAStmt(ar,c,m) =>
        def sm: Scored[Option[(SRange,Exp)]] = m match {
          case None => known(None)
          case Some((cr,m)) => denoteNonVoid(m) map (Some(cr,_))
        }
        biased(Pr.assertStmt,above(productWith(denoteBool(c),sm){case (c,m) =>
          AssertStmt(ar,c,m,env)}))

      case BreakAStmt(br,l) =>
        if (!env.place.breakable) fail("Cannot break outside of a loop or switch statement.")
        else thread(l){case Loc(l,lr) => env.collect(l,s"Label $l not found",{
          case l:Label => Loc(l,lr) })} map (BreakStmt(br,_,env))
      case ContinueAStmt(cr,l) =>
        if (!env.place.continuable) fail("Cannot continue outside of a loop")
        else thread(l){case Loc(l,lr) => env.collect(l,s"Continuable label $l not found",{
          case l:Label if l.continuable => Loc(l,lr) })} map (ContinueStmt(cr,_,env))

      case ReturnAStmt(rr,None) => returnType flatMap (r =>
        if (r==VoidType) known(ReturnStmt(rr,None,env))
        else valuesOfItem(r.item,rr,0,"return") flatMap (x =>
          if (assignsTo(x,r)) known(ReturnStmt(rr,Some(x),env))
          else fail(s"${show(s)}: type ${show(x.ty)} incompatible with return type ${show(r)}")
        )
      )
      case ReturnAStmt(rr,Some(e)) => above(returnType flatMap (r => denoteAssignsTo(e,r) map (e => ReturnStmt(rr,Some(e),env))))
      case ThrowAStmt(tr,e) => above(denoteExp(e) flatMap {e =>
        if (isThrowable(e.item)) single(ThrowStmt(tr,e,env), Pr.throwStmt)
        else fail(s"${show(s)}: type ${e.ty} is not throwable")
      })
      case SyncAStmt(sr,e,a,b) => above(product(denoteRef(e),denoteScoped(b)(env)) flatMap {
        case (e,b) => single(SyncStmt(sr,e,a.a,needBlock(b)),Pr.syncStmt) })
      case IfAStmt(ir,c,a,x) => above(product(denoteBool(c),denoteScoped(x)(env)) flatMap {
        case (c,x) => single(IfStmt(ir,c,a.a,x),Pr.ifStmt) })
      case IfElseAStmt(ir,c,a,x,er,y) => above(product(denoteBool(c),denoteScoped(x)(env),denoteScoped(y)(env)) flatMap {
        case (c,x,y) => single(IfElseStmt(ir,c,a.a,x,er,y),Pr.ifElseStmt) })
      case WhileAStmt(wr,flip,c,a,s) => above(product(denoteBool(c),denoteScoped(s)(env)) flatMap {case (c,s) =>
        single(WhileStmt(wr,xor(flip,c),a.a,s),Pr.whileStmt) })
      case DoAStmt(dr,s,wr,flip,c,a) => above(product(denoteScoped(s)(env),denoteBool(c)) flatMap {case (s,c) =>
        single(DoStmt(dr,s,wr,xor(flip,c),a.a),Pr.doStmt) })
      case f@ForAStmt(fr,For(i,sr0,c,sr1,u),a,s) => {
        // Sanitize an initializer into valid Java
        def init(i: List[Stmt]): Scored[(Option[Exp],List[Exp],Stmt) => Stmt] = i match {
          case List(i:VarStmt) => single((c,u,s) => ForStmt(fr,i,c,sr1,u,a.a,s), Pr.forStmt)
          case _ => allSome(i map {case ExpStmt(e,_) => Some(e); case _ => None}) match {
            case Some(es) => single((c,u,s) => ForStmt(fr,ForExps(es,sr0,env),c,sr1,u,a.a,s), Pr.expForStmt)
            case None => single((c,u,s) => BlockStmt(i:::List(ForStmt(fr,ForExps(Nil,sr0,env),c,sr1,u,a.a,s)),SGroup.approx(f.r),env), Pr.blockForStmt)
          }
        }
        val push = env.pushScope
        denoteStmts(i.list)(push) flatMap (i => init(i) flatMap (f => {
          val env = i match { case Nil => push; case _ => i.last.envAfter }
          init(i) flatMap (i =>
            product(thread(c)(c => noAbove(denoteBool(c)(env))),
                    thread(u.list)(u => noAbove(denoteExp(u)(env))),
                    denoteScoped(s)(env))
              .map {case (c,u,s) => i(c,u,s)})
        }))
      }
      case ForAStmt(fr,info@Foreach(m,t,v,vr,n,cr,e),a,s) => modifiers(m,Final) flatMap (explicitFinal => {
        val isFinal = explicitFinal || t.isEmpty
        def hole = show(ForAStmt(fr,info,a,HoleAStmt(s.r)))
        val tr = t match { case None => vr.before; case Some(t) => t.r }
        val sr = s.r
        val nn = n.size
        above(product(env.newVariable(v,isFinal),thread(t)(denoteType),denoteExp(e)) flatMap {case (f,at,e) =>
          val t = at map (_.beneath)
          val tc = e.ty
          isIterable(tc) match {
            case None => fail(s"${show(e)}: type ${show(tc)} is not Iterable or an Array")
            case Some(te) =>
              def rest(t: Type): Scored[Stmt] = {
                val (after,x) = f(t)
                denoteStmt(s)(after.pushScope) map (ForeachStmt(fr,m,t,tr,x,vr,e,a.a,_,env).discard(at.discards))
              }
              t match {
                case Some(t) =>
                  val ta = arrays(t,nn)
                  if (assignsTo(te,ta)) rest(ta)
                  else fail(s"$hole: can't assign ${show(te)} to ${show(ta)}")
                case None =>
                  val ne = dimensions(te)
                  if (ne >= nn) biased(Pr.forEachArrayNoType,rest(te))
                  else fail(s"$hole: expected $n array dimensions, got type ${show(te)} with $ne")
              }
          }
        })
      })
      case TryAStmt(tr,ts,cs,f) =>
        // CatchBlock(m: Mods, tr: SRange, v: Local, vr: SRange, s: Stmt)
        def catchBlocks(cs: List[(CatchInfo,AStmt)]): Scored[List[CatchBlock]] =
          product(cs map {
            case (CatchInfo(cr,mods,typ,id,around,colon),s) => biased(Pr.catchAround(around) * Pr.catchColon(colon), {
              val t: Scored[TypeDen] = if (typ.isDefined)
                denoteType(typ.get).filter({ case TypeDen(Nil,t) => isThrowable(t.item); case _ => false }, "must be Throwable without side-effects")
              else
                listGood(List(Alt(Pr.ellipsisCatchException,TypeDen(Nil,Base.ExceptionType)),
                              Alt(Pr.ellipsisCatchThrowable,TypeDen(Nil,Base.ThrowableType))))
              val tr = if (typ.isDefined) typ.get.r else around.a.l.after
              val name: String = if (id.isDefined) id.get.x else "$$$eddy_ignored_exception$$$"
              val idr = if (id.isDefined) id.get.r else around.a.r.before
              val env_gens = env.newVariable(name,mods.map(_.x).contains(Mods.Final))
              // for each type, make the inner statement
              product(env_gens,t) flatMap { case (env_gen,tden) =>
                val (local_env,v) = env_gen(tden.beneath)
                denoteStmt(s)(local_env) map (s => CatchBlock(mods, tr, v, idr, needBlock(s)))
              }
          })})

        def sden: Scored[Stmt] = denoteStmt(ts)(imp)
        def csden: Scored[List[CatchBlock]] = catchBlocks(cs)
        def fden: Scored[Option[(SRange,Stmt)]] = product(f map { case (r,fs) => denoteStmt(fs)(imp) map (s => (r,needBlock(s))) })
        product(sden,csden,fden) map { case (s,cbs,fo) => TryStmt(tr,needBlock(s),cbs,fo) }
    }
  }

  def above(s: Scored[Stmt]): Scored[Stmt] = s map (s => s.discards match {
    case Nil => s
    case ds => multiple((s.strip::ds).reverse)
  })
  def noAbove(s: Scored[Exp]): Scored[Exp] = s flatMap (a =>
    if (a.discards.nonEmpty) fail("No room for above statements in this context")
    else known(a))

  def denoteStmts(ss: List[AStmt])(env: Env): Scored[List[Stmt]] = ss match {
    case Nil => known(Nil)
    case List(s) => denoteStmt(s)(env) map (_.flatten)
    case s::ss => denoteStmt(s)(env) flatMap (s => {
      val sf = s.flatten
      denoteStmts(ss)(s.envAfter) map (sf:::_)
    })
  }

  // Statement whose environment is discarded
  def denoteScoped(s: AStmt)(env: Env): Scored[Stmt] =
    denoteStmt(s)(env.pushScope) map blocked
}
