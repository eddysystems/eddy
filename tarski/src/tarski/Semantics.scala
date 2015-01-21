package tarski

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
  private implicit val showFlags = abbrevShowFlags

  // Literals
  def denoteLit(x: ALit): Scored[Lit] = {
    def under(v: String): String = v.replaceAllLiterally("_","")
    def f[A,B](v: String, c: String => A)(t: (A,String) => B) = t(c(under(v)),v)
    x match {
      case IntALit(v,_) =>
        val n = under(v).toLong
        val i = n.toInt
        if (i == n) single(IntLit(i,v),Pr.intLit) else single(LongLit(n,v+'L'),Pr.longIntLit)
      case LongALit(v,_) =>   single(f(v,_.dropRight(1).toLong)(LongLit), Pr.longLit)
      case FloatALit(v,_) =>  single(f(v,_.toFloat)(FloatLit), Pr.floatLit)
      case DoubleALit(v,_) => single(f(v,_.toDouble)(DoubleLit), Pr.doubleLit)
      case CharALit(v,_) =>   single(CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0),v), Pr.charLit)
      case StringALit(v,_) => single(StringLit(denoteStringLit(v),v), Pr.stringLit)
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
      case WildAExp(b,_) => b match {
        case None => known(Above(Nil,WildSub(ObjectType)))
        case Some((AST.Extends,t)) => denoteType(t) flatMap (_ flatMap (fix(_) map WildSub))
        case Some((AST.Super,  t)) => denoteType(t) flatMap (_ flatMap (fix(_) map WildSuper))
      }
      case ParenAExp(e,_,_) => denoteTypeArg(e)
      case _ => denoteType(e) flatMap (_ flatMap fix)
    }
  }

  // Prepare to check n type arguments, producing a function which consumes that many type arguments.
  def prepareTypeArgs(n: Int, f: Den)(implicit env: Env): Scored[Above[List[TypeArg]] => Scored[Den]] = {
    def absorb(vs: List[TypeVar], f: List[TypeArg] => TypeOrCallable)(above: Above[List[TypeArg]]): Scored[TypeOrCallable] =
      above match { case Above(ds,ts) =>
        if (couldMatch(vs,ts)) known(f(ts).discard(ds))
        else fail(s"Arguments ${ts map (a => show(a)) mkString ", "} don't fit type variables ${vs map details mkString ", "}")
      }
    f match {
      case _ if n==0 => known((ts: Above[List[TypeArg]]) => known(f))
      case _:Exp|_:PackageDen => fail(s"${show(f)}: expressions and packages take no type arguments")
      case f:TypeApply => fail(s"${show(f)} expects no more type arguments, got $n")
      case NewDen(p,f,None) =>
        val v0 = f.parent.tparams
        val v1 = f.tparams
        val n0 = v0.size
        val n1 = v1.size
        if (n == n0) known(absorb(v0,ts => NewDen(p,f,Some(ts))))
        else if (n == n0+n1) known(absorb(v0++v1,ts => {
          val (ts0,ts1) = ts splitAt n0
          TypeApply(NewDen(p,f,Some(ts0)),ts1,hide=false)
        })) else fail(s"${show(f)} expects $n0 or ${n0+n1} type arguments, got $n")
      case f:NotTypeApply =>
        val vs = f.tparams
        if (n == vs.size) known(absorb(vs,TypeApply(f,_,hide=false)))
        else fail(s"${show(f)}: expects ${vs.size} type arguments, got $n")
      case TypeDen(ds,RawType(c,p)) =>
        val vs = c.tparams
        if (n == vs.size) known(absorb(vs,ts => TypeDen(ds,GenericType(c,ts,p))))
        else fail(s"${show(f)}: expects ${vs.size} type arguments, got $n")
      case TypeDen(_,t) => fail(s"${show(t)}: can't add type arguments to a non-raw type")
    }
  }
  def addTypeArgs(fs: Scored[Den], ts: KList[AExp], r: SRange)(implicit env: Env): Scored[Den] = ts match {
    case EmptyList => fs // Ignore empty type parameter lists
    case ts =>
      val n = ts.list.size
      val use = product(fs flatMap (prepareTypeArgs(n,_)),product(ts.list map denoteTypeArg) map aboves) flatMap { case (f,ts) => f(ts) }
      use ++ biased(Pr.ignoreTypeArgs(env.place.lastEditIn(r)),fs)
  }
  def addTypeArgs(fs: Scored[Den], ts: Option[Located[KList[AExp]]])(implicit env: Env): Scored[Den] = ts match {
    case None => fs
    case Some(Located(ts,r)) => addTypeArgs(fs,ts,r)
  }

  // Check whether a type is accessible in the environment (can be qualified by something in scope).
  // Pretty-printing will do the actual qualifying
  @tailrec
  def typeAccessible(t: TypeItem)(implicit env: Env): Boolean = env.inScope(t) || (t match {
    case t:ClassItem => t.parent match {
      case p:ClassType => typeAccessible(p.item)
      case p:Package => true
      case _ => false // We're not in scope, and we're a local class
    }
    case _ => false
  })

  // Are we contained in the given type, or in something contained in the given type?
  @tailrec
  def containedIn(i: Item, t: TypeItem): Boolean = i match {
    case f: Member => f.parent == t || containedIn(f.parent,t)
    case _ => false
  }

  def valuesOfItem(c: TypeItem, depth: Int, error: => String)(implicit env: Env): Scored[Exp] =
    if (depth >= 3) fail("Automatic field depth exceeded")
    else objectsOfItem(c) flatMap { x =>
      if (containedIn(x,c)) fail(s"$error: all objects of item ${show(c)} contained in ${show(c)}")
      else denoteValue(x,depth+1) // Increase depth to avoid infinite loops
    }

  def denoteFieldItem(i: FieldItem, depth: Int)(implicit env: Env): Scored[FieldExp] = {
    val c = i.parent
    val objs = valuesOfItem(c,depth,s"Field ${show(i)}")
    objs flatMap { xd => {
      if (shadowedInSubType(i,xd.item.asInstanceOf[ClassItem])) {
        xd match {
          case ThisExp(tt:ThisItem) if tt.item.base.item == c => fail("We'll use super instead of this")
          case _ => single(FieldExp(Some(CastExp(c.raw,xd)),i), Pr.shadowedFieldValue(objs, xd,c,i))
        }
      } else
        single(FieldExp(Some(xd),i), Pr.fieldValue(objs, xd, i))
    }}
  }

  def denoteMethod(i: MethodItem, depth: Int)(implicit env: Env): Scored[MethodDen] = {
    val objs = valuesOfItem(i.parent,depth,s"Method ${show(i)}")
    objs flatMap (xd => single(MethodDen(Some(xd),i), Pr.methodCallable(objs,xd,i)))
  }

  def denoteValue(i: Value, depth: Int)(implicit env: Env): Scored[Exp] = {
    @inline def penalize(e: Exp) = single(e,if (env.inScope(i)) Pr.inScope else Pr.outOfScope)
    i match {
      case i:Local => if (env.inScope(i)) known(LocalExp(i))
                      else fail(s"Local $i is shadowed")

      // We can always access this, static fields, or enums.
      // Pretty-printing takes care of finding a proper name, but we reduce score for out of scope items.
      case LitValue(x) => known(x)
      case i:FieldItem => if (env.inScope(i)) known(FieldExp(None,i))
                          else if (i.isStatic) single(FieldExp(None,i),
                            if (inClass(env.place.place,i.parent)) Pr.outOfScope
                            else if (pkg(env.place.place) == pkg(i.parent)) Pr.outOfScopeOtherClass
                            else Pr.outOfScopeOtherPackage)
                          else denoteFieldItem(i,depth)
      case i:ThisItem => penalize(ThisExp(i))
      case i:SuperItem => penalize(SuperExp(i))
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
    biased(Pr.missingArgList,ArgMatching.fiddleCall(f,Nil,expects,auto=true,ArgMatching.useAll))
  def fixCall(m: Mode, expects: Option[Type], f: => Scored[Den])(implicit env: Env): Scored[Den] =
    if (m.call) f
    else biased(Pr.missingArgList,f flatMap {
      case f:Callable => ArgMatching.fiddleCall(f,Nil,expects,auto=true,ArgMatching.useAll)
      case f => known(f)
    })

  def denote(e: AExp, m: Mode, expects: Option[Type] = None)(implicit env: Env): Scored[Den] = e match {
    case x:ALit if m.exp => denoteLit(x)
    case NameAExp(n,_) => denoteName(n,m,expects)

    // Fields
    case FieldAExp(x,None|Some(Located(EmptyList,_)),f,_) => denoteField(denoteParent(x),f,m,expects,e)
    case FieldAExp(x,Some(Located(ts,r)),f,_) =>
      if (!m.callExp) fail(s"${show(e)}: Unexpected type arguments in mode $m")
      else fixCall(m,expects,addTypeArgs(denoteField(denoteParent(x),f,m.onlyCall,None,e),ts,r))

    // Parentheses.  Java doesn't allow parentheses around types or callables, but we do.
    case ParenAExp(x,_,_) if m==ExpMode => denoteExp(x,expects) map ParenExp
    case ParenAExp(x,_,_) if m.exp => denote(x,m,expects) flatMap {
      case x:Exp => known(ParenExp(x))
      case x:TypeOrPackage => single(x,Pr.weirdParens)
      case x:Callable => if (m.call) single(x,Pr.weirdParens)
                         else bareCall(x,expects) map ParenExp
    }
    case ParenAExp(x,_,_) => biased(Pr.weirdParens,denote(x,m))

    // Type application.  TODO: add around to TypeApplyAExp
    // For callables, this is C++-style application of type arguments to a generic method
    case TypeApplyAExp(x,ts,tr,after,_) => {
      def n = ts.size
      if (n==0) denote(x,m,expects)
      else {
        val mx = m.onlyTyCall | (if (m.exp) CallMode else NoMode)
        val p = x match {
          case TypeApplyAExp(_,_,_,after2,_) if after && !after2 => Pr.badNestedTypeArgs
          case NewAExp(_,_,_) if after => Pr.badNewInsideTypeArgs
          case _ => Pr.reasonable
        }
        biased(p,fixCall(m,expects,addTypeArgs(denote(x,mx),ts,tr)))
      }
    }

    // Explicit new
    case NewAExp(ts,x,_) if m.callExp =>
      fixCall(m,expects,biasedNotNew(m,addTypeArgs(denoteNew(x),ts).asInstanceOf[Scored[Callable]]))

    // Application
    case ApplyAExp(f,EmptyList,BrackAround,_) if m==TypeMode =>
      denoteType(f) map (_.array) // This case also shows up below
    case ApplyAExp(f,xsn,around,_) if m.exp =>
      val n = xsn.list.size
      val args = xsn.list map (denoteExp(_))
      val fs = denote(f,CallMode | (if (m.ty && around==BrackAround) TypeMode else NoMode)
                                 | (if (n > 0) ExpMode else NoMode))
      // Either array index or call
      val call = biased(Pr.callExp(xsn,around), fs flatMap {
        case f:TypeDen => known(f.array) // t[]
        case f:Callable => ArgMatching.fiddleCall(f,args,expects,auto=false,ArgMatching.useAll)
        case _:Exp|_:PackageDen => fail("Expressions and packages are not callable")
      })
      if (n == 0) call // No arguments is never array access
      else {
        val ci = call ++ biased(Pr.indexCallExp(xsn,around),
          productWith(fs.collect({case f:Exp if hasDims(f.ty,n) => f},show(e)+s": expected >= $n dimensions"),
            product(args map (_ flatMap denoteIndex)))((a,is) => is.foldLeft(a)(IndexExp)))
        // Handle Javascript-style field access, Scala-style infix method calls, etc.
        def special(a: Scored[Den], x: Name, ys: List[Scored[Exp]], names: IdentityHashMap[Scored[Exp],Name]): Scored[Den] = {
          val ax = denoteField(a,x,m|CallMode,None,e)
          def apply: Scored[Den] = ax flatMap {
            case ax:Callable => ArgMatching.fiddleCall(ax,ys,expects,auto=false,(axy,zs) => zs match {
              case Nil => known(axy)
              case z::zs => names.get(z) match {
                case null => fail("Not a field name")
                case z => special(known(axy),z,zs,names)
              }
            })
            case _ => fail("Not applicable")
          }
          ys match {
            case Nil => fixCall(m,expects,ax)
            case y::ys => names.get(y) match {
              case null => fail("Not a field name")
              case y => special(ax,y,ys,names) ++ apply
            }
            case _ => apply
          }
        }
        def start(x: Name): Scored[Den] = ci ++ biased(Pr.specialCall,{
          val names = new IdentityHashMap[Scored[Exp],Name]
          (xsn.list.tail,args.tail).zipped foreach {
            case (NameAExp(x,_),s) => names.put(s,x)
            case _ => ()
          }
          special(fs,x,args.tail,names)
        })
        xsn.list.head match {
          case NameAExp(x,_) if n==1 || around==NoAround => start(x)
          case StringALit(v,_) if n==1 => start(denoteStringLit(v))
          case _ => ci
        }
      }

    case UnaryAExp(op,x,_) if m.exp => denoteExp(x) flatMap {
      case x if unaryLegal(op,x.ty) => single(op match {
        case op:ImpOp => ImpExp(op,x)
        case op:NonImpOp => NonImpExp(op,x)
      }, Pr.unaryExp)
      case x => fail(s"${show(e)}: invalid unary ${show(token(op))} on type ${show(x.ty)}")
    }

    case BinaryAExp(op,ax,ay,_) if m.exp => product(denoteExp(ax),denoteExp(ay)) flatMap {case (x,y) => {
      val tx = x.ty
      val ty = y.ty
      @tailrec def isZero(e: AExp): Boolean = e match {
        case IntALit("0",_) => true
        case ParenAExp(x,_,_) => isZero(x)
        case _ => false
      }
      def castZero(t: Type): Exp = t match {
        case BooleanType => BooleanLit(false)
        case _:RefType => NullLit
        case _:NumType => IntLit(0,"0")
        case VoidType => impossible
      }
      if (binaryLegal(op,tx,ty)) known(BinaryExp(op,x,y))
      else if (isZero(ax) && ty!=VoidType) single(BinaryExp(op,castZero(ty),y),Pr.binaryExpCastZero)
      else if (isZero(ay) && tx!=VoidType) single(BinaryExp(op,x,castZero(tx)),Pr.binaryExpCastZero)
      else fail(s"${show(e)}: invalid binary op ${show(tx)} ${show(op)} ${show(ty)}")
    }}

    case CastAExp(t,x,_) if m.exp => product(denoteType(t),denoteExp(x)) flatMap {case (TypeDen(ds,t),x) => {
      val tx = x.ty
      if (castsTo(tx,t)) single(CastExp(t,x).discard(ds),Pr.castExp)
      else fail(s"${show(e)}: can't cast ${show(tx)} to ${show(t)}")
    }}

    case CondAExp(c,x,y,_) if m.exp =>
      biased(Pr.condExp,product(denoteBool(c),denoteExp(x,expects),denoteExp(y,expects)) map {case (c,x,y) =>
        CondExp(c,x,y,condType(x.ty,y.ty))})

    case AssignAExp(None,x,y,_) if m.exp =>
      denoteVariable(x) flatMap (x => denoteAssignsTo(y,x.ty) map (AssignExp(None,x,_)))
    case AssignAExp(Some(op),x,y,_) if m.exp => {
      product(denoteVariable(x),denoteExp(y)) flatMap {case (x,y) => {
        assignOpType(op,x.ty,y.ty) match {
          case None => fail(s"${show(e)}: invalid assignop ${show(x.ty)} ${show(op)} ${show(y.ty)}")
          case Some(t) => known(AssignExp(Some(op),x,y))
        }
      }}
    }

    case ArrayAExp(xs,a,_) if m.exp =>
      biased(Pr.arrayExp,product(xs.list map (denoteExp(_))) map (is => ArrayExp(condTypes(is map (_.ty)),is)))

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

  def denoteName(n: Name, m: Mode, expects: Option[Type])(implicit env: Env): Scored[Den] = env.flatMap(n,s"Name $n not found",{
    case v:Value if m.exp => denoteValue(v,depth=0)
    case t:TypeItem =>
      if (!typeAccessible(t)) fail(s"${show(t)} is inaccessible")
      else {
        val s = if (!m.callExp) fail("Not in call mode") else t match {
          case t:ClassItem if t.constructors(env.place).length == 0 => fail(s"$t has no accessible constructors")
          case t:ClassItem => fixCall(m,expects,uniformGood(Pr.constructor,t.constructors(env.place)) map (NewDen(None,_)))
          case _ => fail(s"$t is not a class, and therefore has no constructors")
        }
        if (m.ty) knownThen(TypeDen(Nil,t.raw),s) else s
      }
    case c:PseudoCallableItem if m.callExp => fixCall(m,expects,c match {
      case i:MethodItem if i.isStatic => knownNotNew(m,MethodDen(None,i))
      case i:MethodItem if env.inScope(i) => knownNotNew(m,LocalMethodDen(i))
      case i:MethodItem => biasedNotNew(m,denoteMethod(i,0))
      case i:ThisItem if env.place.forwardThisPossible(i.item) =>
        biasedNotNew(m,uniformGood(Pr.forwardThis,i.item.constructors(env.place)) flatMap {
          case cons if cons == env.place.place => fail("Can't forward to current constructor")
          case cons => known(ForwardDen(i,cons))
        })
      case i:SuperItem if env.place.forwardSuperPossible(i.item) => biasedNotNew(m,{
        uniformGood(Pr.forwardSuper,i.item.constructors(env.place)) map (ForwardDen(i,_))
      })
      case _ => fail(s"Unusable callable $c")
    })
    case p:Package if m.pack => known(p)
    case i => fail(s"Name $n, item $i (${i.getClass}) doesn't match mode $m")
  })

  def denoteField(xs: Scored[Den], f: Name, mc: Mode, expects: Option[Type], error: AExp)(implicit env: Env): Scored[Den] = {
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
      case ApplyExp(_,_,auto) => auto
      case ParenExp(x) => automatic(x)
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
                                          else single(FieldExp(Some(x),f),
                                                      if (f.isStatic) Pr.staticFieldExpWithObject else Pr.fieldExp)
          case (t:TypeDen,f:FieldItem) => if (f.isStatic) known(FieldExp(None,f).discard(t.discards))
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
            case f:ClassItem if f.constructors(env.place).length>0 =>
              val cons = uniformGood(Pr.constructor,f.constructors(env.place))
              fixCall(mc,expects,x match {
                // TODO: Also try applying the type arguments to the class (not the constructor)
                // Only Classes have constructors, so t or x.ty below must be a ClassType
                case _:PackageDen => cons map (NewDen(None,_))
                case TypeDen(ds,tp) =>
                  val t = Some(tp.asInstanceOf[ClassType])
                  biased(Pr.constructorFieldCallable,cons map (NewDen(t,_).discard(ds)))
                case x:Exp =>
                  val t = Some(x.ty.asInstanceOf[ClassType])
                  val ds = effects(x)
                  biased(Pr.constructorFieldCallableWithObject,cons map (NewDen(t,_).discard(ds)))
              })
            case _ => fail(s"$f has no constructors")
          }
          types++cons
        case f:MethodItem => fixCall(mc,expects,x match {
          case x:Exp     if f.isStatic => if (automatic(x)) fail(s"${show(error)}: Implicit call . static method is silly")
                                          else single(MethodDen(Some(x),f),dropNew(mc,Pr.staticFieldCallableWithObject))
          case x:TypeDen if f.isStatic => knownNotNew(mc,MethodDen(None,f).discard(x.discards))
          case x:Exp     => knownNotNew(mc,MethodDen(Some(x),f))
          case x:TypeDen => fail(s"${show(error)}: Can't call non-static $f without object")
        })
        case _ => fail(s"Invalid field ${show(x)}  .  ${show(f)}")
      }
    }}
  }

  // Expressions with type restrictions
  private val zero = IntLit(0,"0")
  def denoteBool(n: AExp)(implicit env: Env): Scored[Exp] = denoteExp(n) flatMap {e =>
    val t = e.ty
    if (t.unboxesToBoolean) known(e)
    else if (t.unboxesToNumeric) single(BinaryExp(NeOp,e,zero),Pr.insertComparison(t))
    // TODO: all sequences should probably check whether they're empty (or null)
    else if (t.isInstanceOf[RefType]) single(BinaryExp(NeOp,e,NullLit), Pr.insertComparison(t))
    else fail(s"${show(n)}: can't convert type ${show(t)} to boolean")
  }
  def denoteIndex(e: Exp)(implicit env: Env): Scored[Exp] = {
    e.ty.unboxIntegral match {
      case Some(p) if promote(p) == IntType => known(e)
      case _ if castsTo(e.ty, IntType) => single(CastExp(IntType, e), Pr.insertedCastIndexExp)
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
    case _: Lit => false
    case ThisExp(_) => false
    case SuperExp(_) => false
    case LocalExp(i) => !i.isFinal
    case CastExp(_,_) => false // TODO: java doesn't allow this, but I don't see why we shouldn't
    case _:UnaryExp => false // TODO: java doesn't allow this, but we should. Easy for ++,--, and -x = 5 should translate to x = -5
    case BinaryExp(_,_,_) => false
    case AssignExp(_,_,_) => false
    case ParenExp(x) => isVariable(x)
    case ApplyExp(_,_,_) => false
    case FieldExp(_,f) => !f.isFinal
    case IndexExp(_,_) => true // Java arrays are always mutable
    case CondExp(_,_,_,_) => false // TODO: java doesn't allow this, but (x==5?x:y)=10 should be turned into an if statement
    case ArrayExp(_,_) => false
    case EmptyArrayExp(_,_) => false
    case DiscardExp(_,e) => isVariable(e)
  }

  // Guess the item name referred to by e.  Used only for approximate purposes.
  @tailrec def guessItem(e: AExp): Option[Name] = e match {
    case NameAExp(n,_) => Some(n)
    case ParenAExp(e,_,_) => guessItem(e)
    case FieldAExp(_,_,f,_) => Some(f)
    case TypeApplyAExp(e,_,_,_,_) => guessItem(e)
    case ApplyAExp(e,_,_,_) => guessItem(e)
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
  def denoteStmt(s: AStmt)(env: Env): Scored[(Env,List[Stmt])] = {
    implicit val imp = env
    s match {
      case EmptyAStmt => single((env,List(EmptyStmt)), Pr.emptyStmt)
      case HoleAStmt => single((env,List(HoleStmt)), Pr.holeStmt)
      case TokAStmt(t,_) => known(env,List(TokStmt(t)))
      case VarAStmt(m,t,ds,_) => modifiers(m,Final) flatMap (isFinal => {
        def process(d: AVarDecl)(env: Env, x: Local): Scored[VarDecl] = d match {
          case (_,k,None) => known(x,k,None)
          case (_,k,Some(i)) => denoteAssignsTo(i,x.ty) map (i => (x,k,Some(i)))
        }
        val useType = t match {
          case None => Empty
          case Some(t) =>
            env.newVariables(ds.list map (_._1),isFinal,ds.list map process) flatMap (f =>
              above(denoteType(t)(env) flatMap (at => safe(at.beneath)(t => {
                val (env,dss) = f(ds.list map {case (_,k,_) => arrays(t,k)})
                product(dss) map (ds => (env,VarStmt(t,ds,m).discard(at.discards)))
              }))))
        }
        ds.list match {
          case List((v,0,Some(e))) => // For T v = i, allow T to change
            val p = t match {
              case None => Pr.ignoreMissingType
              case Some(t) => Pr.ignoreVarType(env.place.lastEditIn(t.r))
            }
            useType ++ biased(p,{
              val goal = t flatMap guessItem
              product(env.newVariable(v,isFinal),denoteExp(e)(env)) flatMap {case (f,e) =>
                safe(similarBase(e.ty,goal))(t => {
                  val (env,x) = f(t)
                  known(env,List(VarStmt(t,List((x,0,Some(e))),m)))
                })
              }
            })
          case _ => useType
        }
      })
      case ExpAStmt(e) => {
        val exps = denoteExp(e) flatMap {
          case e:StmtExp => known(env,ExpStmt(e))
          case e => effects(e) match {
            case Nil => fail(s"${show(e)}: has no side effects")
            case ss => single((env,blocked(ss)),Pr.expStmtsSplit)
          }
        }
        above(e match {
          case AssignAExp(None,NameAExp(x,_),y,_) => exps ++ biased(Pr.assignmentAsVarStmt,
            product(env.newVariable(x,isFinal=false),denoteExp(y)) flatMap {case (f,y) => safe(y.ty)(t => {
              val (env,x) = f(t)
              known(env,VarStmt(t,List((x,0,Some(y)))))
            })})
          case _ => exps
        })
      }
      case BlockAStmt(b,_) => denoteStmts(b)(env) flatMap {case (e,ss) => single((e,List(BlockStmt(ss))), Pr.blockStmt)}
      case AssertAStmt(c,m,_) => biased(Pr.assertStmt,above(productWith(denoteBool(c),thread(m)(denoteNonVoid)){case (c,m) =>
        (env,AssertStmt(c,m))}))

      case BreakAStmt(l,_) =>
        if (!env.place.breakable) fail("Cannot break outside of a loop or switch statement.")
        else thread(l)(l => env.collect(l,s"Label $l not found",{
          case l:Label => l })) map (l => (env,List(BreakStmt(l))))
      case ContinueAStmt(l,_) =>
        if (!env.place.continuable) fail("Cannot continue outside of a loop")
        else thread(l)(l => env.collect(l,s"Continuable label $l not found",{
          case l:Label if l.continuable => l })) map (l => (env,List(ContinueStmt(l))))

      case ReturnAStmt(None,_) => returnType flatMap (r =>
        if (r==VoidType) known(env,List(ReturnStmt(None)))
        else valuesOfItem(r.item,0,"return") flatMap (x =>
          if (assignsTo(x,r)) known(env,List(ReturnStmt(Some(x))))
          else fail(s"${show(s)}: type ${show(x.ty)} incompatible with return type ${show(r)}")
        )
      )
      case ReturnAStmt(Some(e),_) => above(returnType flatMap (r => denoteAssignsTo(e,r) map (e => (env,ReturnStmt(Some(e))))))
      case ThrowAStmt(e,_) => above(denoteExp(e) flatMap {e =>
        if (isThrowable(e.item)) single((env,ThrowStmt(e)), Pr.throwStmt)
        else fail(s"${show(s)}: type ${e.ty} is not throwable")
      })
      case SyncAStmt(e,b,_,_) => above(product(denoteRef(e),denoteScoped(b)(env)) flatMap {
        case (e,(env,b)) => single((env,SyncStmt(e,b)), Pr.syncStmt) })
      case IfAStmt(c,x,_,_) => above(product(denoteBool(c),denoteScoped(x)(env)) flatMap {
        case (c,(env,x)) => single((env,IfStmt(c,x)), Pr.ifStmt) })
      case IfElseAStmt(c,x,y,_,_) => above(product(denoteBool(c),denoteScoped(x)(env)) flatMap {case (c,(env,x)) =>
        denoteScoped(y)(env) flatMap {case (env,y) => single((env,IfElseStmt(c,x,y)), Pr.ifElseStmt) }})
      case WhileAStmt(c,s,flip,_,_) => above(product(denoteBool(c),denoteScoped(s)(env)) flatMap {case (c,(env,s)) =>
        single((env,WhileStmt(xor(flip,c),s)), Pr.whileStmt) })
      case DoAStmt(s,c,flip,_,_) => above(product(denoteScoped(s)(env),denoteBool(c)) flatMap {case ((env,s),c) =>
        single((env,DoStmt(s,xor(flip,c))), Pr.doStmt) })
      case ForAStmt(For(i,c,u,_),s,_,_) => {
        // Sanitize an initializer into valid Java
        def init(i: List[Stmt]): Scored[(Option[Exp],List[Exp],Stmt) => Stmt] = i match {
          case List(i:VarStmt) => single((c,u,s) => ForStmt(i,c,u,s), Pr.forStmt)
          case _ => allSome(i map {case ExpStmt(e) => Some(e); case _ => None}) match {
            case Some(es) => single((c,u,s) => ForStmt(ForExps(es),c,u,s), Pr.expForStmt)
            case None => single((c,u,s) => BlockStmt(i:::List(ForStmt(ForExps(Nil),c,u,s))), Pr.blockForStmt)
          }
        }
        scoped(env)(env => denoteStmts(i)(env) flatMap {case (env,i) => init(i) flatMap (i => {
          product(thread(c)(c => noAbove(denoteBool(c)(env))),
                  thread(u)(u => noAbove(denoteExp(u)(env))),
                  denoteScoped(s)(env))
            .map {case (c,u,(env,s)) => (env,List(i(c,u,s)))}
        })})
      }
      case ForAStmt(info@Foreach(m,t,v,n,e,_),s,a,r) => modifiers(m,Final) flatMap (explicitFinal => {
        val isFinal = explicitFinal || t.isEmpty
        def hole = show(ForAStmt(info,HoleAStmt,a,r))
        above(product(env.newVariable(v,isFinal),thread(t)(denoteType),denoteExp(e)) flatMap {case (f,at,e) =>
          val t = at map (_.beneath)
          val tc = e.ty
          isIterable(tc) match {
            case None => fail(s"${show(e)}: type ${show(tc)} is not Iterable or an Array")
            case Some(te) =>
              def rest(t: Type): Scored[(Env,Stmt)] = {
                val (env,x) = f(t)
                denoteStmt(s)(env) map {case (env,s) => (env,ForeachStmt(t,x,e,blocked(s)).discard(at.discards))}
              }
              t match {
                case Some(t) =>
                  val ta = arrays(t,n)
                  if (assignsTo(te,ta)) rest(ta)
                  else fail(s"$hole: can't assign ${show(te)} to ${show(ta)}")
                case None =>
                  val ne = dimensions(te)
                  if (ne >= n) biased(Pr.forEachArrayNoType,rest(te))
                  else fail(s"$hole: expected $n array dimensions, got type ${show(te)} with $ne")
              }
          }
        })
      })
    }
  }

  def above(s: Scored[(Env,Stmt)]): Scored[(Env,List[Stmt])] = s map { case (env,s) =>
    s.discards match {
      case Nil => (env,List(s))
      case ds => (env,(s.strip::ds).reverse)
    }}
  def noAbove(s: Scored[Exp]): Scored[Exp] = s flatMap (a =>
    if (a.discards.nonEmpty) fail("No room for above statements in this context")
    else known(a))

  def xor(x: Boolean, y: Exp): Exp =
    if (x) y match {
      case BooleanLit(y) => BooleanLit(!y)
      case _ => NonImpExp(NotOp,y)
    } else y

  def denoteStmts(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    productFoldLeft(env)(s map denoteStmt) map {case (env,ss) => (env,ss.flatten)}

  // Statement whose environment is discarded
  def scoped[A](env: Env)(f: Env => Scored[(Env,A)]): Scored[(Env,A)] =
    f(env.pushScope) map {case (e,x) => (e.popScope,x)}
  def denoteScoped(s: AStmt)(env: Env): Scored[(Env,Stmt)] =
    scoped(env)(denoteStmt(s)(_)) map {case (e,ss) => (e,blocked(ss))}
  def denoteScoped(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    scoped(env)(denoteStmts(s)(_))
}
