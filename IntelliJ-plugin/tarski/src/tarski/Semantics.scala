package tarski

import ambiguity.Utility._
import org.apache.commons.lang.StringEscapeUtils._
import tarski.AST._
import tarski.Base.VoidItem
import tarski.Denotations._
import tarski.Environment._
import tarski.Items._
import tarski.Operators._
import tarski.Pretty._
import tarski.Scores._
import tarski.JavaScores.pmul
import tarski.Tokens._
import tarski.Types._

import scala.annotation.tailrec

object Semantics {
  // Literals
  def denoteLit(x: ALit): Scored[Lit] = {
    def under(v: String): String = v.replaceAllLiterally("_","")
    def f[A,B](v: String, c: String => A)(t: (A,String) => B) = t(c(under(v)),v)
    x match {
      case IntALit(v) =>
        val n = under(v).toLong
        val i = n.toInt
        if (i == n) single(IntLit(i,v),Pr.intLit) else single(LongLit(n,v+'L'),Pr.longIntLit)
      case LongALit(v) =>   single(f(v,_.dropRight(1).toLong)(LongLit), Pr.longLit)
      case FloatALit(v) =>  single(f(v,_.toFloat)(FloatLit), Pr.floatLit)
      case DoubleALit(v) => single(f(v,_.toDouble)(DoubleLit), Pr.doubleLit)
      case CharALit(v) =>   single(CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0),v), Pr.charLit)
      case StringALit(v) => single(StringLit(unescapeJava(v.slice(1,v.size-1)),v), Pr.stringLit)
    }
  }

  // Check for a list of modifiers, and bail if we see any unwanted ones
  def modifiers(mods: List[Mod], want: List[Mod]): List[Boolean] = {
    val modSet = mods.toSet
    val bad = modSet -- want
    if (bad.nonEmpty) throw new RuntimeException("Unexpected modifiers "+bad.mkString(", "))
    want map modSet.contains
  }
  def modifiers(mods: List[Mod], a: Mod): Boolean = modifiers(mods,List(a)).head
  def modifiers(mods: List[Mod], a: Mod, b: Mod): (Boolean,Boolean) = modifiers(mods,List(a,b)) match {
    case List(a,b) => (a,b)
    case _ => impossible
  }
  def modifiers(mods: List[Mod], a: Mod, b: Mod, c: Mod): (Boolean,Boolean,Boolean) = modifiers(mods,List(a,b,c)) match {
    case List(a,b,c) => (a,b,c)
    case _ => impossible
  }

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
      case WildAExp(b) => b match {
        case None => known(Above(Nil,WildSub(ObjectType)))
        case Some((AST.Extends,t)) => denoteType(t) flatMap (_ flatMap (fix(_) map WildSub))
        case Some((AST.Super,  t)) => denoteType(t) flatMap (_ flatMap (fix(_) map WildSuper))
      }
      case ParenAExp(e,_) => denoteTypeArg(e)
      case _ => denoteType(e) flatMap (_ flatMap fix)
    }
  }

  // Prepare to check n type arguments, producing a function which consumes that many type arguments.
  def prepareTypeArgs(n: Int, f: Den)(implicit env: Env): Scored[Above[List[TypeArg]] => Scored[Den]] = {
    def absorb(vs: List[TypeVar], f: List[TypeArg] => TypeOrCallable)(above: Above[List[TypeArg]]): Scored[TypeOrCallable] =
      above match { case Above(ds,ts) =>
        if (couldMatch(vs,ts)) known(f(ts).discard(ds))
        else fail(s"Arguments ${ts map (a => showSep(a,"")) mkString ", "} don't fit type variables ${vs map details mkString ", "}")
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
          TypeApply(NewDen(p,f,Some(ts0)),ts1)
        })) else fail(s"${show(f)} expects $n0 or ${n0+n1} type arguments, got $n")
      case f:NotTypeApply =>
        val vs = f.tparams
        if (n == vs.size) known(absorb(vs,TypeApply(f,_)))
        else fail(s"${show(f)}: expects ${vs.size} type arguments, got $n")
      case TypeDen(ds,RawType(c,p)) =>
        val vs = c.tparams
        if (n == vs.size) known(absorb(vs,ts => TypeDen(ds,GenericType(c,ts,p))))
        else fail(s"${show(f)}: expects ${vs.size} type arguments, got $n")
      case TypeDen(_,t) => fail(s"${show(t)}: can't add type arguments to a non-raw type")
    }
  }
  def addTypeArgs(fs: Scored[Den], ts: KList[AExp])(implicit env: Env): Scored[Den] = ts match {
    case EmptyList => fs // Ignore empty type parameter lists
    case ts =>
      val n = ts.list.size
      product(fs flatMap (prepareTypeArgs(n,_)),product(ts.list map denoteTypeArg) map aboves) flatMap { case (f,ts) => f(ts) }
  }
  def addTypeArgs(fs: Scored[Den], ts: Option[KList[AExp]])(implicit env: Env): Scored[Den] = ts match {
    case None => fs
    case Some(ts) => addTypeArgs(fs,ts)
  }

  // Check whether a type is accessible in the environment (can be qualified by something in scope).
  // Pretty-printing will do the actual qualifying
  @tailrec
  def typeAccessible(t: TypeItem)(implicit env: Env): Boolean = env.inScope(t) || (t match {
    case t:ClassItem => t.parent match {
      case p:ClassType => typeAccessible(p.item)
      case p:PackageItem => true
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

  def valuesOfItem(c: TypeItem, i: Item, depth: Int)(implicit env: Env): Scored[Exp] =
    if (depth >= 3) fail("Automatic field depth exceeded")
    else objectsOfItem(c) flatMap { x =>
      if (containedIn(x,c)) fail(s"Field ${show(i)}: all objects of item ${show(c)} contained in ${show(c)}")
      else denoteValue(x,depth+1) // Increase depth to avoid infinite loops
    }

  def denoteField(i: FieldItem, depth: Int)(implicit env: Env): Scored[FieldExp] = {
    val c = i.parent
    val objs = valuesOfItem(c,i,depth)
    objs flatMap { xd => {
      if (shadowedInSubType(i,xd.item.asInstanceOf[ClassItem])) {
        xd match {
          case ThisExp(tt:ThisItem) if tt.self.base.item == c => fail("We'll use super instead of this")
          case _ => single(FieldExp(Some(CastExp(c.raw,xd)),i), Pr.shadowedFieldValue(objs, xd,c,i))
        }
      } else
        single(FieldExp(Some(xd),i), Pr.fieldValue(objs, xd, i))
    }}
  }

  def denoteMethod(i: MethodItem, depth: Int)(implicit env: Env): Scored[MethodDen] = {
    val objs = valuesOfItem(i.parent,i,depth)
    objs flatMap (xd => single(MethodDen(Some(xd),i), Pr.methodCallable(objs, xd, i)))
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
                          else denoteField(i,depth)
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

  @inline def denoteExp   (e: AExp)(implicit env: Env): Scored[Exp]       = denote(e,ExpMode).asInstanceOf[Scored[Exp]]
  @inline def denoteType  (e: AExp)(implicit env: Env): Scored[TypeDen]   = denote(e,TypeMode).asInstanceOf[Scored[TypeDen]]
  @inline def denoteParent(e: AExp)(implicit env: Env): Scored[ParentDen] = denote(e,ExpMode|TypeMode|PackMode).asInstanceOf[Scored[ParentDen]]
  @inline def denoteNew   (e: AExp)(implicit env: Env): Scored[Callable]  = denote(e,NewMode).asInstanceOf[Scored[Callable]]

  @inline def knownNotNew[A](m: Mode, x: A): Scored[A] = single(x,if (m.inNew) 1 else Pr.dropNew)
  @inline def biasedNotNew[A](m: Mode, x: => Scored[A]): Scored[A] = if (m.inNew) x else biased(Pr.dropNew,x)
  @inline def dropNew(m: Mode, p: Prob) = if (m.inNew) pmul(p,Pr.dropNew) else p

  // Turn f into f(), etc.
  // TODO: Make Pr.missingArgList much higher for explicit new
  def bareCall(f: Callable)(implicit env: Env): Scored[Exp] =
    biased(Pr.missingArgList,ArgMatching.fiddleCall(f,Nil))
  def fixCall(m: Mode, f: => Scored[Callable])(implicit env: Env): Scored[ExpOrCallable] =
    if (m.call) f
    else biased(Pr.missingArgList,f flatMap (ArgMatching.fiddleCall(_,Nil))) // Turn f into f(), etc.

  def denote(e: AExp, m: Mode)(implicit env: Env): Scored[Den] = e match {
    // Literals
    case x:ALit if m.exp => denoteLit(x)

    // Names
    case NameAExp(n) => env.flatMap(n,s"Name $n not found",{
      case v:Value if m.exp => denoteValue(v,depth=0)
      case t:TypeItem =>
        if (!typeAccessible(t)) fail(s"${show(t)} is inaccessible")
        else {
          val s = if (!m.callExp) fail("Not in call mode") else t match {
            case t:ClassItem if t.constructors.length==0 => fail(s"$t has no constructors")
            case t:ClassItem => fixCall(m,uniformGood(Pr.constructor,t.constructors) map (NewDen(None,_)))
            case _ => fail(s"$t is not a class, and therefore has no constructors")
          }
          if (m.ty) knownThen(TypeDen(Nil,t.raw),s) else s
        }
      case c:PseudoCallableItem if m.callExp => fixCall(m,c match {
        case i:MethodItem if i.isStatic => knownNotNew(m,MethodDen(None,i))
        case i:MethodItem if env.inScope(i) => knownNotNew(m,LocalMethodDen(i))
        case i:MethodItem => biasedNotNew(m,denoteMethod(i,0))
        case ThisItem(c) if env.place.forwardThisPossible(c) =>
          biasedNotNew(m,uniformGood(Pr.forwardThis,c.constructors) flatMap {
            case cons if cons == env.place.place => fail("Can't forward to current constructor")
            case cons => known(ForwardDen(Some(c.inside),cons))
          })
        case SuperItem(c) if env.place.forwardSuperPossible(c.item) => biasedNotNew(m,{
          val tenv = c.env
          uniformGood(Pr.forwardSuper,c.item.constructors) map (ForwardDen(Some(c),_))
        })
        case _ => fail(s"Unusable callable $c")
      })
      case p:PackageItem if m.pack => known(PackageDen(p))
      case i => fail(s"Name $n, item $i (${i.getClass}) doesn't match mode $m")
    })

    // Parentheses.  Java doesn't allow parentheses around types or callables, but we do.
    case ParenAExp(x,_) if m==ExpMode => denoteExp(x) map ParenExp
    case ParenAExp(x,_) if m.exp => denote(x,m) flatMap {
      case x:Exp => known(ParenExp(x))
      case x:TypeOrPackage => single(x,Pr.weirdParens)
      case x:Callable => if (m.call) single(x,Pr.weirdParens)
                         else bareCall(x) map ParenExp
    }
    case ParenAExp(x,_) => biased(Pr.weirdParens,denote(x,m))

    // Fields.  x is either a type or an expression, f is an inner type, method, field, or constructor.
    case FieldAExp(_,Some(_),_) if !m.callExp => fail(s"${show(e)}: Unexpected type arguments in mode $m")
    case FieldAExp(x,ts,f) =>
      // Is f a field of x?
      def memberIn(f: Member, x: ParentDen): Boolean = (x,f.parent) match {
        case (x:ExpOrType,p:ClassItem) => isSubitem(x.item,p)
        case (PackageDen(x),p) => x eq p
        case _ => false
      }
      def maybeMemberIn(f: Member): Boolean = f.parent.isInstanceOf[ClassItem]
      // TODO: Also try applying ts to the type
      // TODO: Convert ts expressions to type args and check whether the type arguments fit
      val mc = if (ts.isEmpty) m else m.onlyCallExp
      val fs = env.collect(f,s"$f doesn't look like a field",{
        case f:Value      with Member if mc.exp     && maybeMemberIn(f) => f
        case f:TypeItem   with Member                                   => f
        case f:MethodItem with Member if mc.callExp && maybeMemberIn(f) => f
      })
      val cs = product(denoteParent(x),fs) flatMap {case (x,f) => f match {
        case _ if !memberIn(f,x) => fail(s"${show(x)} does not contain $f")
        case f:Value => if (!mc.exp) fail(s"Value $f doesn't match mode $mc") else (x,f) match {
          case (x:PackageDen,_) => fail("Values aren't members of packages")
          case (x:Exp,    f:FieldItem) => single(FieldExp(Some(x),f),
                                                 if (f.isStatic) Pr.staticFieldExpWithObject else Pr.fieldExp)
          case (t:TypeDen,f:FieldItem) => if (f.isStatic) single(FieldExp(None,f).discard(t.discards),Pr.staticFieldExp)
                                          else fail(s"Can't access non-static field $f without object")
        }
        case f:TypeItem =>
          val types = if (!mc.ty || ts.nonEmpty) fail(s"${show(e)}: Unexpected or invalid type field") else x match {
            case _:PackageDen => known(TypeDen(Nil,f.raw))
            case TypeDen(ds,t) => known(TypeDen(ds,typeIn(f,t)))
            case x:Exp => single(TypeDen(effects(x),typeIn(f,x.ty)),Pr.typeFieldOfExp)
          }
          val cons = if (!mc.callExp) fail(s"${show(e)}: Not in call or exp mode") else f match {
            case f:ClassItem if f.constructors.length>0 =>
              val cons = uniformGood(Pr.constructor,f.constructors)
              fixCall(mc,x match {
                // TODO: Also try applying the type arguments to the class (not the constructor)
                // Only Classes have constructors, so t or x.ty below must be a ClassType
                case PackageDen(p) => cons map (NewDen(None,_))
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
        case f:MethodItem => fixCall(mc,x match {
          case x:Exp     if f.isStatic => single(MethodDen(Some(x),f),dropNew(mc,Pr.staticFieldCallableWithObject))
          case x:TypeDen if f.isStatic => knownNotNew(mc,MethodDen(None,f).discard(x.discards))
          case x:Exp     => knownNotNew(mc,MethodDen(Some(x),f))
          case x:TypeDen => fail(s"${show(e)}: Can't call non-static $f without object")
        })
        case _ => fail(s"Invalid field ${show(x)}  .  ${show(f)}")
      }}
      addTypeArgs(cs,ts)

    // Type application.  TODO: add around to TypeApplyAExp
    // For callables, this is C++-style application of type arguments to a generic method
    case TypeApplyAExp(x,ts) => {
      def n = ts.size
      if (n==0) denote(x,m)
      else addTypeArgs(denote(x,m.onlyTyCall),ts)
    }

    // Explicit new
    case NewAExp(ts,x) if m.callExp =>
      fixCall(m,biasedNotNew(m,addTypeArgs(denoteNew(x),ts).asInstanceOf[Scored[Callable]]))

    // Application
    case ApplyAExp(f,EmptyList,BrackAround) if m==TypeMode =>
      denoteType(f) map (_.array) // This case also shows up below
    case ApplyAExp(f,xsn,around) if m.exp =>
      val n = xsn.list.size
      val args = xsn.list map denoteExp
      val fs = denote(f,CallMode | (if (m.ty && around==BrackAround) TypeMode else NoMode)).asInstanceOf[Scored[TypeOrCallable]]
      // Either array index or call
      val call = biased(Pr.callExp(xsn,around), fs flatMap {
        case f:TypeDen => known(f.array) // t[]
        case f:Callable => ArgMatching.fiddleCall(f,args)
      })
      if (n == 0) call // No arguments is never array access
      else call ++ biased(Pr.indexCallExp(xsn,around),
        productWith(denoteExp(f).filter(f => hasDims(f.ty,n),show(e)+s": expected >= $n dimensions"),
                    product(args map (_ flatMap denoteIndex)))((a,is) => is.foldLeft(a)(IndexExp)))

    case UnaryAExp(op,x) if m.exp => denoteExp(x) flatMap {
      case x if unaryLegal(op,x.ty) => single(op match {
        case op:ImpOp => ImpExp(op,x)
        case op:NonImpOp => NonImpExp(op,x)
      }, Pr.unaryExp)
      case x => fail(s"${show(e)}: invalid unary ${show(token(op))} on type ${show(x.ty)}")
    }

    case BinaryAExp(op,x,y) if m.exp => product(denoteExp(x),denoteExp(y)) flatMap {case (x,y) => {
      val tx = x.ty
      val ty = y.ty
      if (binaryLegal(op,tx,ty)) single(BinaryExp(op,x,y), Pr.binaryExp)
      else fail("${show(e)}: invalid binary op ${show(tx)} ${show(op)} ${show(ty)}")
    }}

    case CastAExp(t,x) if m.exp => product(denoteType(t),denoteExp(x)) flatMap {case (TypeDen(ds,t),x) => {
      val tx = x.ty
      if (castsTo(tx,t)) single(CastExp(t,x).discard(ds),Pr.castExp)
      else fail("${show(e)}: can't cast ${show(tx)} to ${show(t)}")
    }}

    case CondAExp(c,x,y) if m.exp =>
      biased(Pr.condExp,product(denoteBool(c),denoteExp(x),denoteExp(y)) map {case (c,x,y) =>
        CondExp(c,x,y,condType(x.ty,y.ty))})

    case AssignAExp(op,x,y) if m.exp => {
      product(denoteVariable(x),denoteExp(y)) flatMap {case (x,y) => {
        val tx = x.ty
        val ty = y.ty
        assignOpType(op,tx,ty) match {
          case None => fail(s"${show(e)}: invalid assignop ${show(tx)} ${show(token(op))} ${show(ty)}")
          case Some(t) => single(AssignExp(op,x,y), Pr.assignExp)
        }
      }}
    }

    case ArrayAExp(xs,a) if m.exp =>
      biased(Pr.arrayExp,product(xs.list map denoteExp) map (is => ArrayExp(condTypes(is map (_.ty)),is)))

    case _ => fail(s"${show(e)}: doesn't match mode $m ($e)")
  }

  // Expressions with type restrictions
  def denoteBool(n: AExp)(implicit env: Env): Scored[Exp] = denoteExp(n) flatMap {e =>
    val t = e.ty
    if (t.unboxesToBoolean) known(e)
    else if (t.unboxesToNumeric) single(BinaryExp(NeOp, e, IntLit(0, "0")), Pr.insertComparison(t))
    // TODO: all sequences should probably check whether they're empty (or null)
    else if (t.isInstanceOf[RefType]) single(BinaryExp(NeOp, e, NullLit), Pr.insertComparison(t))
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
    case ApplyExp(_,_) => false
    case FieldExp(_,f) => !f.isFinal
    case IndexExp(_,_) => true // Java arrays are always mutable
    case CondExp(_,_,_,_) => false // TODO: java doesn't allow this, but (x==5?x:y)=10 should be turned into an if statement
    case ArrayExp(_,_) => false
    case EmptyArrayExp(_,_) => false
    case DiscardExp(_,e) => isVariable(e)
  }

  // Statements
  def denoteStmt(s: AStmt)(env: Env): Scored[(Env,List[Stmt])] = {
    implicit val imp = env
    s match {
      case EmptyAStmt => single((env,List(EmptyStmt)), Pr.emptyStmt)
      case HoleAStmt => single((env,List(HoleStmt)), Pr.holeStmt)
      case VarAStmt(m,t,ds) =>
        val isFinal = modifiers(m,Final)
        above(denoteType(t)(env) flatMap (at => {
          val t = at.beneath
          def init(t: Type, v: Name, i: Option[AExp], env: Env): Scored[Option[Exp]] = i match {
            case None => single(None, Pr.varInitNone)
            case Some(e) => denoteExp(e)(env) flatMap {e =>
              if (assignsTo(e,t)) single(Some(e), Pr.varInit)
              else fail(s"${show(s)}: can't assign ${show(e)} to type ${show(t)} in declaration of $v}")
            }
          }
          def define(env: Env, ds: List[AVarDecl]): Scored[(Env,List[VarDecl])] = ds match {
            case Nil => known((env,Nil))
            case (v,k,i)::ds =>
              val tk = arrays(t,k)
              product(env.newVariable(v,tk,isFinal),init(tk,v,i,env)) flatMap {case ((env,v),i) =>
                define(env,ds) flatMap {case (env,ds) => single((env,(v,k,i)::ds), Pr.varDecl)}}
          }
          t.safe match {
            case Some(st) => define(env,ds.list) flatMap {case (env,ds) => single((env,VarStmt(st,ds).discard(at.discards)),Pr.varStmt) }
            case None => fail(s"cannot make variables of type $t.")
          }
        }))
      case ExpAStmt(e) => {
        val exps = denoteExp(e) flatMap {
          case e:StmtExp => single((env,ExpStmt(e)),Pr.expStmt)
          case e => effects(e) match {
            case Nil => fail(s"${show(e)}: has no side effects")
            case ss => single((env,blocked(ss)),Pr.expStmtsSplit)
          }
        }
        val stmts: Scored[(Env,Stmt)] = e match {
          case AssignAExp(None,NameAExp(x),y) => denoteExp(y) flatMap {y => y.ty.safe match {
            case Some(t) => env.newVariable(x,t,false) flatMap { case (env,x) => single((env,VarStmt(t,List((x,0,Some(y))))), Pr.assignmentAsVarStmt) }
            case None => fail(s"expression $y does not return anything usable (${y.ty})")
          }}
          case _ => fail(s"${show(e)}: expression doesn't look like a statement ($e)")
        }
        above(exps++stmts)
      }
      case BlockAStmt(b) => denoteStmts(b)(env) flatMap {case (e,ss) => single((e,List(BlockStmt(ss))), Pr.blockStmt)}
      case AssertAStmt(c,m) => biased(Pr.assertStmt,above(productWith(denoteBool(c),thread(m)(denoteNonVoid)){case (c,m) =>
        (env,AssertStmt(c,m))}))

      case BreakAStmt(lab) =>
        if (env.place.breakable) biased(Pr.breakStmt,denoteLabel(lab,(env,List(BreakStmt))))
        else fail("cannot break outside of a loop or switch statement.")
      case ContinueAStmt(lab) =>
        if (env.place.continuable) biased(Pr.continueStmt,denoteLabel(lab,(env,List(ContinueStmt))))
        else fail("cannot break outside of a loop")
      case ReturnAStmt(e) => above(product(returnType,thread(e)(denoteExp)) flatMap {case (r,e) =>
        val t = typeOf(e)
        if (assignsTo(e,r)) single((env,ReturnStmt(e)), Pr.returnStmt)
        else fail(s"${show(s)}: type ${show(t)} incompatible with return type ${show(r)}")
      })
      case ThrowAStmt(e) => above(denoteExp(e) flatMap {e =>
        if (isThrowable(e.item)) single((env,ThrowStmt(e)), Pr.throwStmt)
        else fail(s"${show(s)}: type ${e.ty} is not throwable")
      })
      case SyncAStmt(e,b,_) => above(product(denoteRef(e),denoteScoped(b)(env)) flatMap {
        case (e,(env,b)) => single((env,SyncStmt(e,b)), Pr.syncStmt) })
      case IfAStmt(c,x,_) => above(product(denoteBool(c),denoteScoped(x)(env)) flatMap {
        case (c,(env,x)) => single((env,IfStmt(c,x)), Pr.ifStmt) })
      case IfElseAStmt(c,x,y,_) => above(product(denoteBool(c),denoteScoped(x)(env)) flatMap {case (c,(env,x)) =>
        denoteScoped(y)(env) flatMap {case (env,y) => single((env,IfElseStmt(c,x,y)), Pr.ifElseStmt) }})
      case WhileAStmt(c,s,flip,_) => above(product(denoteBool(c),denoteScoped(s)(env)) flatMap {case (c,(env,s)) =>
        single((env,WhileStmt(xor(flip,c),s)), Pr.whileStmt) })
      case DoAStmt(s,c,flip,_) => above(product(denoteScoped(s)(env),denoteBool(c)) flatMap {case ((env,s),c) =>
        single((env,DoStmt(s,xor(flip,c))), Pr.doStmt) })
      case ForAStmt(For(i,c,u),s,_) => {
        // Sanitize an initializer into valid Java
        def init(i: List[Stmt]): Scored[(Option[Exp],List[Exp],Stmt) => Stmt] = i match {
          case List(i:VarStmt) => single((c,u,s) => ForStmt(i,c,u,s), Pr.forStmt)
          case _ => allSome(i map {case ExpStmt(e) => Some(e); case _ => None}) match {
            case Some(es) => single((c,u,s) => ForStmt(ForExps(es),c,u,s), Pr.expForStmt)
            case None => single((c,u,s) => BlockStmt(i:::List(ForStmt(ForExps(Nil),c,u,s))), Pr.blockForStmt)
          }
        }
        denoteStmts(i)(env.pushScope) flatMap {case (env,i) => init(i) flatMap (i => {
          product(thread(c)(c => noAbove(denoteBool(c)(env))),
                  thread(u)(u => noAbove(denoteExp(u)(env))),
                  denoteScoped(s)(env))
            .map {case (c,u,(env,s)) => (env.popScope,List(i(c,u,s)))}
        })}
      }
      case ForAStmt(info@Foreach(m,t,v,n,e),s,a) => {
        val isFinal = modifiers(m,Final) || t.isEmpty
        def hole = show(ForAStmt(info,HoleAStmt,a))
        above(product(thread(t)(denoteType),denoteExp(e)) flatMap {case (at,e) =>
          val t = at map (_.beneath)
          val tc = e.ty
          isIterable(tc) match {
            case None => fail(s"${show(e)}: type ${show(tc)} is not Iterable or an Array")
            case Some(te) =>
              (t match {
                case Some(t) =>
                  val ta = arrays(t,n)
                  if (assignsTo(te,ta)) single(ta, Pr.forEachArray)
                  else fail(s"$hole: can't assign ${show(te)} to ${show(ta)}")
                case None =>
                  val ne = dimensions(te)
                  if (ne >= n) single(te, Pr.forEachArrayNoType)
                  else fail(s"$hole: expected $n array dimensions, got type ${show(te)} with $ne")
              }) flatMap (t => env.pushScope.newVariable(v,t,isFinal) flatMap {case (env,v) => denoteStmt(s)(env) map {case (env,s) =>
                (env.popScope,ForeachStmt(t,v,e,blocked(s)).discard(at.discards))
              }})
          }
        })
      }
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
    if (x) NonImpExp(NotOp,y) else y

  def denoteLabel[A](lab: Option[Name], x: => A): Scored[A] = lab match {
    case None => single(x, Pr.labelNone)
    case Some(_) => notImplemented
  }

  def denoteStmts(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    productFoldLeft(env)(s map denoteStmt) map {case (env,ss) => (env,ss.flatten)}

  // Statement whose environment is discarded
  def denoteScoped(s: AStmt)(env: Env): Scored[(Env,Stmt)] =
    denoteStmt(s)(env.pushScope) map {case (env,ss) => (env.popScope,blocked(ss))}
  def denoteScoped(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    denoteStmts(s)(env.pushScope) map {case (env,s) => (env.popScope,s)}
}
