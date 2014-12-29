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

  def collectDiscardsA[A,B](xs: List[Above[A]])(f: List[A] => Scored[B]): Scored[Above[B]] =
    aboves(xs) mapA f
  def collectDiscardsB[A,B<:HasDiscard[B],C](xs: List[Above[A]])(f: List[A] => Scored[(B,C)]): Scored[(B,C)] =
    aboves(xs) match {
      case Above(Nil,xs) => f(xs)
      case Above(ds,xs) => f(xs) map { case (b,c) => (b.discard(ds),c) }
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

  def filterTypeArgs(tparams: List[TypeVar], args: List[AExp])(implicit env: Env): Scored[Above[List[TypeArg]]] = {
    val nv = tparams.size
    val na = args.size
    if (nv != na) fail(s"Type arity mismatch: expected $nv (${tparams mkString ", "}), got $na")
    else product(args map denoteTypeArg) flatMap (above => {
      val args = aboves(above)
      val as = args.beneath
      if (couldMatch(tparams,as)) known(args)
      else failTypeArgs(tparams,as)
    })
  }
  @inline def failTypeArgs(tparams: List[TypeVar], args: List[TypeArg])(implicit env: Env): Scored[Nothing] =
    fail(s"Arguments ${args map (a => showSep(a,"")) mkString ", "} don't fit type variables ${tparams map details mkString ", "}")

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
          case _ => single(FieldExp(CastExp(c.raw,xd),i), Pr.shadowedFieldValue(objs, xd,c,i))
        }
      } else
        single(FieldExp(xd,i), Pr.fieldValue(objs, xd, i))
    }}
  }

  def denoteMethod(i: MethodItem, depth: Int)(implicit env: Env): Scored[MethodDen] = {
    val objs = valuesOfItem(i.parent,i,depth)
    objs flatMap (xd => single(MethodDen(xd,i), Pr.methodCallable(objs, xd, i)))
  }

  def denoteValue(i: Value, depth: Int)(implicit env: Env): Scored[Exp] = {
    @inline def penalize(e: Exp) = if (env.inScope(i)) known(e) else single(e,Pr.outOfScope)
    i match {
      case i:ParameterItem => if (env.inScope(i)) known(ParameterExp(i))
      else fail(s"Parameter $i is shadowed")
      case i:LocalVariableItem => if (env.inScope(i)) known(LocalVariableExp(i))
      else fail(s"Local variable $i is shadowed")

      // We can always access this, static fields, or enums.
      // Pretty-printing takes care of finding a proper name, but we reduce score for out of scope items.
      case LitValue(x) => known(x)
      case i:StaticFieldItem => penalize(StaticFieldExp(None,i))
      case i:EnumConstantItem => penalize(EnumConstantExp(None,i))
      case i:ThisItem => penalize(ThisExp(i))
      case i:SuperItem => penalize(SuperExp(i))

      case i:FieldItem if env.inScope(i) => known(LocalFieldExp(i))
      case i:FieldItem => denoteField(i, depth)
    }
  }

  def denoteCallable(e: AExp, inNew: Boolean)(implicit env: Env): Scored[(Callable,Option[List[TypeArg]])] = {
    def knownNotNew[A](x: A): Scored[A] = single(x,if (inNew) 1 else Pr.dropNew)
    def biasedNotNew[A](x: => Scored[A]): Scored[A] = if (inNew) x else biased(Pr.dropNew,x)
    def dropNew(p: Prob) = if (inNew) pmul(p,Pr.dropNew) else p
    e match {
      case NameAExp(n) =>
        callableScores(n) flatMap {
          case i: MethodItem if i.isStatic => knownNotNew(StaticMethodDen(None,i),None)
          case i: MethodItem if env.inScope(i) => knownNotNew(LocalMethodDen(i),None)
          case i: MethodItem => biasedNotNew(denoteMethod(i, 0) map ((_,None)))
          case i: ConstructorItem => known(NewDen(None,i),None)
          case ThisItem(c) => biasedNotNew(uniformGood(Pr.forwardThis,c.constructors) flatMap {
            case cons if cons == env.place.place => fail("Can't forward to current constructor")
            case cons => known(ForwardDen(Some(c.inside), cons),None)
          })
          case SuperItem(c) => biasedNotNew({
            val tenv = c.env
            uniformGood(Pr.forwardSuper,c.item.constructors) map (cc => (ForwardDen(Some(c),cc),None))
          })
        }

      // Java doesn't allow parentheses around callables, but we do
      case ParenAExp(x,_) => biased(Pr.parensAroundCallable,denoteCallable(x,inNew))

      // x is either a type or an expression, f is a method, static method, or constructor
      case FieldAExp(x,ts,f) =>
        val fs = env.collect(f,s"No callable item $f",{case f:CallableItem with Member if maybeMemberIn(f) => f})
        // TODO: Also try applying ts to the type
        // TODO: Convert ts expressions to type args and check whether the type arguments fit
        val cs = product(denote(x,ExpTypeMode),fs) flatMap {
          case (x,f) if !memberIn(f,x.item) => fail(s"${show(x)} does not contain $f")
          case (x:Exp,    f:MethodItem) if f.isStatic => single(StaticMethodDen(Some(x),f),dropNew(Pr.staticFieldCallableWithObject))
          case (x:TypeDen,f:MethodItem) if f.isStatic => knownNotNew(StaticMethodDen(None,f).discard(x.discards))
          case (x:Exp,f:MethodItem) => knownNotNew(MethodDen(x,f))
          case (_,    f:MethodItem) => fail(show(f)+" is not static, and is used without an object")
          // TODO: Also try applying the type arguments to the class (not the constructor)
          case (x:Exp,             f:ConstructorItem) => single(NewDen(Some(x.ty.asInstanceOf[ClassType]),f).discard(effects(x)),
            Pr.constructorFieldCallableWithObject) // only Classes have constructors, so x.ty is a ClassType
          case (TypeDen(ds,t:Type),f:ConstructorItem) => single(NewDen(Some(t.asInstanceOf[ClassType]),f).discard(ds),
            Pr.constructorFieldCallable) // only Classes have constructors, so t is a ClassType
        }
        ts match {
          case None => cs map ((_,None))
          case Some(ts) => product(ts.list map denoteTypeArg) flatMap (args => aboves(args) match {
            case Above(ds,as) => cs map (c => (c.discard(ds),Some(as)))
          })
        }

      // C++-style application of type arguments to a generic method
      case TypeApplyAExp(x,ts) => denoteCallable(x,inNew) flatMap {
        // Callable doesn't have type args yet
        case (c,None) => biased(Pr.typeApplyCallable,filterTypeArgs(c.tparams,ts.list) map {case Above(ds,ts) => (c.discard(ds),Some(ts))})
        case c => fail(s"Cannot apply type arguments $ts to callable $c")
      }

      case NewAExp(Some(_),_) => notImplemented
      case NewAExp(None,x) => biasedNotNew(denoteCallable(x,inNew))

      case MethodRefAExp(x,ts,f) => throw new NotImplementedError("MethodRefs not implemented: " + e)
      case NewRefAExp(x,t) => throw new NotImplementedError("NewRef not implemented: " + e)

      // Objects that are function-like interfaces should be callable, but that is handled in denoteExp: ApplyAExp
      case CondAExp(c,x,y) => fail(s"${show(e)}: Conditional is not callable") // TODO: Should be callable if x and y are
      case _:ALit|_:ApplyAExp|_:UnaryAExp|_:BinaryAExp|_:CastAExp|_:AssignAExp|_:InstanceofAExp|_:WildAExp|_:ArrayAExp =>
        fail(s"${show(e)}: Expression is not callable")
    }
  }

  sealed abstract class Mode
  sealed trait HasTypeMode extends Mode
  sealed trait HasExpMode extends Mode
  case object TypeMode extends Mode with HasTypeMode
  case object ExpMode extends Mode with HasExpMode
  case object ExpTypeMode extends Mode with HasTypeMode with HasExpMode

  def denoteType(e: AExp)(implicit env: Env): Scored[TypeDen] = denote(e,TypeMode).asInstanceOf[Scored[TypeDen]]
  def denoteExp(e: AExp)(implicit env: Env): Scored[Exp] = denote(e,ExpMode).asInstanceOf[Scored[Exp]]

  def denote(e: AExp, m: Mode)(implicit env: Env): Scored[ExpOrType] = (e,m) match {
    // Literals
    case (x:ALit,ExpMode) => denoteLit(x)

    // Names
    case (NameAExp(n),TypeMode) => env.collect(n,s"Type $n not found",{
      case t:TypeItem if typeAccessible(t) => TypeDen(Nil,t.raw)
    })
    case (NameAExp(n),ExpMode) => env.flatMap(n,s"Value $n not found",{
      case v:Value => denoteValue(v,depth=0)
      case i:Item => fail(s"$i is not a Value")
    })
    case (NameAExp(n),ExpTypeMode) => env.flatMap(n,s"Value or type $n not found",{
      case v:Value => denoteValue(v,depth=0)
      case t:TypeItem if typeAccessible(t) => known(TypeDen(Nil,t.raw))
      case t:TypeItem => fail(s"${show(t)} is inaccessible")
      case i:Item => fail(s"$i is not a Value or a Type")
    })

    // Parentheses.  Java doesn't allow parentheses around types, but we do.
    case (ParenAExp(x,_),TypeMode) => biased(Pr.parensAroundType,denoteType(x))
    case (ParenAExp(x,_),ExpMode) => denoteExp(x) map ParenExp
    case (ParenAExp(x,_),ExpTypeMode) => denote(x,m) flatMap {
      case x:Exp => known(ParenExp(x))
      case x:TypeDen => single(x,Pr.parensAroundType)
    }

    // Fields.  x is either a type or an expression, f is an inner type, method, or field.
    case (FieldAExp(_,Some(_),_),_) => fail(m match {
      case TypeMode => "Inner classes' type arguments go after the class"
      case ExpMode => s"${show(e)}: Non-callable field access takes no template arguments"
      case ExpTypeMode => s"${show(e)}: Unexpected type arguments"
    })
    case (FieldAExp(x,None,f),_) =>
      val fs = env.collect(f,s"$f doesn't look like a field",{
        case f:TypeItem with Member if m.isInstanceOf[HasTypeMode] && maybeMemberIn(f) => f
        case f:Value with Member if m.isInstanceOf[HasTypeMode] && maybeMemberIn(f) => f
      })
      product(denote(x,ExpTypeMode),fs) flatMap {case (x,f) => (x,f,m) match {
        case _ if !memberIn(f,x.item) => fail(s"${show(x)} does not contain $f")
        case (x:Exp,f:TypeItem,_:HasTypeMode) => single(TypeDen(effects(x),typeIn(f,x.ty)),Pr.typeFieldOfExp)
        case (TypeDen(ds,t:Type),f:TypeItem,_:HasTypeMode) => known(TypeDen(ds,typeIn(f,t)))
        case (x:Exp,             f:EnumConstantItem,_:HasExpMode) => single(EnumConstantExp(Some(x),f),Pr.enumFieldExpWithObject)
        case (TypeDen(ds,_:Type),f:EnumConstantItem,_:HasExpMode) => single(EnumConstantExp(None,f).discard(ds),Pr.enumFieldExp)
        case (x:Exp,             f:StaticFieldItem,_:HasExpMode) => single(StaticFieldExp(Some(x),f),Pr.staticFieldExpWithObject)
        case (TypeDen(ds,_:Type),f:StaticFieldItem,_:HasExpMode) => single(StaticFieldExp(None,f).discard(ds),Pr.staticFieldExp)
        case (x:Exp,f:FieldItem,_:HasExpMode) => single(FieldExp(x,f),Pr.fieldExp)
        case _ => fail(s"Invalid field ${show(x)}  .  ${show(f)}")
      }}

    // Type application.  TODO: add around to TypeApplyAExp
    case (TypeApplyAExp(x,ts),_:HasTypeMode) => biased(Pr.typeApply,denoteType(x) flatMap {
      // TODO: Do something smarter, similar to fiddleCall, if there are bounded parameters?
      case TypeDen(ds0,RawType(c,parent)) => filterTypeArgs(c.tparams,ts.list) map {
        case Above(ds1,ts) => TypeDen(ds0++ds1,GenericType(c,ts,parent))
      }
      case TypeDen(_,x) => fail(s"cannot apply parameters $ts to type $x")
    })

    // Application
    case (ApplyAExp(f,EmptyList,BrackAround),TypeMode) =>
      denoteType(f) map (_.array) // This case also shows up below
    case (ApplyAExp(f,xsn,around),m:HasExpMode) =>
      val n = xsn.list.size
      val args = xsn.list map denoteExp
      // Either array index or call
      val call = biased(Pr.callExp(xsn,around), denoteCallable(f,inNew=false) flatMap {
        case (f,None) => ArgMatching.fiddleCall(f,args) map { case (ts,xs) => ApplyExp(f,ts,xs) }
        case (f,Some(ts)) =>
          if (f.arity != ts.size) fail(s"${show(e)}: Arity mismatch: expected ${f.arity}, got ${ts.size}")
          else if (!couldMatch(f.tparams,ts)) failTypeArgs(f.tparams,ts)
          else {
            // TODO: Map.empty may be wrong here
            val tenv = capture(f.tparams,ts,Map.empty)._1
            val fts = new Signature {
              override def toString = s"$f<${ts map (showSep(_,"")) mkString ","}>"
              def tparams = Nil
              def alltparams = Nil
              val params = f.params map (_.substitute(tenv))
            }
            ArgMatching.fiddleCall(fts,args) map {
              case (_::_,_) => impossible
              case (Nil,xs) => ApplyExp(f,ts,xs)
            }
          }
      })
      if (n == 0) (m,around) match { // No arguments is never array access
        case (ExpTypeMode,BrackAround) => call ++ (denoteType(f) map (_.array)) // But it might be an ArrayType
        case _ => call
      } else call ++ biased(Pr.indexCallExp(xsn,around),
        productWith(denoteExp(f).filter(f => hasDims(f.ty,n),show(e)+s": expected >= $n dimensions"),
                    product(args map (_ flatMap denoteIndex)))((a,is) => is.foldLeft(a)(IndexExp)))

    case (UnaryAExp(op,x),_:HasExpMode) => denoteExp(x) flatMap {
      case x if unaryLegal(op,x.ty) => single(op match {
        case op:ImpOp => ImpExp(op,x)
        case op:NonImpOp => NonImpExp(op,x)
      }, Pr.unaryExp)
      case x => fail(s"${show(e)}: invalid unary ${show(token(op))} on type ${show(x.ty)}")
    }

    case (BinaryAExp(op,x,y),_:HasExpMode) => product(denoteExp(x),denoteExp(y)) flatMap {case (x,y) => {
      val tx = x.ty
      val ty = y.ty
      if (binaryLegal(op,tx,ty)) single(BinaryExp(op,x,y), Pr.binaryExp)
      else fail("${show(e)}: invalid binary op ${show(tx)} ${show(op)} ${show(ty)}")
    }}

    case (CastAExp(t,x),_:HasExpMode) => product(denoteType(t),denoteExp(x)) flatMap {case (TypeDen(ds,t),x) => {
      val tx = x.ty
      if (castsTo(tx,t)) single(CastExp(t,x).discard(ds),Pr.castExp)
      else fail("${show(e)}: can't cast ${show(tx)} to ${show(t)}")
    }}

    case (CondAExp(c,x,y),_:HasExpMode) =>
      biased(Pr.condExp,product(denoteBool(c),denoteExp(x),denoteExp(y)) map {case (c,x,y) =>
        CondExp(c,x,y,condType(x.ty,y.ty))})

    case (AssignAExp(op,x,y),_:HasExpMode) => {
      product(denoteVariable(x),denoteExp(y)) flatMap {case (x,y) => {
        val tx = x.ty
        val ty = y.ty
        assignOpType(op,tx,ty) match {
          case None => fail(s"${show(e)}: invalid assignop ${show(tx)} ${show(token(op))} ${show(ty)}")
          case Some(t) => single(AssignExp(op,x,y), Pr.assignExp)
        }
      }}
    }

    case (ArrayAExp(xs,a),_:HasExpMode) =>
      biased(Pr.arrayExp,product(xs.list map denoteExp) map (is => ArrayExp(condTypes(is map (_.ty)),is)))

    case _ => fail(s"${show(e)}: doesn't match mode $m")
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
    case ParameterExp(i) => !i.isFinal
    case LocalVariableExp(i) => !i.isFinal
    case EnumConstantExp(_,_) => false
    case CastExp(_,_) => false // TODO: java doesn't allow this, but I don't see why we shouldn't
    case _:UnaryExp => false // TODO: java doesn't allow this, but we should. Easy for ++,--, and -x = 5 should translate to x = -5
    case BinaryExp(_,_,_) => false
    case AssignExp(_,_,_) => false
    case ParenExp(x) => isVariable(x)
    case ApplyExp(_,_,_) => false
    case FieldExp(_,f) => !f.isFinal
    case LocalFieldExp(f) => !f.isFinal
    case StaticFieldExp(_,f) => !f.isFinal
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
                (env.popScope,ForeachStmt(t,v,e,blocked(s)).discard(discardsOption(at)))
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
