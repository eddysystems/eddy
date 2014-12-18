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

  // If an expression has side effects, evaluate it and return a type
  def discardType(e: Exp, f: Scored[Type]): Scored[AboveType] = effects(e) match {
    case Nil => f
    case ds => f map (_.discard(ds))
  }
  def discardCallable(e: Exp, f: Scored[Callable]): Scored[Callable] = effects(e) match {
    case Nil => f
    case ds => f map (_.discard(ds))
  }

  def mapTypeTypeDiscards(t: AboveType, f: Type => Scores.Scored[Type]): Scores.Scored[AboveType] = t match {
    case t:Type => f(t)
    case dt@(DiscardType(_,_)|DiscardRefType(_,_)) => f(dt.beneath) map (_.discard(dt.discards))
  }
  def mapTypeExpDiscards(t: AboveType, f: Type => Scores.Scored[Exp]): Scores.Scored[Exp] = t match {
    case t:Type => f(t)
    case dt@(DiscardType(_,_)|DiscardRefType(_,_)) => f(dt.beneath) map (_.discard(dt.discards))
  }
  def mapTypeCallableDiscards(t: AboveType, f: Type => Scores.Scored[Callable]): Scores.Scored[Callable] = t match {
    case t:Type => f(t)
    case dt@(DiscardType(_,_)|DiscardRefType(_,_)) => f(dt.beneath) map (_.discard(dt.discards))
  }
  def collectTypeArgDiscards(ts: List[AboveTypeArg], f: List[TypeArg] => Scores.Scored[Type]): Scores.Scored[AboveType] = {
    val discards = (ts map (_.discards)).fold(Nil) { (x:List[Denotations.Stmt],y:List[Denotations.Stmt]) => (x :: y).asInstanceOf[List[Denotations.Stmt]] }
    val tys = ts map (_.beneath)
    f(tys) map (_.discard(discards))
  }
  def collectTypeArgCallableDiscards(ts: List[AboveTypeArg], f: List[TypeArg] => Scores.Scored[(Callable,Option[List[TypeArg]])]): Scores.Scored[(Callable,Option[List[TypeArg]])] = {
    val discards = (ts map (_.discards)).fold(Nil) { (x:List[Denotations.Stmt],y:List[Denotations.Stmt]) => (x :: y).asInstanceOf[List[Denotations.Stmt]] }
    val tys = ts map (_.beneath)
    f(tys) map { case (c,t) => (c.discard(discards),t) }
  }

  def denoteTypeArg(e: AExp)(implicit env: Env): Scored[AboveTypeArg] = e match {
    // TODO: passing _, *, or a name should probably also work just as well as '?' (names at least for extends).
    // The name would be made into a class definition like so:
    //   Collection<A extends Integer>
    // turns into
    //   [abstract] class A extends Integer {};
    //   Collection<A> ...
    // this does not work for ? super Integer.
    case WildAExp(b) => b match {
      case None => single(WildSub(ObjectType), Pr.wildcard)
      case Some((AST.Extends,inner)) => denoteType(inner) flatMap {
        case r: RefType => single(WildSub(r), Pr.subWildcard)
        case DiscardType(ls,r:RefType) => single(WildSub(r).discard(ls), Pr.subWildcard)
        case _ => fail(s"cannot use $inner in a wildcard expression")
      }
      case Some((AST.Super,inner)) => denoteType(inner) flatMap {
        case r: RefType => single(WildSub(r), Pr.superWildcard)
        case DiscardType(ls,r:RefType) => single(WildSub(r).discard(ls), Pr.superWildcard)
        case _ => fail(s"cannot use $inner in a wildcard expression")
      }
    }

    case _ => denoteType(e) flatMap {
      case r:RefType => known(r)
      case DiscardType(ls,r:RefType) => known(DiscardTypeArg(ls,r))
      case _ => fail(s"cannot use $e as a typearg")
    }
  }

  // Types
  def denoteType(e: AExp)(implicit env: Env): Scored[AboveType] = e match {
    case NameAExp(n) => typeScores(n)
    case ParenAExp(x,_) => biased(Pr.parensAroundType,denoteType(x)) // Java doesn't allow parentheses around types, but we do

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) fail("inner classes' type arguments go after the class") else {
      // First, the ones where x is a type
      val tdens = biased(Pr.typeFieldOfType, denoteType(x) flatMap (t => mapTypeTypeDiscards(t,typeFieldScores(_,f))))
      val edens = biased(Pr.typeFieldOfExp,  denoteExp(x) flatMap (x => discardType(x,typeFieldScores(x.ty,f))) )
      tdens ++ edens
    }

    // TODO: add around to TypeApplyAExp
    case TypeApplyAExp(x,ts) => denoteType(x) bias Pr.typeApply flatMap { den => den.beneath match {
      // TODO: do something smarter, similar to fiddleCall, if there are bounded parameters?
      case RawType(ci,parent) if ci.arity == ts.list.size => {
        val tys = ts.list map denoteTypeArg
        // filter the types according to what fits where
        productWithReversePrefixFilter(tys, ci.makeReverseIncrementalMatcher(Some(parent))) flatMap { ls =>
          println(s"collecting typeargs discards for $ls")
          collectTypeArgDiscards(ls, ts => single(GenericType(ci,ts,parent), Pr.base))
        }
      }
      case x => fail(s"cannot apply parameters $ts to type $x")
    }}

    case ApplyAExp(x,EmptyList,BrackAround) => denoteType(x) flatMap (t => mapTypeTypeDiscards(t,t => known(ArrayType(t))))

    case MethodRefAExp(x,ts,f) => throw new NotImplementedError("MethodRefs not implemented: " + e)
    case NewRefAExp(x,t) => throw new NotImplementedError("NewRef not implemented: " + e)
    case NewAExp(ts,e) => throw new NotImplementedError("new expression not implemented: " + e)

    case WildAExp(_) => fail("wildcards are type arguments, not types")
    case _:ALit|_:ApplyAExp|_:UnaryAExp|_:BinaryAExp|_:CastAExp|_:CondAExp
        |_:AssignAExp|_:ArrayAExp|_:InstanceofAExp => fail(show(e)+": is not a type")
  }

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

  def denoteValue(i: Value, depth: Int)(implicit env: Env): Scored[Exp] = i match {
    case i: ParameterItem if env.inScope(i) => known(ParameterExp(i))
    case i: LocalVariableItem if env.inScope(i) => known(LocalVariableExp(i))

    // We can always access this, static fields, or enums. Pretty-printing takes care of finding a proper name.
    case LitValue(x) => known(x)
    case i: StaticFieldItem => known(StaticFieldExp(None,i))
    case i: EnumConstantItem => known(EnumConstantExp(None,i))
    case i: ThisItem => known(ThisExp(i))
    case i: SuperItem => known(SuperExp(i))

    case i: FieldItem if env.inScope(i) => known(LocalFieldExp(i))
    case i: FieldItem => denoteField(i, depth)
  }

  def denoteCallable(e: AExp)(implicit env: Env): Scored[(Callable,Option[List[AboveTypeArg]])] = e match {
    case NameAExp(n) => callableScores(n) flatMap {
      case i: MethodItem if i.isStatic => known(StaticMethodDen(None,i), None)
      case i: MethodItem if env.inScope(i) => known(LocalMethodDen(i), None)
      case i: MethodItem => denoteMethod(i, 0) map ((_,None))
      case i: ConstructorItem => known(NewDen(None,i),None)
      case ThisItem(c) => uniformGood(1,c.constructors) flatMap {
        case cons if cons == env.place.place => fail("Can't forward to current constructor")
        case cons => known(ForwardDen(Some(c.inside), cons),None)
      }
      case SuperItem(c) => {
        val tenv = c.env
        uniformGood(1,c.item.constructors) map (cc => (ForwardDen(Some(c),cc),None))
      }
    }
    case ParenAExp(x,_) => biased(Pr.parensAroundCallable,denoteCallable(x)) // Java doesn't allow parentheses around callables, but we do

    // x is either a type or an expression, f is a method, static method, or constructor
    case FieldAExp(x,ts,f) => {
      // first, the ones where x is a type
      // TODO: also try applying ts to the type
      // TODO: convert ts expressions to typeargs and check whether the type arguments fit
      val tdens = denoteType(x) flatMap (t => mapTypeCallableDiscards(t, t => callableFieldScores(t.item,f) flatMap {
          case f:MethodItem if f.isStatic => single(StaticMethodDen(None,f), Pr.staticFieldCallable)
          case f:MethodItem => fail(show(f)+" is not static, and is used without an object.")
          case f:ConstructorItem => single(NewDen(Some(t.asInstanceOf[ClassType]), f), Pr.constructorFieldCallable) // only Classes have constructors, so t is a ClassType
        }))
      val edens = denoteExp(x) flatMap (x => callableFieldScores(x.item,f) flatMap {
        case f:MethodItem if f.isStatic => single(StaticMethodDen(Some(x),f),Pr.staticFieldCallableWithObject)
        case f:MethodItem => single(MethodDen(x,f),Pr.methodFieldCallable)
        // TODO: also try applying the type arguments to the class (not the constructor)
        case f:ConstructorItem => discardCallable(x,single(NewDen(Some(x.ty.asInstanceOf[ClassType]),f),Pr.constructorFieldCallableWithObject)) // only Classes have constructors, so x.ty is a ClassType
      })

      if (!ts.isDefined) tdens ++ edens map ((_,None))
      else notImplemented // TODO
    }

    // C++-style application of type arguments to a generic method
    case TypeApplyAExp(x,ts) => denoteCallable(x) flatMap {
      // callable doesn't have typeargs yet
      case (c,None) if ts.list.size == c.tparams.size =>
        val tys = ts.list map denoteTypeArg
        // filter the types according to what fits where
        productWithReversePrefixFilter(tys, c.makeReverseIncrementalMatcher(c.parent)) flatMap { ls =>
          collectTypeArgCallableDiscards(ls, ts => single((c,Some(ts)), Pr.typeApplyCallable))
        }
      case xden => fail(s"cannot apply type arguments $ts to callable $xden")
    }

    case MethodRefAExp(x,ts,f) => throw new NotImplementedError("MethodRefs not implemented: " + e)
    case NewRefAExp(x,t) => throw new NotImplementedError("NewRef not implemented: " + e)
    case NewAExp(ts,e) => throw new NotImplementedError("new expression not implemented: " + e)

    // objects that are function-like interfaces should be callable, but that is handled in denoteExp: ApplyAExp
    case x: ALit => fail("literals are not callable.")
    case ApplyAExp(_,_,_) => fail("results of function applications are not callable.")
    case UnaryAExp(_,_) => fail("expressions are not callable")
    case BinaryAExp(_,_,_) => fail("expressions are not callable")
    case CastAExp(_,_) => fail("expressions are not callable")
    case CondAExp(c,x,y) => fail("expressions are not callable") // TODO: of course, this should be callable if the two options are
    case AssignAExp(op,x,y) => fail("expressions are not callable")
    case ArrayAExp(xs,a) => fail("expressions are not callable")
    case InstanceofAExp(_,_) => fail("expressions are not callable")
    case WildAExp(b) => fail("wildcard expressions are not callable")
  }

  def denoteExp(e: AExp)(implicit env: Env): Scored[Exp] = e match {
    case NameAExp(n) => valueScores(n) flatMap (denoteValue(_,depth=0))
    case x: ALit => denoteLit(x)
    case ParenAExp(x,_) => biased(Pr.parenExp,denoteExp(x) map ParenExp)

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // First, the ones where x is a type
      // TODO: penalize unnecessarily qualified field expressions?
      val tdens = denoteType(x) flatMap (t => mapTypeExpDiscards(t,t => staticFieldScores(t.item,f) flatMap {
        case f: EnumConstantItem => single(EnumConstantExp(None,f), Pr.enumFieldExp)
        case f: StaticFieldItem => single(StaticFieldExp(None,f), Pr.staticFieldExp)
      }))
      // Now, x is an expression
      val edens = denoteExp(x) flatMap (x => fieldScores(x.item,f) flatMap {
        case f: EnumConstantItem => single(EnumConstantExp(Some(x),f), Pr.enumFieldExpWithObject)
        case f: StaticFieldItem => single(StaticFieldExp(Some(x),f), Pr.staticFieldExpWithObject)
        case f: FieldItem => single(FieldExp(x,f), Pr.fieldExp)
      })
      tdens++edens
    }

    case ApplyAExp(f,xsn,around) => {
      val n = xsn.list.size
      val args = xsn.list map denoteExp
      // Either array index or call
      val call = biased(Pr.callExp(xsn,around), denoteCallable(f) flatMap {
        case (f,None) => ArgMatching.fiddleCall(f,args)
        case (f,Some(ts)) => throw new NotImplementedError("Generic method calls not implemented")
      })
      if (n == 0) call // No arguments is never array access
      else call ++ biased(Pr.indexCallExp(xsn,around),
        productWith(denoteExp(f).filter(f => hasDims(f.ty,n),show(e)+s": expected >= $n dimensions"),
                    product(args map (_ flatMap denoteIndex)))((a,is) => is.foldLeft(a)(IndexExp)))
    }

    case UnaryAExp(op,x) => denoteExp(x) flatMap {
      case x if unaryLegal(op,x.ty) => single(op match {
        case op:ImpOp => ImpExp(op,x)
        case op:NonImpOp => NonImpExp(op,x)
      }, Pr.unaryExp)
      case x => fail(s"${show(e)}: invalid unary ${show(token(op))} on type ${show(x.ty)}")
    }

    case BinaryAExp(op,x,y) => product(denoteExp(x),denoteExp(y)) flatMap {case (x,y) => {
      val tx = x.ty
      val ty = y.ty
      if (binaryLegal(op,tx,ty)) single(BinaryExp(op,x,y), Pr.binaryExp)
      else fail("${show(e)}: invalid binary op ${show(tx)} ${show(op)} ${show(ty)}")
    }}

    case CastAExp(t,x) => product(denoteType(t),denoteExp(x)) flatMap {case (t,x) => mapTypeExpDiscards(t,t => {
      val tx = x.ty
      if (castsTo(tx,t)) single(CastExp(t,x), Pr.castExp)
      else fail("${show(e)}: can't cast ${show(tx)} to ${show(t)}")
    })}

    case CondAExp(c,x,y) =>
      biased(Pr.condExp,product(denoteBool(c),denoteExp(x),denoteExp(y)) map {case (c,x,y) =>
        CondExp(c,x,y,condType(x.ty,y.ty))})

    case AssignAExp(op,x,y) => {
      product(denoteVariable(x),denoteExp(y)) flatMap {case (x,y) => {
        val tx = x.ty
        val ty = y.ty
        assignOpType(op,tx,ty) match {
          case None => fail(s"${show(e)}: invalid assignop ${show(tx)} ${show(token(op))} ${show(ty)}")
          case Some(t) => single(AssignExp(op,x,y), Pr.assignExp)
        }
      }}
    }

    case ArrayAExp(xs,a) =>
      biased(Pr.arrayExp,product(xs.list map denoteExp) map (is => ArrayExp(condTypes(is map (_.ty)),is)))

    case InstanceofAExp(x,t) => notImplemented // much more likely that you ask this if x.ty has strict subtypes that are also subtypes of t (in case x.ty or t is an interface type, otherwise this just means x.ty should be a supertype of t)
    case MethodRefAExp(x,ts,f) => throw new NotImplementedError("MethodRefs not implemented: " + e)
    case NewRefAExp(x,t) => throw new NotImplementedError("NewRef not implemented: " + e)
    case NewAExp(ts,e) => throw new NotImplementedError("new expression not implemented: " + e)

    case WildAExp(b) => fail("wildcard types are not expressions")
    case TypeApplyAExp(x,ts) => fail("type arguments make no sense for expression")
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
          val st = t.safe
          if (st.isDefined)
            define(env,ds.list) flatMap {case (env,ds) => single((env,VarStmt(st.get,ds).discard(at.discards)), Pr.varStmt) }
          else
            fail(s"cannot make variables of type $t.")
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
          case _ => fail(show(e)+": expression doesn't look like a statement")
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
      case ds => (env,(s.beneath::ds).reverse)
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
