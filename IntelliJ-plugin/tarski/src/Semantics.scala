package tarski

import org.apache.commons.lang.StringEscapeUtils.unescapeJava

import AST._
import Types._
import Operators._
import Environment._
import Items._
import Denotations._
import Scores._
import tarski.Tokens._
import tarski.Pretty._
import ambiguity.Utility._
import scala.language.implicitConversions

object Semantics {
  // We support generating prior statements above expressions as expressions are parsed.
  type ScoredAbove[A] = ScoredAccum[Stmt,A]

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
      case BoolALit(b) =>   single(BooleanLit(b), Pr.booleanLit)
      case CharALit(v) =>   single(CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0),v), Pr.charLit)
      case StringALit(v) => single(StringLit(unescapeJava(v.slice(1,v.size-1)),v), Pr.stringLit)
      case NullALit() =>    single(NullLit, Pr.nullLit)
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

  // If an expression has side effects, evaluate it above
  def discard[A,B](e: Exp)(f: ScoredAbove[B]): ScoredAbove[B] = {
    val s = if (noEffects(e)) Nil else List(ExpStmt(e))
    ScoredAccum(known((s,()))) flatMap (u => f)
  }

  // Types
  def denoteType(e: AExp)(implicit env: Env): ScoredAbove[Type] = e match {
    case NameAExp(n) => typeScores(n)
    case x: ALit => fail("literals are not types.")
    case ParenAExp(x) => denoteType(x) flatMap { x => single(x,Pr.parensAroundType(x)) } // Java doesn't allow parentheses around types, but we do

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // First, the ones where x is a type
      val tdens = denoteType(x) flatMap (t => typeFieldScores(t,f) flatMap (f => single(f,Pr.typeFieldOfType(t,f))))
      val edens = denoteExp(x) flatMap (x => discard(x)(typeFieldScores(typeOf(x),f) flatMap (f => single(f,Pr.typeFieldOfExp(x,f)))))
      tdens ++ edens
    }

    case MethodRefAExp(x,ts,f) => throw new NotImplementedError("MethodRefs not implemented: " + e)
    case NewRefAExp(x,t) => throw new NotImplementedError("NewRef not implemented: " + e)
    case TypeApplyAExp(x,ts) => throw new NotImplementedError("Generics not implemented (TypeApplyExp): " + e)
    case NewAExp(ts,e) => throw new NotImplementedError("new expression not implemented: " + e)
    case WildAExp(b) => throw new NotImplementedError("wildcard expressions not implemented: " + e)

    case ApplyAExp(f,xsn,_) => fail("expressions are not types")
    case UnaryAExp(op,x) => fail("expressions are not types")
    case BinaryAExp(op,x,y) => fail("expressions are not types")
    case CastAExp(t,x) => fail("expressions are not types")
    case CondAExp(c,x,y) => fail("expressions are not types") // TODO: translate to if statement somehow
    case AssignAExp(op,x,y) => fail("expressions are not types")
    case ArrayAExp(xs,a) => fail("expressions are not types")
    case InstanceofAExp(_,_) => fail("expressions are not types")
  }

  // Are we contained in the given type, or in something contained in the given type?
  def containedIn(i: Item, t: TypeItem): Boolean = i match {
    case f: Member => f.parent == t || containedIn(f.parent,t)
    case _ => false
  }

  def denoteField[ItemKind <: Item with ClassMember,FieldDen]
                 (i: ItemKind, combine: (Exp,ItemKind) => FieldDen,
                  superFieldProb: (Probs[Exp],TypeItem,ItemKind) => Prob,
                  shadowedFieldProb: (Probs[Exp],Exp,TypeItem,ItemKind) => Prob,
                  fieldProb: (Probs[Exp],Exp,ItemKind) => Prob)(implicit env: Env): Scored[FieldDen] = {
    val c: TypeItem = i.parent
    val objs = objectsOfItem(c) flatMap { x =>
      if (containedIn(x,c)) fail(s"Field ${show(i)}: all objects of item ${show(c)} contained in ${show(c)}")
      else denoteValue(x)
    }

    objs flatMap { xd => {
      val vprobs = objs.all.right.get
      if (shadowedInSubType(i, typeOf(xd).asInstanceOf[RefType])) {
        xd match {
          case ThisExp(ThisItem(tt:ClassItem)) if tt.base.item == c => single(combine(SuperExp(ThisItem(tt)),i), superFieldProb(vprobs, c, i))
          case _ => single(combine(CastExp(c.raw,xd),i), shadowedFieldProb(vprobs, xd,c,i))
        }
      } else {
        single(combine(xd,i), fieldProb(vprobs, xd, i))
      }
    }}
  }

  def denoteValue(i: Value)(implicit env: Env): Scored[Exp] = i match {
    case i: ParameterItem if env.itemInScope(i) => single(ParameterExp(i), Pr.parameterValue)
    case i: LocalVariableItem if env.itemInScope(i) => single(LocalVariableExp(i), Pr.localValue)

    // We can always access this, static fields, or enums. Pretty-printing takes care of finding a proper name.
    case i: StaticFieldItem => single(StaticFieldExp(None,i), Pr.staticFieldValue)
    case i: EnumConstantItem => single(EnumConstantExp(None,i), Pr.enumConstantValue)
    case i: ThisItem => single(ThisExp(i), Pr.thisValue)

    case i: FieldItem if env.itemInScope(i) => single(LocalFieldExp(i), Pr.localFieldValue)
    case i: FieldItem => denoteField(i, FieldExp, Pr.superFieldValue, Pr.shadowedFieldValue, Pr.fieldValue)
    case _ => fail("Can't find a denotation for " + i + ", inaccessible")
  }

  def denoteCallable(e: AExp)(implicit env: Env): ScoredAbove[Callable] = e match {
    case NameAExp(n) => callableScores(n) flatMap {
      case i: MethodItem if env.itemInScope(i) => single(LocalMethodDen(i), Pr.localMethodCallable)
      case i: MethodItem => denoteField(i, MethodDen, Pr.superMethodCallable, Pr.shadowedMethodCallable, Pr.methodCallable)
      case i: StaticMethodItem => single(StaticMethodDen(None,i), Pr.staticMethodCallable)
      case i: ConstructorItem => single(NewDen(i), Pr.constructorCallable)
    }
    case ParenAExp(x) => denoteCallable(x) bias Pr.parensAroundCallable // Java doesn't allow parentheses around callables, but we do

    // x is either a type or an expression, f is a method, static method, or constructor
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // first, the ones where x is a type
      val tdens = denoteType(x) flatMap (t => callableFieldScores(t,f) flatMap {
        case f:StaticMethodItem => single(StaticMethodDen(None,f), Pr.staticFieldCallable)
        case f:MethodItem => fail(show(f)+" is not static, and is used without an object.")
        case f:ConstructorItem => single(NewDen(f), Pr.constructorFieldCallable)
      })
      val edens = denoteExp(x) flatMap (x => liftAccum(callableFieldScores(typeOf(x),f)) flatMap {
        case f:StaticMethodItem => single(StaticMethodDen(Some(x),f),Pr.staticFieldCallableWithObject)
        case f:MethodItem => single(MethodDen(x,f),Pr.methodFieldCallable)
        case f:ConstructorItem => discard(x)(single(NewDen(f),Pr.constructorFieldCallableWithObject))
      })
      tdens ++ edens
    }

    case MethodRefAExp(x,ts,f) => throw new NotImplementedError("MethodRefs not implemented: " + e)
    case NewRefAExp(x,t) => throw new NotImplementedError("NewRef not implemented: " + e)
    case TypeApplyAExp(x,ts) => throw new NotImplementedError("Generics not implemented (TypeApplyExp): " + e)
    case NewAExp(ts,e) => throw new NotImplementedError("new expression not implemented: " + e)
    case WildAExp(b) => throw new NotImplementedError("wildcard expressions not implemented: " + e)

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
  }

  def denoteExp(e: AExp)(implicit env: Env): ScoredAbove[Exp] = e match {
    case NameAExp(n) => valueScores(n) flatMap denoteValue
    case x: ALit => denoteLit(x)
    case ParenAExp(x) => denoteExp(x) map ParenExp bias Pr.parenExp

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // First, the ones where x is a type
      // TODO: penalize unnecessarily qualified field expressions?
      val tdens = denoteType(x) flatMap (t => staticFieldScores(t,f) flatMap {
        case f: EnumConstantItem => single(EnumConstantExp(None,f), Pr.enumFieldExp)
        case f: StaticFieldItem => single(StaticFieldExp(None,f), Pr.staticFieldExp)
      })
      // Now, x is an expression
      val edens = denoteExp(x) flatMap (x => fieldScores(typeOf(x),f) flatMap {
        case f: EnumConstantItem => single(EnumConstantExp(Some(x),f), Pr.enumFieldExpWithObject)
        case f: StaticFieldItem => single(StaticFieldExp(Some(x),f), Pr.staticFieldExpWithObject)
        case f: FieldItem => single(FieldExp(x,f), Pr.fieldExp)
      })
      tdens++edens
    }

    case MethodRefAExp(x,ts,f) => throw new NotImplementedError("MethodRefs not implemented: " + e)
    case NewRefAExp(x,t) => throw new NotImplementedError("NewRef not implemented: " + e)
    case TypeApplyAExp(x,ts) => throw new NotImplementedError("Generics not implemented (TypeApplyExp): " + e)
    case NewAExp(ts,e) => throw new NotImplementedError("new expression not implemented: " + e)
    case WildAExp(b) => throw new NotImplementedError("wildcard expressions not implemented: " + e)

    case ApplyAExp(f,xsn,around) => {
      val xsl = xsn.list map denoteExp
      val n = xsl.size
      def call(f: Callable): ScoredAbove[Exp] =
        productA(xsl) flatMap { xl => ArgMatching.fiddleArgs(f,xl) bias Pr.callExp(xsn,around) }
      def index(f: Exp, ft: Type): ScoredAbove[Exp] = {
        def hasDims(t: Type, d: Int): Boolean = d==0 || (t match {
          case ArrayType(t) => hasDims(t,d-1)
          case _ => false
        })
        if (!hasDims(ft,n)) fail(show(e)+s": expected >= $n dimensions, got ${dimensions(ft)}")
        else {
          val filtered = xsl map (_ flatMap {x => typeOf(x).unboxIntegral match {
            case Some(p) if promote(p) == IntType => single(x, Pr.indexCallExp(xsn, around))
            case _ => fail(s"Index ${show(x)} doesn't convert to int")
          }})
          productA(filtered) map (_.foldLeft(f)(IndexExp))
        }
      }
      val adens = denoteArray(f) flatMap (a => index(a,typeOf(a)))
      val cdens = denoteCallable(f) flatMap call
      adens ++ cdens
    }

    case UnaryAExp(op,x) => denoteExp(x) flatMap {
      case x if unaryLegal(op,typeOf(x)) => single(UnaryExp(op,x), Pr.unaryExp)
      case x => fail(s"${show(e)}: invalid unary ${show(token(op))} on type ${show(typeOf(x))}")
    }

    case BinaryAExp(op,x,y) => productA(denoteExp(x),denoteExp(y)) flatMap {case (x,y) => {
      val tx = typeOf(x)
      val ty = typeOf(y)
      if (binaryLegal(op,tx,ty)) single(BinaryExp(op,x,y), Pr.binaryExp)
      else fail("${show(e)}: invalid binary op ${show(tx)} ${show(op)} ${show(ty)}")
    }}

    case CastAExp(t,x) => productA(denoteType(t),denoteExp(x)) flatMap {case (t,x) => {
      val tx = typeOf(x)
      if (castsTo(tx,t)) single(CastExp(t,x), Pr.castExp)
      else fail("${show(e)}: can't cast ${show(tx)} to ${show(t)}")
    }}

    case CondAExp(c,x,y) =>
      productA(denoteBool(c),denoteExp(x),denoteExp(y)) map {case (c,x,y) =>
        CondExp(c,x,y,condType(typeOf(x),typeOf(y)))} bias Pr.condExp

    case AssignAExp(op,x,y) => {
      productA(denoteVariable(x),denoteExp(y)) flatMap {case (x,y) => {
        val tx = typeOf(x)
        val ty = typeOf(y)
        assignOpType(op,tx,ty) match {
          case None => fail(s"${show(e)}: invalid assignop ${show(tx)} ${show(token(op))} ${show(ty)}")
          case Some(t) => single(AssignExp(op,x,y), Pr.assignExp)
        }
      }}
    }

    case ArrayAExp(xs,a) =>
      productA(xs.list map denoteExp) map (is => ArrayExp(condTypes(is map typeOf),is)) bias Pr.arrayExp

    case InstanceofAExp(x,t) => notImplemented
  }

  // Expressions with type restrictions
  def denoteBool(n: AExp)(implicit env: Env): ScoredAbove[Exp] = denoteExp(n) flatMap {e =>
    val t = typeOf(e)
    if (t.unboxesToBoolean) single(e, Pr.boolExp)
    else if (t.unboxesToNumeric) single(BinaryExp(NeOp, e, IntLit(0, "0")), Pr.insertComparison(t))
    // TODO: all sequences should probably check whether they're empty (or null)
    else if (t.isInstanceOf[RefType]) single(BinaryExp(NeOp, e, NullLit), Pr.insertComparison(t))
    else fail(s"${show(n)}: can't convert type ${show(t)} to boolean")
  }
  def denoteNonVoid(n: AExp)(implicit env: Env): ScoredAbove[Exp] = denoteExp(n) flatMap {e =>
    if (typeOf(e) != VoidType) single(e, Pr.nonVoidExp)
    else fail(s"${show(n)}: expected non-void expression")
  }
  def denoteArray(e: AExp)(implicit env: Env): ScoredAbove[Exp] = denoteExp(e) flatMap {e => {
    val t = typeOf(e)
    if (t.isInstanceOf[ArrayType]) single(e, Pr.arrayTypeExp)
    else fail(s"${show(e)} has non-array type ${show(t)}")
  }}
  def denoteRef(e: AExp)(implicit env: Env): ScoredAbove[Exp] = denoteExp(e) flatMap {e => {
    val t = typeOf(e)
    if (t.isInstanceOf[RefType]) single(e, Pr.refExp)
    else fail(s"${show(e)} has non-reference type ${show(t)}")
  }}
  def denoteVariable(e: AExp)(implicit env: Env): ScoredAbove[Exp] = {
    denoteExp(e) flatMap { x =>
      if (isVariable(x)) single(x, Pr.variableExp)
      else fail(s"${show(e)}: ${show(x)} cannot be assigned to")
    }
  }

  def isVariable(e: Exp): Boolean = e match {
    // In Java, we can only assign to actual variables, never to values returned by functions or expressions.
    case _: Lit => false
    case ThisExp(_) => false
    case SuperExp(_) => false
    case ParameterExp(i) => !i.isFinal
    case LocalVariableExp(i) => !i.isFinal
    case EnumConstantExp(_,_) => false
    case CastExp(_,_) => false // TODO: java doesn't allow this, but I don't see why we shouldn't
    case UnaryExp(_,_) => false // TODO: java doesn't allow this, but we should. Easy for ++,--, and -x = 5 should translate to x = -5
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
  }

  // Statements
  def denoteStmt(s: AStmt)(env: Env): Scored[(Env,List[Stmt])] = {
    implicit val imp = env
    s match {
      case EmptyAStmt() => single((env,List(EmptyStmt)), Pr.emptyStmt)
      case HoleAStmt() => single((env,List(HoleStmt)), Pr.holeStmt)
      case VarAStmt(m,t,ds) =>
        val isFinal = modifiers(m,Final)
        above(denoteType(t)(env) flatMap (t => {
          def init(t: Type, v: Name, i: Option[AExp], env: Env): ScoredAbove[Option[Exp]] = i match {
            case None => single(None, Pr.varInitNone)
            case Some(e) => denoteExp(e)(env) flatMap {e =>
              if (assignsTo(e,t)) single(Some(e), Pr.varInit)
              else fail(s"${show(s)}: can't assign ${show(e)} to type ${show(t)} in declaration of $v}")
            }
          }
          def define(env: Env, ds: List[AVarDecl]): ScoredAbove[(Env,List[VarDecl])] = ds match {
            case Nil => single((env,Nil), Pr.varDeclNil)
            case (v,k,i)::ds =>
              val tk = arrays(t,k)
              productA(env.newVariable(v,tk,isFinal),init(tk,v,i,env)) flatMap {case ((env,v),i) =>
                define(env,ds) flatMap {case (env,ds) => single((env,(v,k,i)::ds), Pr.varDecl)}}
          }
          val st = t.safe
          if (st.isDefined)
            define(env,ds.list) flatMap {case (env,ds) => single((env,VarStmt(st.get,ds)), Pr.varStmt) }
          else
            fail(s"cannot make variables of type $t.")
        }))
      case ExpAStmt(e) => {
        val exps = denoteExp(e) flatMap { e => single((env,ExpStmt(e)), Pr.expStmt) }
        val stmts: ScoredAbove[(Env,Stmt)] = e match {
          case AssignAExp(None,NameAExp(x),y) => denoteExp(y) flatMap {y => typeOf(y).safe match {
            case Some(t) => env.newVariable(x,t,false) flatMap { case (env,x) => single((env,VarStmt(t,List((x,0,Some(y))))), Pr.assignmentAsVarStmt) }
            case None => fail(s"expression $y does not return anything usable (${typeOf(y)})")
          }}
          case _ => fail(show(e)+": expression doesn't look like a statement")
        }
        above(exps++stmts)
      }
      case BlockAStmt(b) => denoteStmts(b)(env) flatMap {case (e,ss) => single((e,List(BlockStmt(ss))), Pr.blockStmt)}
      case AssertAStmt(c,m) => above(productWithA(denoteBool(c),threadA(m)(denoteNonVoid)){case (c,m) =>
        (env,AssertStmt(c,m))} bias Pr.assertStmt)

      case BreakAStmt(lab) =>
        if (env.inside_breakable) denoteLabel(lab,(env,List(BreakStmt))) bias Pr.breakStmt
        else fail("cannot break outside of a loop or switch statement.")
      case ContinueAStmt(lab) =>
        if (env.inside_continuable) denoteLabel(lab,(env,List(ContinueStmt))) bias Pr.continueStmt
        else fail("cannot break outside of a loop")
      case ReturnAStmt(e) => above(productA(returnType,threadA(e)(denoteExp)) flatMap {case (r,e) =>
        val t = typeOf(e)
        if (assignsTo(e,r)) single((env,ReturnStmt(e)), Pr.returnStmt)
        else fail(s"${show(s)}: type ${show(t)} incompatible with return type ${show(r)}")
      })
      case ThrowAStmt(e) => above(denoteExp(e) flatMap {e =>
        val t = typeOf(e)
        if (isThrowable(t)) single((env,ThrowStmt(e)), Pr.throwStmt)
        else fail(s"${show(s)}: type $t is not throwable")
      })
      case SyncAStmt(e,b) => above(productA(denoteRef(e),denoteScoped(b)(env)) flatMap {
        case (e,(env,b)) => single((env,SyncStmt(e,b)), Pr.syncStmt) })
      case IfAStmt(c,x) => above(productA(denoteBool(c),denoteScoped(x)(env)) flatMap {
        case (c,(env,x)) => single((env,IfStmt(c,x)), Pr.ifStmt) })
      case IfElseAStmt(c,x,y) => above(productA(denoteBool(c),denoteScoped(x)(env)) flatMap {case (c,(env,x)) =>
        denoteScoped(y)(env) flatMap {case (env,y) => single((env,IfElseStmt(c,x,y)), Pr.ifElseStmt) }})
      case WhileAStmt(c,s,flip) => above(productA(denoteBool(c),denoteScoped(s)(env)) flatMap {case (c,(env,s)) =>
        single((env,WhileStmt(xor(flip,c),s)), Pr.whileStmt) })
      case DoAStmt(s,c,flip) => above(productA(denoteScoped(s)(env),denoteBool(c)) flatMap {case ((env,s),c) =>
        single((env,DoStmt(s,xor(flip,c))), Pr.doStmt) })
      case ForAStmt(i,c,u,s) => {
        // Sanitize an initializer into valid Java
        def init(i: List[Stmt]): Scored[(Option[Exp],List[Exp],Stmt) => Stmt] = i match {
          case List(i:VarStmt) => single((c,u,s) => ForStmt(i,c,u,s), Pr.forStmt)
          case _ => allSome(i map {case ExpStmt(e) => Some(e); case _ => None}) match {
            case Some(es) => single((c,u,s) => ForStmt(ForExps(es),c,u,s), Pr.expForStmt)
            case None => single((c,u,s) => BlockStmt(i:::List(ForStmt(ForExps(Nil),c,u,s))), Pr.blockForStmt)
          }
        }
        denoteStmts(i)(env.pushScope) flatMap {case (env,i) => init(i) flatMap (i => {
          product(noAbove(threadA(c)(c => noAbove(denoteBool(c)(env)))),
                  noAbove(threadA(u)(u => noAbove(denoteExp(u)(env)))),
                  denoteScoped(s)(env))
            .map {case (c,u,(env,s)) => (env.popScope,List(i(c,u,s)))}
        })}
      }
      case ForeachAStmt(m,t,v,n,e,s) => {
        val isFinal = modifiers(m,Final) || t.isEmpty
        def hole = show(ForeachAStmt(m,t,v,n,e,HoleAStmt()))
        above(productA(threadA(t)(denoteType),denoteExp(e)) flatMap {case (t,e) =>
          val tc = typeOf(e)
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
                (env.popScope,ForeachStmt(t,v,e,blocked(s)))
              }})
          }
        })
      }
    }
  }

  def above(s: ScoredAbove[(Env,Stmt)]): Scored[(Env,List[Stmt])] =
    s.x map {case (above,(env,s)) => (env,(s::above).reverse)}
  def noAbove[A](s: ScoredAbove[A]): Scored[A] =
    s.x map {case (above,a) => {
      if (above.nonEmpty) fail("No room for above statements in this context")
      a
    }}

  def xor(x: Boolean, y: Exp): Exp =
    if (x) UnaryExp(NotOp,y) else y

  def denoteLabel[A](lab: Option[Name], x: => A): Scored[A] = lab match {
    case None => single(x, Pr.labelNone)
    case Some(_) => notImplemented
  }

  def denoteStmts(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    productFoldLeft(env)(s map denoteStmt) map {case (env,ss) => (env,ss.flatten)} bias Pr.stmtList

  def blocked(ss: List[Stmt]): Stmt = ss match {
    case Nil => EmptyStmt
    case List(s) => s
    case ss => BlockStmt(ss)
  }

  // Statement whose environment is discarded
  def denoteScoped(s: AStmt)(env: Env): Scored[(Env,Stmt)] =
    denoteStmt(s)(env.pushScope) map {case (env,ss) => (env.popScope,blocked(ss))}
  def denoteScoped(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    denoteStmts(s)(env.pushScope) map {case (env,s) => (env.popScope,s)}
}
