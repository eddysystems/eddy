package tarski

import org.apache.commons.lang.StringEscapeUtils.unescapeJava

import AST._
import Types._
import Environment._
import Items._
import Denotations._
import Scores._
import tarski.Tokens._
import tarski.Pretty._
import ambiguity.Utility._

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
      case BoolALit(b) =>   single(BooleanLit(b), Pr.booleanLit)
      case CharALit(v) =>   single(CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0),v), Pr.charLit)
      case StringALit(v) => single(StringLit(unescapeJava(v.slice(1,v.size-1)),v), Pr.stringLit)
      case NullALit() =>    single(NullLit, Pr.nullLit)
    }
  }

  // Types
  def denoteType(n: AType)(implicit env: Env): Scored[Type] = n match {
    case NameAType(n) => typeScores(n)
    case VoidAType() => typeScores("void")
    case PrimAType(t) => typeScores(show(pretty(t)))
    case FieldAType(x,f) => ( for (t <- denoteType(x); fi <- typeFieldScores(t,f)) yield (t,fi) ) flatMap { case (t,fi) =>
      single(fi, Pr.fieldType(t,fi)) }
    case ModAType(Annotation(_),t) => throw new NotImplementedError("Types with annotations")
    case ModAType(_,_) => fail("Not implemented: type modifiers")
    case ArrayAType(t) => denoteType(t) flatMap { t => single(ArrayType(t), Pr.arrayType(t)) }
    case ApplyAType(_,_) => throw new NotImplementedError("Generics not implemented (ApplyType): " + n)
    case WildAType(_) => throw new NotImplementedError("Type bounds not implemented (WildType): " + n)
  }

  def denoteType(e: AExp)(implicit env: Env): Scored[Type] = e match {
    case NameAExp(n) => typeScores(n)
    case x: ALit => fail("literals are not types.")
    case ParenAExp(x) => denoteType(x) flatMap { x => single(x, Pr.parensAroundType(x)) } // Java doesn't allow parentheses around types, but we do

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // first, the ones where x is a type
      val tdens = for {t <- denoteType(x)
                       fi <- typeFieldScores(t,f)}
                    yield (t,fi)
      val edens = for {e <- denoteExp(x)
                       fi <- typeFieldScores(typeOf(e),f)} // TODO: we cannot return only a type if the expression has side effects
                    yield (e,fi)
      (tdens flatMap { case(t,fi) => single(fi,Pr.typeFieldOfType(t,fi)) }) ++
        (edens flatMap { case(e,fi) => single(fi,Pr.typeFieldOfExp(e,fi)) })
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
  def containedIn(i: NamedItem, t: TypeItem): Boolean = i match {
    case f: Member => f.parent == t || containedIn(f.parent,t)
    case _ => false
  }

  def denoteField[ItemKind <: NamedItem with ClassMember,FieldDen <: Den]
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

    // we can always access this, static fields, or enums. Pretty-printing takes care of finding a proper name.
    case i: StaticFieldItem => single(StaticFieldExp(i), Pr.staticFieldValue)
    case i: EnumConstantItem => single(EnumConstantExp(i), Pr.enumConstantValue)
    case i: ThisItem => single(ThisExp(i), Pr.thisValue)

    case i: FieldItem if env.itemInScope(i) => single(LocalFieldExp(i), Pr.localFieldValue)
    case i: FieldItem => denoteField(i, FieldExp, Pr.superFieldValue, Pr.shadowedFieldValue, Pr.fieldValue)
    case _ => fail("Can't find a denotation for " + i + ", inaccessible")
  }

  def denoteCallable(e: AExp)(implicit env: Env): Scored[Callable] = e match {
    case NameAExp(n) => callableScores(n) flatMap {
      case i: MethodItem if env.itemInScope(i) => single(LocalMethodDen(i), Pr.localMethodCallable)
      case i: MethodItem => denoteField(i, MethodDen, Pr.superMethodCallable, Pr.shadowedMethodCallable, Pr.methodCallable)
      case i: StaticMethodItem => single(StaticMethodDen(i), Pr.staticMethodCallable)
      case i: ConstructorItem => single(NewDen(i), Pr.constructorCallable)
    }
    case ParenAExp(x) => bias(denoteCallable(x), Pr.parensAroundCallable) // Java doesn't allow parentheses around callables, but we do

    // x is either a type or an expression, f is a method, static method, or constructor
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // first, the ones where x is a type
      val tdens: Scored[Callable] = for {t <- denoteType(x)
                                         fi <- callableFieldScores(t,f)
                                         r <- (t,fi) match {
                                           case (_, f: StaticMethodItem) => single(StaticMethodDen(f), Pr.staticFieldCallable)
                                           case (_, f: MethodItem) => fail(show(fi)+" is not static, and is used without an object.")
                                           case (_, f: ConstructorItem) => single(NewDen(f), Pr.constructorFieldCallable)
                                         }}
                                      yield r
      val edens: Scored[Callable] = for {e <- denoteExp(x)
                                         fi <- callableFieldScores(typeOf(e),f)
                                         r <- (e,fi) match {
                                           case (_, f: StaticMethodItem) => single(StaticMethodDen(f), Pr.staticFieldCallableWithObject) // TODO: we cannot discard the expression if it has side effects
                                           case (_, f: MethodItem) => single(MethodDen(e,f), Pr.methodFieldCallable)
                                           case (_, f: ConstructorItem) => single(NewDen(f), Pr.constructorFieldCallableWithObject) // TODO: we cannot discard the expression if it has side effects
                                         }}
                                      yield r
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

  def denoteExp(e: AExp)(implicit env: Env): Scored[Exp] = e match {
    case NameAExp(n) => valueScores(n) flatMap denoteValue
    case x: ALit => denoteLit(x)
    case ParenAExp(x) => bias(denoteExp(x) map ParenExp, Pr.parenExp)

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // First, the ones where x is a type
      // TODO: penalize unnecessarily qualified field expressions?
      val tdens = denoteType(x) flatMap (t => staticFieldScores(t,f) flatMap {
        case f: EnumConstantItem => single(EnumConstantExp(f), Pr.enumFieldExp)
        case f: StaticFieldItem => single(StaticFieldExp(f), Pr.staticFieldExp)
      })
      // Now, x is an expression
      val edens = denoteExp(x) flatMap (e => fieldScores(typeOf(e),f) flatMap {
        case f: EnumConstantItem => single(EnumConstantExp(f), Pr.enumFieldExpWithObject) // TODO: cannot discard expression if there are side effects
        case f: StaticFieldItem => single(StaticFieldExp(f), Pr.staticFieldExpWithObject) // TODO: cannot discard expression if there are side effects
        case f: FieldItem => single(FieldExp(e,f), Pr.fieldExp)
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
      def call(f: Callable): Scored[Exp] = {
        product(xsl) flatMap { xl => bias(ArgMatching.fiddleArgs(f, xl), Pr.callExp(xsn, around)) }
      }
      def index(f: Exp, ft: Type): Scored[Exp] = {
        def hasDims(t: Type, d: Int): Boolean = d==0 || (t match {
          case ArrayType(t) => hasDims(t,d-1)
          case _ => false
        })
        if (!hasDims(ft,n)) fail(show(e)+s": expected >= $n dimensions, got ${dimensions(ft)}")
        else {
          val filtered = xsl map (_ flatMap {x => unbox(typeOf(x)) match {
            case Some(p: PrimType) if promote(p) == IntType => single(x, Pr.indexCallExp(xsn, around))
            case _ => fail(s"Index ${show(x)} doesn't convert to int")
          }})
          for (xl <- product(filtered)) yield xl.foldLeft(f)(IndexExp)
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

    case BinaryAExp(op,x,y) => product(denoteExp(x),denoteExp(y)) flatMap {case (x,y) => {
      val tx = typeOf(x)
      val ty = typeOf(y)
      if (binaryLegal(op,tx,ty)) single(BinaryExp(op,x,y), Pr.binaryExp)
      else fail("${show(e)}: invalid binary op ${show(tx)} ${show(op)} ${show(ty)}")
    }}

    case CastAExp(t,x) => product(denoteType(t),denoteExp(x)) flatMap {case (t,x) => {
      val tx = typeOf(x)
      if (castsTo(tx,t)) single(CastExp(t,x), Pr.castExp)
      else fail("${show(e)}: can't cast ${show(tx)} to ${show(t)}")
    }}

    case CondAExp(c,x,y) =>
      bias(product(denoteBool(c),denoteExp(x),denoteExp(y)) map {case (c,x,y) =>
           CondExp(c,x,y,condType(typeOf(x),typeOf(y)))}, Pr.condExp)

    case AssignAExp(op,x,y) => {
      product(denoteVariable(x),denoteExp(y)) flatMap {case (x,y) => {
        val tx = typeOf(x)
        val ty = typeOf(y)
        assignOpType(op,tx,ty) match {
          case None => fail(s"${show(e)}: invalid assignop ${show(tx)} ${show(token(op))} ${show(ty)}")
          case Some(t) => single(AssignExp(op,x,y), Pr.assignExp)
        }
      }}
    }

    case ArrayAExp(xs,a) =>
      bias({for (is <- product(xs.list map denoteExp))
             yield ArrayExp(condTypes(is map typeOf),is)}, Pr.arrayExp)

    case InstanceofAExp(x,t) => notImplemented
  }

  // Expressions with type restrictions
  def denoteBool(n: AExp)(implicit env: Env): Scored[Exp] = denoteExp(n) flatMap {e =>
    val t = typeOf(e)
    (t,toBoolean(t),toNumeric(t)) match {
      case (_,Some(_),_) => single(e, Pr.boolExp)
      case (_,None,Some(_)) => single(BinaryExp(NeOp, e, IntLit(0, "0")), Pr.insertComparison(t))
      // TODO: all sequences should probably check whether they're empty (or null)
      case (_:RefType,_,_) => single(BinaryExp(NeOp, e, NullLit), Pr.insertComparison(t))
      case _ => fail(s"${show(n)}: can't convert type ${show(t)} to boolean")
    }
  }
  def denoteNonVoid(n: AExp)(implicit env: Env): Scored[Exp] = denoteExp(n) flatMap {e =>
    if (typeOf(e) != VoidType) single(e, Pr.nonVoidExp)
    else fail(s"${show(n)}: expected non-void expression")
  }
  def denoteArray(e: AExp)(implicit env: Env): Scored[Exp] = denoteExp(e) flatMap {e => {
    val t = typeOf(e)
    if (t.isInstanceOf[ArrayType]) single(e, Pr.arrayTypeExp)
    else fail(s"${show(e)} has non-array type ${show(t)}")
  }}
  def denoteRef(e: AExp)(implicit env: Env): Scored[Exp] = denoteExp(e) flatMap {e => {
    val t = typeOf(e)
    if (t.isInstanceOf[RefType]) single(e, Pr.refExp)
    else fail(s"${show(e)} has non-reference type ${show(t)}")
  }}
  def denoteVariable(e: AExp)(implicit env: Env): Scored[Exp] = {
    denoteExp(e) flatMap { x =>
      if (isVariable(x)) single(x, Pr.variableExp)
      else fail("${show(e)}: ${show(x)} cannot be assigned to")
    }
  }


  def isVariable(e: Exp): Boolean = e match {
    // in java, we can only assign to actual variables, never to values returned by functions or expressions.
    // TODO: implement final, private, protected
    case _: Lit => false
    case ThisExp(_) => false
    case SuperExp(_) => false
    case ParameterExp(i) => true // TODO: check for final
    case LocalVariableExp(i) => true // TODO: check for final
    case EnumConstantExp(_) => false
    case CastExp(_,_) => false // TODO: java doesn't allow this, but I don't see why we shouldn't
    case UnaryExp(_,_) => false // TODO: java doesn't allow this, but we should. Easy for ++,--, and -x = 5 should translate to x = -5
    case BinaryExp(_,_,_) => false
    case AssignExp(_,_,_) => false
    case ParenExp(x) => isVariable(x)
    case ApplyExp(_,_,_) => false
    case FieldExp(obj, field) => true // TODO: check for final, private, protected
    case LocalFieldExp(field) => true // TODO: check for final
    case StaticFieldExp(field) => true // TODO: check for final, private, protected
    case IndexExp(a, i) => isVariable(a)
    case CondExp(_,_,_,_) => false // TODO: java doesn't allow this, but (x==5?x:y)=10 should be turned into an if statement
    case ArrayExp(_,_) => false
    case EmptyArrayExp(_,_) => false
  }

  // Statements
  def denoteStmt(s: AStmt)(env: Env): Scored[(Env,Stmt)] = {
    implicit val imp = env
    s match {
      case EmptyAStmt() => single((env,EmptyStmt), Pr.emptyStmt)
      case HoleAStmt() => single((env,HoleStmt), Pr.holeStmt)
      case VarAStmt(mod,t,ds) =>
        if (mod.nonEmpty) notImplemented
        else denoteType(t)(env).flatMap(t => {
          def init(t: Type, v: Name, i: Option[AExp], env: Env): Scored[Option[Exp]] = i match {
            case None => single(None, Pr.varInitNone)
            case Some(e) => denoteExp(e)(env) flatMap {e =>
              if (assignsTo(e,t)) single(Some(e), Pr.varInit)
              else fail(s"${show(s)}: can't assign ${show(e)} to type ${show(t)} in declaration of $v}")
            }
          }
          def define(env: Env, ds: List[AVarDecl]): Scored[(Env,List[VarDecl])] = ds match {
            case Nil => single((env,Nil), Pr.varDeclNil)
            case (v,k,i)::ds =>
              val tk = arrays(t,k)
              product(env.newVariable(v,tk),init(tk,v,i,env)) flatMap {case ((env,v),i) =>
                define(env,ds) flatMap {case (env,ds) => single((env,(v,k,i)::ds), Pr.varDecl)}}
          }
          val st = safe(t)
          if (st.isDefined)
            define(env,ds.list) flatMap {case (env,ds) => single((env,VarStmt(st.get,ds)), Pr.varStmt) }
          else
            fail(s"cannot make variables of type $t.")
        })
      case ExpAStmt(e) => {
        val exps = denoteExp(e) flatMap { e => single((env,ExpStmt(e)), Pr.expStmt) }
        val stmts = e match {
          case AssignAExp(None,NameAExp(x),y) => denoteExp(y) flatMap {y => safe(typeOf(y)) match {
            case Some(t) => env.newVariable(x,t) flatMap { case (env,x) => single((env,VarStmt(t,List((x,0,Some(y))))), Pr.assignmentAsVarStmt) }
            case None => fail(s"expression $y does not return anything usable (${typeOf(y)})")
          }}
          case _ => fail(show(e)+": expression doesn't look like a statement")
        }
        exps ++ stmts
      }
      case BlockAStmt(b) => denoteStmts(b)(env) flatMap {case (e,ss) => single((e,BlockStmt(ss)), Pr.blockStmt)}
      case AssertAStmt(c,m) => productWith(denoteBool(c),thread(m)(denoteNonVoid)){case (c,m) =>
        (env,AssertStmt(c,m))}.flatMap( single(_, Pr.assertStmt) )

      // TODO: make sure the labels are valid
      case BreakAStmt(lab) =>
        if (env.inside_breakable) denoteLabel(lab,(env,BreakStmt)).flatMap( single(_, Pr.breakStmt))
        else fail("cannot break outside of a loop or switch statement.")
      case ContinueAStmt(lab) =>
        if (env.inside_continuable) denoteLabel(lab,(env,ContinueStmt)).flatMap( single(_, Pr.continueStmt))
        else fail("cannot break outside of a loop")
      case ReturnAStmt(e) => product(returnType,thread(e)(denoteExp)) flatMap {case (r,e) =>
        val t = typeOf(e)
        if (assignsTo(e,r)) single((env,ReturnStmt(e)), Pr.returnStmt)
        else fail(s"${show(s)}: type ${show(t)} incompatible with return type ${show(r)}")
      }
      case ThrowAStmt(e) => denoteExp(e) flatMap {e =>
        val t = typeOf(e)
        if (isThrowable(t)) single((env,ThrowStmt(e)), Pr.throwStmt)
        else fail(s"${show(s)}: type $t is not throwable")
      }
      case SyncAStmt(e,b) => product(denoteRef(e),denoteScoped(b)(env)) flatMap {
        case (e,(env,b)) => single((env,SyncStmt(e,b)), Pr.syncStmt) }
      case IfAStmt(c,x) => product(denoteBool(c),denoteScoped(x)(env)) flatMap {
        case (c,(env,x)) => single((env,IfStmt(c,x)), Pr.ifStmt) }
      case IfElseAStmt(c,x,y) => product(denoteBool(c),denoteScoped(x)(env)) flatMap {case (c,(env,x)) =>
        denoteScoped(y)(env) flatMap {case (env,y) => single((env,IfElseStmt(c,x,y)), Pr.ifElseStmt) }}
      case WhileAStmt(c,s,flip) => product(denoteBool(c),denoteScoped(s)(env)) flatMap {case (c,(env,s)) =>
        single((env,WhileStmt(xor(flip,c),s)), Pr.whileStmt) }
      case DoAStmt(s,c,flip) => product(denoteScoped(s)(env),denoteBool(c)) flatMap {case ((env,s),c) =>
        single((env,DoStmt(s,xor(flip,c))), Pr.doStmt) }
      case ForAStmt(i,c,u,s) => denoteStmts(i)(env.pushScope) flatMap {case (env,i) =>
        product(thread(c)(denoteBool(_)(env)),thread(u)(denoteExp(_)(env)),denoteScoped(s)(env)) flatMap {case (c,u,(env,s)) =>
          // Sanitize initializer into valid Java
          i match {
            case List(i:VarStmt) => single((env.popScope, ForStmt(i,c,u,s)), Pr.forStmt)
            case _ => allSome(i map {case ExpStmt(e) => Some(e); case _ => None}) match {
              case Some(es) => single((env.popScope, ForStmt(ForExps(es),c,u,s)), Pr.expForStmt)
              case None => single((env.popScope, BlockStmt(i:::List(ForStmt(ForExps(Nil),c,u,s)))), Pr.blockForStmt)
            }
          }
        }
      }
      case ForeachAStmt(t,v,n,e,s) => {
        def hole = show(ForeachAStmt(t,v,n,e,HoleAStmt()))
        product(thread(t)(denoteType),denoteExp(e)) flatMap {case (t,e) =>
          val tc = typeOf(e)
          isIterable(tc) match {
            case None => fail(s"${show(e)}: type ${show(tc)} is not Iterable or an Array")
            case Some(te) =>
              (t match {
                case Some(t) =>
                  val ta = arrays(t,n)
                  if (typeAssignsTo(te,ta)) single(ta, Pr.forEachArray)
                  else fail(s"$hole: can't assign ${show(te)} to ${show(ta)}")
                case None =>
                  val ne = dimensions(te)
                  if (ne >= n) single(te, Pr.forEachArrayNoType)
                  else fail(s"$hole: expected $n array dimensions, got type ${show(te)} with $ne")
              }) flatMap (t => env.pushScope.newVariable(v,t) flatMap {case (env,v) => denoteStmt(s)(env) map {case (env,s) =>
                (env.popScope,ForeachStmt(t,v,e,s))
              }})
          }
        }
      }
    }
  }

  def xor(x: Boolean, y: Exp): Exp =
    if (x) UnaryExp(NotOp,y) else y

  def denoteLabel[A](lab: Option[Name], x: => A): Scored[A] = lab match {
    case None => single(x, Pr.labelNone)
    case Some(_) => notImplemented
  }

  def denoteStmts(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    bias(productFoldLeft(env)(s map denoteStmt), Pr.stmtList)

  // Statement whose environment is discarded
  def denoteScoped(s: AStmt)(env: Env): Scored[(Env,Stmt)] =
    denoteStmt(s)(env.pushScope) map {case (env,s) => (env.popScope,s)}
  def denoteScoped(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    denoteStmts(s)(env.pushScope) map {case (env,s) => (env.popScope,s)}
}
