package tarski

import org.apache.commons.lang.StringEscapeUtils.unescapeJava

import AST._
import Types._
import Environment._
import Items._
import Denotations._
import Scores._
import tarski.Base.IterableItem
import tarski.Tokens._
import Pretty._
import ambiguity.Utility._

object Semantics {
  /*
  // TODO: Add back bias?
  // Bias score for a AST tree node (given an instance), given the environment
  def bias(node: Node)(implicit env: JavaEnvironment): Score = node match {
    case _ => ZeroScore
  }
  def withBias[A](n: Node, ds: Scored[A]): Scored[A] = {
    println("      scores for " + n + ": " + ds)
    ds
  }
  */

  // Literals
  def denoteLit(x: ALit): Scored[Lit] = {
    def under(v: String): String = v.replaceAllLiterally("_","")
    def f[A,B](v: String, c: String => A)(t: (A,String) => B) = t(c(under(v)),v)
    single(x match {
      case IntALit(v) =>
        val n = under(v).toLong
        val i = n.toInt
        if (i == n) IntLit(i,v) else LongLit(n,v+'L')
      case LongALit(v) =>   f(v,_.dropRight(1).toLong)(LongLit)
      case FloatALit(v) =>  f(v,_.toFloat)(FloatLit)
      case DoubleALit(v) => f(v,_.toDouble)(DoubleLit)
      case BoolALit(b) =>   BooleanLit(b)
      case CharALit(v) =>   CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0),v)
      case StringALit(v) => StringLit(unescapeJava(v.slice(1,v.size-1)),v)
      case NullALit() =>    NullLit
    })
  }

  // Types
  def denoteType(n: AType)(implicit env: Env): Scored[Type] = n match {
    case NameAType(n) => typeScores(n)
    case VoidAType() => single(VoidType)
    case PrimAType(t) => single(t)
    case FieldAType(x,f) => for (t <- denoteType(x); fi <- typeFieldScores(t,f)) yield fi
    case ModAType(Annotation(_),t) => throw new NotImplementedError("Types with annotations")
    case ModAType(_,_) => fail("Not implemented: type modifiers")
    case ArrayAType(t) => denoteType(t) map ArrayType
    case ApplyAType(_,_) => throw new NotImplementedError("Generics not implemented (ApplyType): " + n)
    case WildAType(_) => throw new NotImplementedError("Type bounds not implemented (WildType): " + n)
  }

  def denoteType(e: AExp)(implicit env: Env): Scored[Type] = e match {
    case NameAExp(n) => typeScores(n)
    case x: ALit => fail("literals are not types.")
    case ParenAExp(x) => denoteType(x) // Java doesn't allow parentheses around types, but we do

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // first, the ones where x is a type
      val tdens: Scored[Type] = for {t <- denoteType(x)
                                    fi <- typeFieldScores(t,f)}
                                 yield fi
      val edens: Scored[Type] = for {e <- denoteExp(x)
                                     fi <- typeFieldScores(typeOf(e),f)}
                                 yield fi
      tdens++edens
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
  }

  // Are we contained in the given type, or in something contained in the given type?
  def containedIn(i: NamedItem, t: TypeItem): Boolean = i match {
    case f: Member => f.container == t || containedIn(f.container,t)
    case _ => false
  }

  def denoteValue(i: Value)(implicit env: Env): Scored[Exp] = i match {
    case i: ParameterItem if env.itemInScope(i) => single(ParameterExp(i))
    case i: LocalVariableItem if env.itemInScope(i) => single(LocalVariableExp(i))

    // we can always access this, static fields, or enums. Pretty-printing takes care of finding a proper name.
    case i: StaticFieldItem => single(StaticFieldExp(i))
    case i: EnumConstantItem => single(EnumConstantExp(i))
    case i: ThisItem => single(ThisExp(i))

    case i: FieldItem =>
      if (env.itemInScope(i))
        single(LocalFieldExp(i))
      else {
        val c = i.container
        objectsOfItem(c).flatMap(x =>
          if (containedIn(x,i.container))
            fail(s"Field ${show(i)}: all objects of item ${show(c)} contained in ${show(c)}")
          else
            denoteValue(x).flatMap( xd =>
              if (shadowedInSubType(i, typeOf(xd).asInstanceOf[RefType])) {
                xd match {
                  case ThisExp(ThisItem(tt:ClassItem)) if toItem(tt.base) == Some(c) => single(FieldExp(SuperExp(ThisItem(tt)),i))
                  case _ => single(FieldExp(CastExp(toType(c,Nil),xd),i))
                }
              } else {
                single(FieldExp(xd,i))
              }
            ))
      }
    case _ => fail("Can't find a denotation for " + i + ", inaccessible")
  }

  def denoteArray(e: AExp)(implicit env: Env): Scored[Exp] = denoteExp(e) flatMap {
    case a if typeOf(a).isInstanceOf[ArrayType] => single(a)
    case d => fail(show(d) + " is not an array")
  }

  def denoteCallable(e: AExp)(implicit env: Env): Scored[Callable] = e match {
    case NameAExp(n) => callableScores(n) flatMap {
      // Callables
      // TODO: take proper care of scoping and shadowing here
      case i: MethodItem =>
        if (env.itemInScope(i))
          single(LocalMethodDen(i))
        else
          for (obj <- objectsOfItem(i.container); obj <- denoteValue(obj)) yield MethodDen(obj, i)
      case i: StaticMethodItem => single(StaticMethodDen(i))
      case i: ConstructorItem => single(NewDen(i))
    }
    case ParenAExp(x) => denoteCallable(x) // Java doesn't allow parentheses around callables, but we do

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // first, the ones where x is a type
      val tdens: Scored[Callable] = for {t <- denoteType(x)
                                         fi <- callableFieldScores(t,f)
                                         r <- (t,fi) match {
                                           case (_, f: StaticMethodItem) => single(StaticMethodDen(f))
                                           case (_, f: MethodItem) => fail(show(fi)+" is not static, and is used without an object.")
                                           case (_, f: ConstructorItem) => throw new NotImplementedError("FieldExp: ConstructorItem")
                                         }}
                                      yield r
      val edens: Scored[Callable] = for {e <- denoteExp(x)
                                         fi <- callableFieldScores(typeOf(e),f)
                                         r <- (e,fi) match {
                                           case (_, f: StaticMethodItem) => single(StaticMethodDen(f))
                                           case (_, f: MethodItem) => single(MethodDen(e,f))
                                           case (_, f: ConstructorItem) => throw new NotImplementedError("FieldExp: ConstructorItem")
                                         }}
                                      yield r
      tdens++edens
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
  }

  def denoteExp(e: AExp)(implicit env: Env): Scored[Exp] = e match {
    case NameAExp(n) => valueScores(n) flatMap denoteValue
    case x: ALit => denoteLit(x)
    case ParenAExp(x) => denoteExp(x) map ParenExp

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else {
      // First, the ones where x is a type
      val tdens = denoteType(x) flatMap (t => staticFieldScores(t,f) flatMap {
        case f: EnumConstantItem => single(EnumConstantExp(f))
        case f: StaticFieldItem => single(StaticFieldExp(f))
      })
      // Now, x is an expression
      val edens = denoteExp(x) flatMap (e => fieldScores(typeOf(e),f) flatMap {
        case f: EnumConstantItem => single(EnumConstantExp(f))
        case f: StaticFieldItem => single(StaticFieldExp(f))
        case f: FieldItem => single(FieldExp(e,f))
      })
      tdens++edens
    }

    case MethodRefAExp(x,ts,f) => throw new NotImplementedError("MethodRefs not implemented: " + e)
    case NewRefAExp(x,t) => throw new NotImplementedError("NewRef not implemented: " + e)
    case TypeApplyAExp(x,ts) => throw new NotImplementedError("Generics not implemented (TypeApplyExp): " + e)
    case NewAExp(ts,e) => throw new NotImplementedError("new expression not implemented: " + e)
    case WildAExp(b) => throw new NotImplementedError("wildcard expressions not implemented: " + e)

    case ApplyAExp(f,xsn,_) => {
      val xsl = xsn.list map denoteExp
      val n = xsl.size
      def call(f: Callable): Scored[Exp] = {
        val fn = f.params.size
        if (fn != n) fail(show(f)+s": expected $fn arguments (${show(CommaList(f.params))}), got $n ($xsn)")
        else product(xsl) flatMap { xl => resolve(List(f),xl map typeOf) match {
          case None => fail(show(f)+": params "+show(tokensSig(f))
                            +" don't match arguments "+show(CommaList(xl))+" with types "+show(CommaList(xl map typeOf)))
          case Some((_,ts)) => single(ApplyExp(f,ts,xl))
        }}
      }
      def index(f: Exp, ft: Type): Scored[Exp] = {
        def hasDims(t: Type, d: Int): Boolean = d==0 || (t match {
          case ArrayType(t) => hasDims(t,d-1)
          case _ => false
        })
        if (!hasDims(ft,n)) fail(show(e)+s": expected >= $n dimensions, got ${dimensions(ft)}")
        else {
          val filtered = xsl map (_ flatMap {x => unbox(typeOf(x)) match {
            case Some(p: PrimType) if promote(p) == IntType => single(x)
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
      case x if unaryLegal(op,typeOf(x)) => single(UnaryExp(op,x))
      case x => fail(s"${show(e)}: invalid unary ${show(token(op))} on type ${show(typeOf(x))}")
    }

    case BinaryAExp(op,x,y) => product(denoteExp(x),denoteExp(y)) flatMap {case (x,y) => {
      val tx = typeOf(x)
      val ty = typeOf(y)
      if (binaryLegal(op,tx,ty)) single(BinaryExp(op,x,y))
      else fail("${show(e)}: invalid binary op ${show(tx)} ${show(op)} ${show(ty)}")
    }}

    case CastAExp(t,x) => product(denoteType(t),denoteExp(x)) flatMap {case (t,x) => {
      val tx = typeOf(x)
      if (castsTo(tx,t)) single(CastExp(t,x))
      else fail("${show(e)}: can't cast ${show(tx)} to ${show(t)}")
    }}

    case CondAExp(c,x,y) =>
      product(denoteBool(c),denoteExp(x),denoteExp(y)) map {case (c,x,y) =>
        CondExp(c,x,y,condType(typeOf(x),typeOf(y)))}

    case AssignAExp(op,x,y) => {
      val xx = denoteExp(x) flatMap {x =>
        if (isVariable(x)) single(x)
        else fail("${show(e)}: ${show(x)} cannot be assigned to")
      }
      product(xx,denoteExp(y)) flatMap {case (x,y) => {
        val tx = typeOf(x)
        val ty = typeOf(y)
        assignOpType(op,tx,ty) match {
          case None => fail(s"${show(e)}: invalid assignop ${show(tx)} ${show(token(op))} ${show(ty)}")
          case Some(t) => single(AssignExp(op,x,y))
        }
      }}
    }

    case ArrayAExp(xs,a) =>
      for (is <- product(xs.list map denoteExp))
        yield ArrayExp(condTypes(is map typeOf),is)
}

  // Expressions with type restrictions
  def denoteBool(n: AExp)(implicit env: Env): Scored[Exp] = denoteExp(n) flatMap {e =>
    val t = typeOf(e)
    if (isToBoolean(t)) single(e)
    else fail(s"${show(n)}: can't convert type ${show(t)} to boolean")
  }
  def denoteNonVoid(n: AExp)(implicit env: Env): Scored[Exp] = denoteExp(n) flatMap {e =>
    if (typeOf(e) != VoidType) single(e)
    else fail(s"${show(n)}: expected non-void expression")
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
      case EmptyAStmt() => single((env,EmptyStmt))
      case HoleAStmt() => single((env,HoleStmt))
      case VarAStmt(mod,t,ds) =>
        if (mod.nonEmpty) notImplemented
        else denoteType(t)(env).flatMap(t => {
          def init(t: Type, v: Name, i: Option[AExp], env: Env): Scored[Option[Exp]] = i match {
            case None => single(None)
            case Some(e) => denoteExp(e)(env) flatMap {e =>
              if (assignsTo(typeOf(e),t)) single(Some(e))
              else fail(s"${show(s)}: can't assign ${show(e)} to type ${show(t)} in declaration of $v}")
            }
          }
          def define(env: Env, ds: List[AVarDecl]): Scored[(Env,List[VarDecl])] = ds match {
            case Nil => single((env,Nil))
            case (v,k,i)::ds =>
              val tk = arrays(t,k)
              product(env.newVariable(v,tk),init(tk,v,i,env)) flatMap {case ((env,v),i) =>
                define(env,ds) map {case (env,ds) => (env,(v,k,i)::ds)}}
          }
          val st = safe(t)
          define(env,ds.list) map {case (env,ds) => (env,VarStmt(st,ds))}
        })
      case ExpAStmt(e) => {
        val exps = denoteExp(e) map ExpStmt
        val stmts = e match {
          case AssignAExp(None,NameAExp(x),y) => denoteExp(y) flatMap {y => {
            val t = safe(typeOf(y))
            env.newVariable(x,t) map {case (env,x) => (env,VarStmt(t,List((x,0,Some(y)))))}
          }}
          case _ => fail(show(e)+": expression doesn't look like a statement")
        }
        exps.map((env,_)) ++ stmts
      }
      case BlockAStmt(b) => denoteStmts(b)(env) map {case (e,ss) => (e,BlockStmt(ss))}
      case AssertAStmt(c,m) => productWith(denoteBool(c),thread(m)(denoteNonVoid)){case (c,m) =>
        (env,AssertStmt(c,m))}
      case BreakAStmt(lab) => denoteLabel(lab,(env,BreakStmt))
      case ContinueAStmt(lab) => denoteLabel(lab,(env,ContinueStmt))
      case ReturnAStmt(e) => product(returnType,thread(e)(denoteExp(_))) flatMap {case (r,e) =>
        val t = typeOf(e)
        if (assignsTo(t,r)) single((env,ReturnStmt(e)))
        else fail(s"${show(s)}: type ${show(t)} incompatible with return type ${show(r)}")
      }
      case ThrowAStmt(e) => denoteExp(e) flatMap {e =>
        val t = typeOf(e)
        if (isThrowable(t)) single((env,ThrowStmt(e)))
        else fail(s"${show(s)}: type $t is not throwable")
      }
      case SyncAStmt(e,b) => notImplemented
      case IfAStmt(c,x) => product(denoteBool(c),denoteScoped(x)(env)) map {case (c,(env,x)) => (env,IfStmt(c,x))}
      case IfElseAStmt(c,x,y) => product(denoteBool(c),denoteScoped(x)(env)) flatMap {case (c,(env,x)) =>
        denoteScoped(y)(env) map {case (env,y) => (env,IfElseStmt(c,x,y))}}
      case WhileAStmt(c,s,flip) => product(denoteBool(c),denoteScoped(s)(env)) map {case (c,(env,s)) =>
        (env,WhileStmt(xor(flip,c),s))}
      case DoAStmt(s,c,flip) => product(denoteScoped(s)(env),denoteBool(c)) map {case ((env,s),c) =>
        (env,DoStmt(s,xor(flip,c)))}
      case ForAStmt(i,c,u,s) => denoteStmts(i)(env.pushScope) flatMap {case (env,i) =>
        product(thread(c)(denoteBool(_)(env)),thread(u)(denoteExp(_)(env)),denoteScoped(s)(env)) map {case (c,u,(env,s)) =>
          // Sanitize initializer into valid Java
          (env.popScope, i match {
            case List(i:VarStmt) => ForStmt(i,c,u,s)
            case i => allSome(i map {case ExpStmt(e) => Some(e); case _ => None}) match {
              case Some(es) => ForStmt(ForExps(es),c,u,s)
              case None => BlockStmt(i:::List(ForStmt(ForExps(Nil),c,u,s)))
            }
          })
        }
      }
      case ForeachAStmt(t,v,n,e,s) => {
        def hole = show(ForeachAStmt(t,v,n,e,HoleAStmt()))
        product(thread(t)(denoteType(_)),denoteExp(e)) flatMap {case (t,e) =>
          val tc = typeOf(e)
          isIterable(tc) match {
            case None => fail(s"${show(e)}: type ${show(tc)} is not Iterable or an Array")
            case Some(te) =>
              (t match {
                case Some(t) =>
                  val ta = arrays(t,n)
                  if (assignsTo(te,ta)) single(ta)
                  else fail(s"$hole: can't assign ${show(te)} to ${show(ta)}")
                case None =>
                  val ne = dimensions(te)
                  if (ne >= n) single(te)
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
    if (x) UnaryExp(NotOp(),y) else y

  def denoteLabel[A](lab: Option[Name], x: => A): Scored[A] = lab match {
    case None => single(x)
    case Some(_) => notImplemented
  }

  def denoteStmts(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    productFoldLeft(env)(s map denoteStmt)

  // Statement whose environment is discarded
  def denoteScoped(s: AStmt)(env: Env): Scored[(Env,Stmt)] =
    denoteStmt(s)(env.pushScope) map {case (env,s) => (env.popScope,s)}
  def denoteScoped(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    denoteStmts(s)(env.pushScope) map {case (env,s) => (env.popScope,s)}
}
