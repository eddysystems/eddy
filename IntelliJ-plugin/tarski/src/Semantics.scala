package tarski

import org.apache.commons.lang.StringEscapeUtils.unescapeJava

import AST._
import Types._
import Environment._
import Items._
import Denotations._
import Scores._
import Tokens.show
import Pretty._
import ambiguity.Utility.notImplemented

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
    def f[A,B](v: String, c: String => A)(t: (A,String) => B) = t(c(v.replaceAllLiterally("_","")),v)
    single(x match {
      case IntALit(v) =>    f(v,_.toInt)(IntLit)
      case LongALit(v) =>   f(v,_.toLong)(LongLit)
      case FloatALit(v) =>  f(v,_.toFloat)(FloatLit)
      case DoubleALit(v) => f(v,_.toDouble)(DoubleLit)
      case BoolALit(b) =>   BooleanLit(b)
      case CharALit(v) =>   CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0),v)
      case StringALit(v) => StringLit(unescapeJava(v.slice(1,v.size-1)),v)
      case NullALit() =>    NullLit()
    })
  }

  // Types
  // TODO: The grammar currently rules out stuff like (1+2).A matching as a type.  Maybe we want this?
  def denote(n: AType)(implicit env: Env): Scored[Type] = n match {
    case NameAType(n) => typeScores(n)
    case VoidAType() => single(VoidType)
    case PrimAType(t) => single(t)
    case FieldAType(x,f) => for (t <- denote(x); fi <- typeFieldScores(t,f)) yield fi
    case ModAType(Annotation(_),t) => throw new NotImplementedError("Types with annotations")
    case ModAType(_,_) => fail("Not implemented: type modifiers")
    case ArrayAType(t) => denote(t) map ArrayType
    case ApplyAType(_,_) => throw new NotImplementedError("Generics not implemented (ApplyType): " + n)
    case WildAType(_) => throw new NotImplementedError("Type bounds not implemented (WildType): " + n)
  }

  def denoteType(n: AExp)(implicit env: Env): Scored[Type] =
    denote(n) flatMap {
      case t: TypeDen => single(t.item)
      case e: Exp => single(typeOf(e)) // Allow expressions to be used as types
      case d => fail(show(d)+": has no type")
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

    // TODO: take proper care of scoping and shadowing here
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

  // Expressions
  def denote(e: AExp)(implicit env: Env): Scored[Den] = e match {
    case NameAExp(n) => scores(n) flatMap {
      case i: Value => denoteValue(i)

      // Callables
      // TODO: take proper care of scoping and shadowing here
      case i: MethodItem =>
        if (env.itemInScope(i))
          single(LocalMethodDen(i))
        else
          for (obj <- objectsOfItem(i.container); obj <- denoteValue(obj)) yield MethodDen(obj, i)

      case i: StaticMethodItem => single(StaticMethodDen(i))
      case i: ConstructorItem => single(NewDen(i))
      case i: TypeParamItem => single(TypeDen(ParamType(i)))
      case _: TypeItem => fail("Expression cannot return types")
      case _: PackageItem => notImplemented
      case _: AnnotationItem => notImplemented
    }
    case x: ALit => denoteLit(x)
    case ParenAExp(x) => denote(x) // Java doesn't allow parentheses around types, but we do

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldAExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else
      for (d <- denote(x);
           t <- d match {
             case t: TypeDen => single(t.item)
             case e: Exp => single(typeOf(e))
             case _ => fail(show(d)+" has no type")
           };
           fi <- fieldScores(t,f);
           r <- (d,fi) match {
             case (_, f: TypeItem) => single(TypeDen(toType(f,Nil)))
             case (_, f: EnumConstantItem) => single(EnumConstantExp(f))
             case (_, f: StaticFieldItem) => single(StaticFieldExp(f))
             case (e: Exp, f: FieldItem) => single(FieldExp(e,f))
             case (_, f: StaticMethodItem) => single(StaticMethodDen(f))
             case (e: Exp, f: MethodItem) => single(MethodDen(e,f))
             case (_, f: PackageItem) => throw new NotImplementedError("FieldExp: packages not implemented: " + e)
             case (_, f: ConstructorItem) => throw new NotImplementedError("FieldExp: ConstructorItem")
             case (_, f: LocalItem) => fail("Can't qualify locals, parameters") // TODO: return something with a low score
             case _ => fail(show(fi)+" is not field-like")
           })
        yield r

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
      def index(f: Exp, ft: ArrayType): Scored[Exp] = {
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
      denote(f) flatMap {
        case a: Exp => typeOf(a) match {
          case t: ArrayType => index(a,t)
          case _ => fail(show(f)+": not an array")
        }
        case c: Callable => call(c)
        case _ => fail(show(f)+": neither callable nor an array")
      }
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

    case CastAExp(t,x) => product(denote(t),denoteExp(x)) flatMap {case (t,x) => {
      val tx = typeOf(x)
      if (castsTo(tx,t)) single(CastExp(t,x))
      else fail("${show(e)}: can't cast ${show(tx)} to ${show(t)}")
    }}

    case CondAExp(c,x,y) => {
      val cc = denoteExp(c) flatMap {c =>
        if (isToBoolean(typeOf(c))) single(c)
        else fail("${show(c)}: can't convert to boolean")
      }
      product(cc,denoteExp(x),denoteExp(y)) map {case (c,x,y) =>
        CondExp(c,x,y,condType(typeOf(x),typeOf(y)))}
    }

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

  def denoteExp(n: AExp)(implicit env: Env): Scored[Exp] =
    denote(n) flatMap {
      case e: Exp => single(e)
      case d => fail("${show(n)}: ${show(d)} isn't an expression")
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
  def denoteStmt(s: AStmt)(env: Env): Scored[(Env,Stmt)] = s match {
    case EmptyAStmt() => single((env,EmptyStmt()))
    case VarAStmt(mod,t,ds) =>
      if (mod.nonEmpty) notImplemented
      else denote(t)(env).flatMap(t => {
        def init(t: Type, v: Name, i: Option[AExp], env: Env): Scored[Option[Exp]] = i match {
          case None => single(None)
          case Some(e) => denoteExp(e)(env) flatMap {e =>
            if (assignsTo(typeOf(e),t)) single(Some(e))
            else {
              implicit val imp = env // Make env available for show
              fail(s"${show(s)}: can't assign ${show(e)} to type ${show(t)} in declaration of $v}")
            }
          }
        }
        def define(env: Env, ds: List[AVarDecl]): Scored[(Env,List[VarDecl])] = ds match {
          case Nil => single((env,Nil))
          case (v,k,i)::ds =>
            val tk = arrays(t,k)
            product(env.newVariable(v,tk),init(tk,v,i,env)) flatMap {case ((env,v),i) =>
              define(env,ds) map {case (env,ds) => (env,(v,k,i)::ds)}}
        }
        define(env,ds.list) map {case (env,ds) => (env,VarStmt(t,ds))}
      })
    case ExpAStmt(e) => {
      val exps = denoteExp(e)(env) map ExpStmt
      val stmts = e match {
        case AssignAExp(None,NameAExp(x),y) => denoteExp(y)(env) flatMap {y => {
          val t = typeOf(y)
          env.newVariable(x,t) map {case (env,x) => (env,VarStmt(t,List((x,0,Some(y)))))}
        }}
        case _ => fail(show(e)+": expression doesn't look like a statement")
      }
      exps.map((env,_)) ++ stmts
    }
    case BlockAStmt(b) => denoteStmts(b)(env) map {case (e,ss) => (e,BlockStmt(ss))}
    case AssertAStmt(cond: AExp, msg: Option[AExp]) => notImplemented
    case BreakAStmt(label: Option[Name]) => notImplemented
    case ContinueAStmt(label: Option[Name]) => notImplemented
    case ReturnAStmt(e: Option[AExp]) => notImplemented
    case ThrowAStmt(e: AExp) => notImplemented
    case SyncAStmt(e: AExp, b: Block) => notImplemented
  }

  def denoteStmts(s: List[AStmt])(env: Env): Scored[(Env,List[Stmt])] =
    productFoldLeft(env)(s map denoteStmt)
}
