package tarski

import org.apache.commons.lang.StringEscapeUtils.unescapeJava

import AST.{Type => _, ArrayType => _, _}
import Types._
import Environment._
import Items._
import tarski.Denotations._
import Scores._
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
  def denote(x: Lit): Scored[LitDen] = {
    def f[A,B](v: String, c: String => A)(t: (A,String) => B) = t(c(v.replaceAllLiterally("_","")),v)
    single(x match {
      case AST.IntLit(v) =>    f(v,_.toInt)(Denotations.IntLit)
      case AST.LongLit(v) =>   f(v,_.toLong)(Denotations.LongLit)
      case AST.FloatLit(v) =>  f(v,_.toFloat)(Denotations.FloatLit)
      case AST.DoubleLit(v) => f(v,_.toDouble)(Denotations.DoubleLit)
      case AST.BoolLit(b) =>   Denotations.BooleanLit(b)
      case AST.CharLit(v) =>   Denotations.CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0),v)
      case AST.StringLit(v) => Denotations.StringLit(unescapeJava(v.slice(1,v.size-1)),v)
      case AST.NullLit() =>    Denotations.NullLit()
    })
  }

  // Types
  // TODO: The grammar currently rules out stuff like (1+2).A matching as a type.  Maybe we want this?
  def denote(n: AST.Type)(implicit env: Env): Scored[Type] = n match {
    case NameType(n) => typeScores(n)
    case FieldType(x,f) => for (t <- denote(x); fi <- typeFieldScores(t,f)) yield fi
    case ModType(Annotation(_),t) => throw new NotImplementedError("Types with annotations")
    case ModType(_,_) => fail
    case AST.ArrayType(t) => denote(t) map ArrayType
    case ApplyType(_,_) => throw new NotImplementedError("Generics not implemented (ApplyType): " + n)
    case WildType(_) => throw new NotImplementedError("Type bounds not implemented (WildType): " + n)
  }

  def denoteType(n: Exp)(implicit env: Env): Scored[Type] =
    denote(n) collect {
      case t: TypeDen => t.item
      case e: ExpDen => typeOf(e) // Allow expressions to be used as types
    }

  // TODO: this should not rely on string matching.
  def isLocal(i: Items.NamedItem): Boolean = i.name == i.relativeName || i.relativeName == "this" || i.relativeName == "super"

  def denoteValue(i: Items.Value)(implicit env: Env): Scored[ExpDen] = i match {
    case i: ParameterItem => single(ParameterExpDen(i))
    case i: LocalVariableItem => single(LocalVariableExpDen(i))
    case i: EnumConstantItem => single(EnumConstantExpDen(i))
    case i: FieldItem =>
      if (isLocal(i))
        single(LocalFieldExpDen(i))
      else
        for (obj <- objectsOfType("", i.containing); objden <- denoteValue(obj)) yield FieldExpDen(objden, i)
    case i: StaticFieldItem => single(StaticFieldExpDen(i))
  }

  // Expressions
  def denote(e: Exp)(implicit env: Env): Scored[Den] = e match {
    case NameExp(n) => scores(n) flatMap {
      case i: Value => denoteValue(i)

      // callables
      case i: MethodItem =>
        if (isLocal(i))
          single(LocalMethodDen(i))
        else
          for (obj <- objectsOfType("", i.containing); objden <- denoteValue(obj)) yield MethodDen(objden, i)
      case i: StaticMethodItem => single(Denotations.StaticMethodDen(i))
      case i: ConstructorItem => single(Denotations.ForwardDen(i))
      case _: Type => fail // Expression cannot return types
      case PackageItem(_,_,_) => notImplemented
      case AnnotationItem(_,_,_) => notImplemented
    }
    case LitExp(x) => denote(x)
    case ParenExp(x) => denote(x) // Java doesn't allow parentheses around types, but we do

    // x is either a type or an expression, f is an inner type, method, or field
    case FieldExp(x,ts,f) => if (ts.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + e) else
      for (d <- denote(x);
           t <- d match {
             case t: TypeDen => single(t.item)
             case e: ExpDen => single(typeOf(e))
             case _ => fail
           };
           fi <- fieldScores(t,f);
           r <- (d,fi) match {
             case (_, f: Items.Type) => single(TypeDen(f))
             case (_, f: Items.EnumConstantItem) => single(EnumConstantExpDen(f))
             case (_, f: Items.StaticFieldItem) => single(StaticFieldExpDen(f))
             case (e: ExpDen, f: Items.FieldItem) => single(FieldExpDen(e,f))
             case (_, f: Items.StaticMethodItem) => single(StaticMethodDen(f))
             case (e: ExpDen, f: Items.MethodItem) => single(MethodDen(e,f))
             case (_, f: Items.PackageItem) => throw new NotImplementedError("FieldExp: packages not implemented: " + e)
             case (_, f: Items.ConstructorItem) => throw new NotImplementedError("FieldExp: ConstructorItem")
             case (_, f: Items.LocalItem) => fail // can't qualify locals, parameters, TODO: return something with a low score
             case _ => fail
           })
        yield r

    case IndexExp(x,is) => {
      def hasDims(t: Type, d: Int): Boolean = d==0 || (t match {
        case Items.ArrayType(t) => hasDims(t,d-1)
        case _ => false
      })
      val n = is.list.size
      val ints = product(is.list.map(i => denoteExp(i).filter(e => typeOf(e) match {
        case p: PrimType => promote(p) == IntType
        case _ => false
      })))
      for (x <- denoteExp(x);
           if hasDims(typeOf(x),n);
           is <- ints)
        yield is.foldLeft(x)(IndexExpDen)
    }

    case MethodRefExp(x,ts,f) => throw new NotImplementedError("MethodRefs not implemented: " + e)
    case NewRefExp(x,t) => throw new NotImplementedError("NewRef not implemented: " + e)
    case TypeApplyExp(x,ts) => throw new NotImplementedError("Generics not implemented (TypeApplyExp): " + e)
    case NewExp(ts,e) => throw new NotImplementedError("new expression not implemented: " + e)
    case WildExp(b) => throw new NotImplementedError("wildcard expressions not implemented: " + e)

    case ApplyExp(f,xsn) => {
      val xsl = xsn.list map denoteExp
      val n = xsl.size
      for (f <- denoteCallable(f);
           if f.paramTypes.size == n;
           // Filter the denotations for each argument
           filtered = (f.paramTypes zip xsl) map {case (p,xs) => xs.filter(x => looseInvokeContext(typeOf(x),p))};
           // Expand into full Cartesian product (exponential time)
           xl <- product(filtered))
        yield ApplyExpDen(f,xl)
    }

    case UnaryExp(op,x) =>
      for (x <- denoteExp(x);
           if unaryLegal(op,typeOf(x)))
        yield UnaryExpDen(op,x)

    case BinaryExp(op,x,y) => {
      val dy = denoteExp(y);
      for (x <- denoteExp(x);
           y <- dy;
           if binaryLegal(op,typeOf(x),typeOf(y)))
        yield BinaryExpDen(op,x,y)
    }

    case CastExp(t,x) =>
      for (t <- denote(t);
           x <- denoteExp(x);
           if castsTo(typeOf(x),t))
        yield CastExpDen(t,x)

    case CondExp(c,t,f) =>
      for (c <- denoteExp(c);
           if isToBoolean(typeOf(c));
           t <- denoteExp(t);
           f <- denoteExp(f);
           r <- option(condType(typeOf(t),typeOf(f))))
        yield CondExpDen(c,t,f,r)

    case AssignExp(op,x,y) =>
      for (x <- denoteExp(x);
           if isVariable(x);
           xt = typeOf(x);
           y <- denoteExp(y);
           yt = typeOf(y);
           t <- option(assignOpType(op,xt,yt)))
        yield AssignExpDen(op,x,y)

    case _ => throw new NotImplementedError("Trying to compute denotation for node: " + e)
  }
  def denoteExp(n: Exp)(implicit env: Env): Scored[ExpDen] =
    denote(n) collect {case e: ExpDen => e}

  def isVariable(e: ExpDen): Boolean = e match {
    // in java, we can only assign to actual variables, never to values returned by functions or expressions.
    // TODO: implement final, private, protected
    case _: LitDen => false
    case ParameterExpDen(i) => true // TODO: check for final
    case LocalVariableExpDen(i) => true // TODO: check for final
    case EnumConstantExpDen(_) => false
    case CastExpDen(_,_) => false // TODO: java doesn't allow this, but I don't see why we shouldn't
    case UnaryExpDen(_,_) => false // TODO: java doesn't allow this, but we should. Easy for ++,--, and -x = 5 should translate to x = -5
    case BinaryExpDen(_,_,_) => false
    case AssignExpDen(_,_,_) => false
    case ParenExpDen(x) => isVariable(x)
    case ApplyExpDen(_,_) => false
    case FieldExpDen(obj, field) => true // TODO: check for final, private, protected
    case LocalFieldExpDen(field) => true // TODO: check for final
    case StaticFieldExpDen(field) => true // TODO: check for final, private, protected
    case IndexExpDen(a, i) => isVariable(a)
    case CondExpDen(_,_,_,_) => false // TODO: java doesn't allow this, but (x==5?x:y)=10 should be turned into an if statement
  }

  def denoteCallable(e: Exp)(implicit env: Env): Scored[Denotations.Callable] =
    denote(e) collect { case e: Denotations.Callable => e }

  // Statements
  def denoteStmt(s: Stmt)(env: Env): Scored[(Env,StmtDen)] = s match {
    case EmptyStmt() => single((env,EmptyStmtDen()))
    case VarStmt(mod,t,v) => notImplemented
    case ExpStmt(e) => {
      val exps = denoteExp(e)(env) map ExprStmtDen
      val stmts = e match {
        case AssignExp(None,NameExp(x),y) =>
          for {y <- denoteExp(y)(env);
               t = typeOf(y);
               (env,x) <- env.newVariable(x,t)}
            yield (env,VarStmtDen(t,List((x,Some(ExpInitDen(y))))))
        case _ => fail
      }
      exps.map((env,_)) ++ stmts
    }
    case BlockStmt(b) => denoteStmts(b)(env) map {case (e,ss) => (e,BlockStmtDen(ss))}
    case AssertStmt(cond: Exp, msg: Option[Exp]) => notImplemented
    case BreakStmt(label: Option[Name]) => notImplemented
    case ContinueStmt(label: Option[Name]) => notImplemented
    case ReturnStmt(e: Option[Exp]) => notImplemented
    case ThrowStmt(e: Exp) => notImplemented
    case SyncStmt(e: Exp, b: Block) => notImplemented
  }

  def denoteStmts(s: List[Stmt])(env: Env): Scored[(Env,List[StmtDen])] =
    productFoldLeft(env)(s map denoteStmt)

}
