package tarski

import org.apache.commons.lang.StringEscapeUtils.{escapeJava,unescapeJava}

import AST.{Type => _, ArrayType => _, _}
import Types._
import Environment._
import Items._
import tarski.Denotations._
import Scores._

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
  def denote(n: AST.Type)(implicit env: JavaEnvironment): Scored[Type] = n match {
    case NameType(n) => typeScores(n)
    case FieldType(x,f) => for (t <- denote(x); fi <- typeFieldScores(t,f)) yield fi
    case ModType(Annotation(_),t) => throw new NotImplementedError("Types with annotations")
    case ModType(_,_) => fail
    case AST.ArrayType(t) => denote(t) map ArrayType
    case ApplyType(_,_) => throw new NotImplementedError("Generics not implemented (ApplyType): " + n)
    case WildType(_) => throw new NotImplementedError("Type bounds not implemented (WildType): " + n)
  }
  def denoteType(n: Exp)(implicit env: JavaEnvironment): Scored[Type] =
    denote(n) collect {
      case t: TypeDen => t.item
      case e: ExpDen => typeOf(e) // Allow expressions to be used as types
    }

  // Expressions
  def denote(e: Exp)(implicit env: JavaEnvironment): Scored[Den] = e match {
    case NameExp(n) => scores(n) collect {
      case i: FieldItem => Denotations.LocalFieldExpDen(i)
      case i: ParameterItem => Denotations.ParameterExpDen(i)
      case i: LocalVariableItem => Denotations.LocalVariableExpDen(i)
      case i: EnumConstantItem => Denotations.EnumConstantExpDen(i)
      case i: MethodItem => throw new NotImplementedError("MethodDen") // Denotations.MethodDen(i)
      case i: ConstructorItem => Denotations.ForwardDen(i)
    }
    case LitExp(x) => denote(x)
    case ParenExp(e) => denote(e) // Java doesn't allow parentheses around types, but we do

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
    case IndexExp(x,i) => throw new NotImplementedError("Index semantics not implemented: " + e)
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

    case BinaryExp(op,x,y) =>
      for (x <- denoteExp(x);
           y <- denoteExp(y);
           if binaryLegal(op,typeOf(x),typeOf(y)))
        yield BinaryExpDen(op,x,y)

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
  def denoteExp(n: Exp)(implicit env: JavaEnvironment): Scored[ExpDen] =
    denote(n) collect {case e: ExpDen => e}
  def isVariable(e: ExpDen): Boolean = throw new NotImplementedError("isVariable")
  def denoteCallable(n: Exp)(implicit env: JavaEnvironment): Scored[Denotations.Callable] =
    throw new NotImplementedError("denoteCallable")

  // Statements
  def denote(s: Stmt)(implicit env: JavaEnvironment): Scored[StmtDen] = s match {
    case EmptyStmt() => single(EmptyStmtDen())
    case ExpStmt(e) => denote(e) collect {case e: ExpDen => ExprStmtDen(e)}
    case BlockStmt(b) => partialProduct(b.map(denote)){case s: StmtDen => s} map BlockStmtDen
    case _ => throw new NotImplementedError("Trying to compute denotation for statement: " + s)
  }
}
