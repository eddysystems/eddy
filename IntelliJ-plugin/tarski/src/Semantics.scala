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
    def f[A,B](v: String, c: String => A)(t: (A,String) => B) = single(t(c(v.replaceAllLiterally("_","")),v))
    x match {
      case AST.IntLit(v) =>    f(v,_.toInt)(Denotations.IntLit)
      case AST.LongLit(v) =>   f(v,_.toLong)(Denotations.LongLit)
      case AST.FloatLit(v) =>  f(v,_.toFloat)(Denotations.FloatLit)
      case AST.DoubleLit(v) => f(v,_.toDouble)(Denotations.DoubleLit)
      case AST.BoolLit(b) =>   single(Denotations.BooleanLit(b))
      case AST.CharLit(v) =>   single(Denotations.CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0),v))
      case AST.StringLit(v) => single(Denotations.StringLit(unescapeJava(v.slice(1,v.size-1)),v))
      case AST.NullLit() =>    single(Denotations.NullLit())
    }
  }

  // Types
  def denote(t: AST.Type)(implicit env: JavaEnvironment): Scored[Den] = t match {
    case NameType(n) => typeScores(n) map TypeDen
    case FieldType(x,f) =>
      for (d <- denote(x);
           t <- d match {
             case t: TypeDen => single(t.item)
             case e: ExpDen => single(typeOf(e)) // Allow expressions to be used as if they were their types
             case _ => fail
           };
           fi <- typeFieldScores(t,f))
        yield Denotations.TypeDen(fi)
    case ModType(m,t) => denote(t) flatMap {
      case TypeDen(t) => m match {
        case Annotation(_) => throw new NotImplementedError("Types with annotations")
        case _ => fail
      }
      case _ => fail
    }
    case AST.ArrayType(t) => denote(t) collect {
      case TypeDen(t) => TypeDen(ArrayType(t))
    }
    case ApplyType(_,_) => throw new NotImplementedError("Generics not implemented (ApplyType): " + t)
    case WildType(_) => throw new NotImplementedError("Type bounds not implemented (WildType): " + t)
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

      /*
    case ApplyExp(e, args) => {
      val adens = args.list.map(denotationScores(_, env))
      for {
        (eden, escore) <- denotationScores(e, env)
        called = callable(eden)
        if called != null && called.paramTypes.size == args.list.size
      } yield {
        // filter the denotations for each argument
        val filtered_adens = (adens zip called.paramTypes).map( x => x._1.filter( z => looseInvokeContext(typeOf(z._1),x._2) ) )

        // combine into new trees
        val product = cartesianProduct(filtered_adens)

        if (eden.isInstanceOf[MethodDen])
          product.map( args => (new MethodCallDen(eden, stmts.map(_.asInstanceOf[StmtDen]).toList), combine()) )
        else

        if (!args.list.forall( isValue(_,eden) ) ) {
          Nil
        } else {
          // check all elements of the list for denotations which match (convertible to the types required by e)
          // combine all these denotations
          val dens: List[DenotationScores] = for {
            (arg, ptype) <- args.list zip eden(e).asInstanceOf[Callable].paramTypes
          } yield {
            // TODO: Try strict before loose in order to handle overloads
            denotationScores(arg,env).filter( ds => looseInvokeContext(typeOf(arg,ds._1), ptype) )
          }

          if (dens.isEmpty)
            List((eden,escore))
          else
            combineDenotationScores(dens).map( merge2Denotations(_,(eden,escore)) )
        }
      }
    }.flatten.toList

    case UnaryExp(op, e) => // could be UnaryExpItem
      combine2Denotations(denotationScores(e,env), denotationScores(op,env), den => unaryLegal(op,typeOf(e,den)))

    case BinaryExp(op, e0, e1) => combine3Denotations(denotationScores(e0,env), // could be BinaryExpItem
                                                      denotationScores(op,env),
                                                      denotationScores(e1,env),
                                                      // it's not a binary exp if it's an assign exp
                                                      den => binaryLegal(op, typeOf(e0, den), typeOf(e1, den)))

    case CastExp(t, e) =>
      combine2Denotations(denotationScores(t,env), denotationScores(e,env), den => castsTo(typeOf(e,den), typeItem(t,den)) )

    case CondExp(cond, t, f) =>
      combine3Denotations(denotationScores(cond,env), denotationScores(t,env), denotationScores(f,env),
                          den => isToBoolean(typeOf(cond,den)) ) // TODO: are there restrictions on the types of t and f?

    case AssignExp(left, None, right) =>
      combine2Denotations(denotationScores(left, env),
                          denotationScores(right, env),
                          den => isVariable(left, den) && assignsTo(typeOf(right, den), typeOf(left, den)))
    case AssignExp(left, Some(op), right) =>
      combine3Denotations(denotationScores(left, env),
                          denotationScores(op, env),
                          denotationScores(right, env),
                          den => {
                            val lt = typeOf(left,den)
                            val rt = typeOf(right,den)
                            isVariable(left,den) && binaryType(op,lt,rt).forall(assignsTo(_,lt))
                          })
      */

    case _ => throw new NotImplementedError("Trying to compute denotation for node: " + e);
  }

  // Statements
  def denote(s: Stmt)(implicit env: JavaEnvironment): Scored[StmtDen] = s match {
    case EmptyStmt() => single(EmptyStmtDen())
    case ExpStmt(e) => denote(e) collect {case e: ExpDen => ExprStmtDen(e)}
    case BlockStmt(b) => partialProduct(b.map(denote)){case s: StmtDen => s} map BlockStmtDen
    case _ => throw new NotImplementedError("Trying to compute denotation for statement: " + s)
  }
}
