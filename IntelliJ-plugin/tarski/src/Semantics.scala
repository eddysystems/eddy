package tarski

import org.apache.commons.lang.StringEscapeUtils.{escapeJava,unescapeJava}

import AST.{Type => _, ArrayType => _, _}
import Types._
import Environment._
import Items._
import tarski.Denotations._

object Semantics {

  /**
   * A score.
   * Really just a Float, but with some functions to encapsulate the averaging and such that happens
   */
  case class Score(s: Float) extends Ordered[Score] {
    override def compare(s2: Score) = s.compare(s2.s)

    override def toString = s.toString
  }

  object ZeroScore extends Score(0)

  def combine(s: List[Score]): Score = {
    s.reduce( (x,y) => new Score(x.s + y.s) )
  }

  type Denotation = Den
  type DenotationScores = List[(Denotation,Score)]

  // get a score for a AST tree node type (given an instance), given the environment
  def ASTscore(node: Node, env: JavaEnvironment): Score = node match {
    case _ => ZeroScore
  }

  // get scores different things in the environment an AST node could be
  def denotationScores(node: Node, env: JavaEnvironment): DenotationScores = {

    def cartesianProduct[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] = xs.foldLeft(Seq(Seq.empty[A])) {
          (x, y) => for (a <- x.view; b <- y) yield a :+ b
    }

    def cartesianProductIf[A](xs: Traversable[Traversable[A]], cond: A => Boolean): Seq[Seq[A]] = xs.foldLeft(Seq(Seq.empty[A])) {
          (x, y) => for (a <- x.view; b <- y if cond(b) ) yield a :+ b
    }

    val scores: DenotationScores = node match {
      case EmptyStmt() => List((EmptyStmtDen(),ZeroScore))
      case ExpStmt(e) => denotationScores(e,env).collect( {
        case (x: ExpDen,y) => (new ExprStmtDen(x),y)
      })
      case BlockStmt(b) => cartesianProductIf( b.map(denotationScores(_, env)), (x: (Den,Score)) => x._1.isInstanceOf[StmtDen] ).map( stmts =>
        (new BlockStmtDen(stmts.map(_._1.asInstanceOf[StmtDen]).toList), combine(stmts.map(_._2).toList))
      ).toList

      case _: Stmt => throw new NotImplementedError("Trying to compute denotation for statement: " + node)

      // AST nodes without name lookup just are what they are
      case _: BinaryOp => throw new RuntimeException("Trying to compute denotation for binary operator node: " + node)
      case _: UnaryOp => throw new RuntimeException("Trying to compute denotation for unary operator node: " + node)

      case AST.IntLit(v) => List((new Denotations.IntLit(v.replaceAllLiterally("_","").toInt, v), ZeroScore))
      case AST.LongLit(v) => List((new Denotations.LongLit(v.replaceAllLiterally("_","").toLong,v), ZeroScore))
      case AST.FloatLit(v) => List((new Denotations.FloatLit(v.replaceAllLiterally("_","").toFloat,v), ZeroScore))
      case AST.DoubleLit(v) => List((new Denotations.DoubleLit(v.replaceAllLiterally("_","").toDouble,v), ZeroScore))
      case AST.BoolLit(v) => List((new Denotations.BooleanLit(v,v.toString), ZeroScore))
      case AST.CharLit(v) => List((new Denotations.CharLit(unescapeJava(v.slice(1,v.size-1)).charAt(0), v), ZeroScore))
      case AST.StringLit(v) => List((new Denotations.StringLit(unescapeJava(v.slice(1,v.size-1)),v), ZeroScore))
      case AST.NullLit() => List((new Denotations.NullLit, ZeroScore))

      // Annotations
      case Annotation(name) => throw new NotImplementedError("Trying to compute denotation for annotation: " + node)

      case _: AST.Mod => throw new RuntimeException("Trying to compute denotation for modifier: " + node)
      case _: Bound => throw new RuntimeException("Trying to compute denotation for bound: " + node)

      // Types
      case NameType(name) => env.typeScores(name).collect( {
        case (s: Score, item: Items.Type) => (new TypeDen(item),s)
      })

      case FieldType(base, field) => {
        for {
          (bden,bscore) <- denotationScores(base,env)
          tbase <- bden match {
            case t: TypeDen => List(t.item)
            case e: ExpDen => List(typeOf(e)) // Allow expressions to be used as if they were their types
            case _ => Nil
          }
          (fscore, fitem) <- env.typeFieldScores(tbase, field) if (fitem match { case m: Member => m.containing == tbase
                                                                                 case _ => false })
        } yield {
          List((new Denotations.TypeDen(fitem), combine(List(bscore, fscore))))
        }
      }.flatten.toList

      case ModType(mod, t) => denotationScores(t,env).flatMap({
        case (TypeDen(t), s: Score) => mod match {
          case Annotation(_) => throw new NotImplementedError("Types with annotations")
          case _ => Nil
        }
      })

      case AST.ArrayType(t) => denotationScores(t, env).collect( {
        case (TypeDen(t), s) => (TypeDen(ArrayType(t)), s)
      })

      case ApplyType(t, a) => throw new NotImplementedError("Generics not implemented (ApplyType): " + node)
      case WildType(b) => throw new NotImplementedError("Type bounds not implemented (WildType): " + node)

      // Expressions

      // this is never a type
      case NameExp(name) => env.scores(name).collect({
        case (s, i: FieldItem) => (Denotations.LocalFieldExpDen(i),s)
        case (s, i: ParameterItem) => (Denotations.ParameterExpDen(i),s)
        case (s, i: LocalVariableItem) => (Denotations.LocalVariableExpDen(i),s)
        case (s, i: EnumConstantItem) => (Denotations.EnumConstantExpDen(i),s)
        case (s, i: MethodItem) => (Denotations.MethodDen(i),s)
        case (s, i: ConstructorItem) => (Denotations.ForwardDen(i),s)
      })

      case LitExp(l) => denotationScores(l, env) // just forward, this node doesn't appear in the denotation tree
      case ParenExp(e) => denotationScores(e, env).collect( {
        case (den: ExpDen, s: Score) => (new ParenExpDen(den), s)
      })

      // base is either a type, or an expression, field is an inner type, a method, or a field
      case FieldExp(base, tparams, field) => if (tparams.isDefined) throw new NotImplementedError("Generics not implemented (FieldExp): " + node) else {
        for {
          (bden,bscore) <- denotationScores(base,env)
          tbase <- bden match {
            case t: TypeDen => List(t.item)
            case e: ExpDen => List(typeOf(e))
            case _ => Nil
          }
          (fscore, fitem) <- env.fieldScores(tbase, field) if (fitem match { case m: Member => m.containing == tbase // TODO: Subtypes are not handled here
                                                                             case _ => false })
        } yield {
          ((bden,fitem) match {
            case (_, f: Items.Type) => Some(TypeDen(f))
            case (_, f: Items.EnumConstantItem) => Some(EnumConstantExpDen(f))
            case (_, f: Items.StaticFieldItem) => Some(StaticFieldExpDen(f))
            case (e: ExpDen, f: Items.FieldItem) => Some(FieldExpDen(e,f))
            case (_, f: Items.StaticMethodItem) => Some(StaticMethodDen(f))
            case (e: ExpDen, f: Items.MethodItem) => Some(MethodDen(e,f))

            case (_, f: Items.PackageItem) => throw new NotImplementedError("FieldExp: packages not implemented: " + node)
            case (_, f: Items.ConstructorItem) => throw new NotImplementedError("FieldExp: ConstructorItem")
            case (_, f: Items.LocalItem) => None // can't qualify locals, parameters, TODO: return something with a low score
            case _ => None
          }) match {
            case None => Nil
            case Some(d) => List((d,combine(List(bscore, fscore))))
          }
        }
      }.flatten.toList

      case IndexExp(e, i) => throw new NotImplementedError("Index semantics not implemented: " + node)

      case MethodRefExp(e, t, f) => throw new NotImplementedError("MethodRefs not implemented: " + node)
      case NewRefExp(e, t) => throw new NotImplementedError("NewRef not implemented: " + node)
      case TypeApplyExp(e, t) => throw new NotImplementedError("Generics not implemented (TypeApplyExp): " + node)
      case NewExp(t, e) => throw new NotImplementedError("new expression not implemented: " + node)
      case WildExp(b) => throw new NotImplementedError("wildcard expressions not implemented: " + node)

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

      case _ => throw new NotImplementedError("Trying to compute denotation for node: " + node);
    }

    println("      scores for " + node + ": " + scores)

    // add the AST node bias
    scores.map( x => (x._1, combine(List(x._2, ASTscore(node,env)))) )
  }
}
