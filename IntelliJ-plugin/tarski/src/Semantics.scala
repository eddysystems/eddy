package tarski

import AST._
import Environment._

/**
 * Created by martin on 21.10.14.
 */
object Semantics {

  /**
   * A score.
   * Really just a Float, but with some functions to encapsulate the averaging and such that happens
   */
  class Score(val s: Float) extends Ordered[Score] {
    override def compare(s2: Score) = s.compare(s2.s)

    override def toString() = s.toString
  }

  object ZeroScore extends Score(0)

  def combine(s: List[Score]): Score = {
    s.reduce( (x,y) => new Score(x.s + y.s) )
  }

  type Denotation = Map[Node,EnvItem]
  type DenotationScores = List[(Denotation,Score)]

  // get a score for a AST tree node type (given an instance), given the environment
  def ASTscore(node: Node, env: JavaEnvironment): Score = node match {
    case _ => ZeroScore
  }

  // for an expression with meaning as a type, its associated type item, otherwise, null
  def typeItem(node: Node, meaning: Denotation): TypeItem = {
    def typeItemLeaf(item: EnvItem): TypeItem = {
      if (item.isInstanceOf[TypeItem]) item.asInstanceOf[TypeItem] else null
    }

    node match {
      case NameType(_) => typeItemLeaf(meaning(node))
      case FieldType(_,_) => typeItemLeaf(meaning(node))
      case NameExp(_) => typeItemLeaf(meaning(node))
      case ParenExp(e) => typeItem(e, meaning)
      case FieldExp(_, _, f) => typeItemLeaf(meaning(node))
      case TypeApplyExp(e, _) => typeItem(e, meaning)

      case _ => {
        assert(!node.isInstanceOf[Type])
        null
      }
    }
  }

  // for an expression which has a type, its associated type item
  def typeOf(node: Node, meaning: Denotation): TypeItem = {
    def typeOfLeaf(item: EnvItem): TypeItem = {
      if (item.isInstanceOf[HasTypeItem]) item.asInstanceOf[HasTypeItem].ourType else null
    }

    node match {
      case NameType(_) => typeOfLeaf(meaning(node))
      case FieldType(_,_) => typeOfLeaf(meaning(node))
      case NameExp(_) => typeOfLeaf(meaning(node))
      case ParenExp(e) => typeOf(e, meaning)
      case FieldExp(_, _, f) => typeOfLeaf(meaning(node))
      case TypeApplyExp(e, _) => typeOf(e, meaning)
      // TODO
      case _ => null
    }
  }

  // given the denotation, is this node a type?
  def isType(node: Node, meaning: Denotation): Boolean = node.isInstanceOf[Type]

  // given the denotation. does this node have a type?
  def hasType(node: Node, meaning: Denotation): Boolean = typeOf(node,meaning) != null

  // is this an lvalue
  def isVariable(node: Node, meaning: Denotation): Boolean = {
    // TODO
    true
  }

  // get scores different things in the environment an AST node could be
  def denotationScores(node: Node, env: JavaEnvironment): DenotationScores = {

    def cartesianProduct[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] = xs.foldLeft(Seq(Seq.empty[A])) {
          (x, y) => for (a <- x.view; b <- y) yield a :+ b
    }

    // merge several denotations and combine their scores
    def mergeDenotations(ds: Seq[(Denotation,Score)]): (Denotation,Score) = ds.reduce {
      (ds1, ds2) => (ds1._1 ++ ds2._1, combine(List(ds1._2, ds2._2)))
    }

    def combineDenotationScores(ds: List[DenotationScores]): DenotationScores =
      cartesianProduct(ds).map( dens => mergeDenotations(dens) ).toList

    def nodeDenotationScores(node: Node, scores: List[(Score,EnvItem)]): DenotationScores = scores.map( x => (Map((node, x._2)), x._1) )
    def combineDenotationAndScores(node: Node, den: Denotation, score: Score, scores: List[(Score,EnvItem)]): DenotationScores =
      scores.map( x => (den ++ Map((node, x._2)), combine(List(score, x._1))) )

    def combine2Denotations(d0: DenotationScores, d1: DenotationScores, cond: Denotation => Boolean = _ => true): DenotationScores = {
      for {
        (den0, score0) <- d0
        (den1, score1) <- d1
        den: Denotation = den0 ++ den1
        score: Score = combine(List(score0, score1))
      } yield {
        // check if e1 and e2 can be combined using op
        if (cond(den)) {
          List((den, score))
        } else {
          Nil
        }
      }
    }.flatten.toList

    def combine3Denotations(d0: DenotationScores, d1: DenotationScores, d2: DenotationScores, cond: Denotation => Boolean = _ => true): DenotationScores = {
      for {
        (den0, score0) <- d0
        (den1, score1) <- d1
        (den2, score2) <- d2
        den: Denotation = den0 ++ den1 ++ den2
        score: Score = combine(List(score0, score1, score2))
      } yield {
        // check if e1 and e2 can be combined using op
        if (cond(den)) {
          List((den, score))
        } else {
          Nil
        }
      }
    }.flatten.toList

    def biasDenotationScores(s: DenotationScores, b: Score): DenotationScores =
      s.map( x => (x._1,combine(List(x._2, ASTscore(node,env)))) )

    // a denotation that assigns no meanings, and is indifferent about likelihood
    val NullDenotation = List((Nil.toMap[Node,EnvItem],ZeroScore))

    val scores = node match {
        // TODO: control flow statements are a bit more involved
      case _: Stmt => combineDenotationScores(children(node).map(denotationScores(_, env)))

      // AST nodes without name lookup just are what they are
      case _: BinaryOp => NullDenotation
      case _: UnaryOp => NullDenotation
      case _: Lit => NullDenotation
      case Abstract() => NullDenotation
      case Public() => NullDenotation
      case Protected() => NullDenotation
      case Private() => NullDenotation
      case Static() => NullDenotation
      case Final() => NullDenotation
      case Strictfp() => NullDenotation
      case Transient() => NullDenotation
      case Volatile() => NullDenotation
      case Synchronized() => NullDenotation
      case _: Bound => NullDenotation

      // Annotations
      case Annotation(name) => nodeDenotationScores(node, env.getAnnotationScores(name))

      // Types
      case NameType(name) => nodeDenotationScores(node, env.getTypeScores(name))

      case FieldType(t, field) => {
        for {
          (tden, tscore) <- denotationScores(t, env)
        } yield {
          if (isType(t, tden)) {
            combineDenotationAndScores(node, tden, tscore, env.getTypeFieldScores(typeItem(t, tden), field))
          } else if (hasType(t, tden)) {
            combineDenotationAndScores(node, tden, tscore, env.getTypeFieldScores(typeOf(t, tden), field))
          } else {
            Nil
          }
        }
      }.flatten.toList

      case ModType(mod, t) => combine2Denotations(denotationScores(mod, env), denotationScores(t, env)) // could be ModifiedTypeItem
      case ArrayType(t: Type) => denotationScores(t, env) // could be ArrayTypeItem

      case ApplyType(t: Type, a: KList[Type]) => // could be GenericTypeItem
        throw new Exception()

      case WildType(b: Option[(Bound, Type)]) => // could be BoundedTypeItem
        if (b.isEmpty) NullDenotation
        else combine2Denotations(denotationScores(b.get._1, env),
          denotationScores(b.get._2, env),
          den => den(b.get._2).isInstanceOf[ClassItem] || den(b.get._2).isInstanceOf[InterfaceItem])

      // Expressions
      case NameExp(name) => nodeDenotationScores(node, env.getScores(name))

      case LitExp(l) => denotationScores(l, env)
      case ParenExp(e) => denotationScores(e, env)

      case FieldExp(e, t, f) => throw new Exception()
      case IndexExp(e, i) => throw new Exception()

      case MethodRefExp(e, t, f) => throw new Exception()
      case NewRefExp(e, t) => throw new Exception()
      case TypeApplyExp(e, t) => throw new Exception()

      case ApplyExp(e, args) => {
        for {
          (eden, escore) <- denotationScores(e, env)
        } yield {
          if (!eden(e).isInstanceOf[CallableItem] ||
              eden(e).asInstanceOf[CallableItem].paramTypes.size != args.list.size ||
              !args.list.forall( isType(_,eden) ) ) {
            Nil
          } else {
            // check all elements of the list for denotations which match (convertible to the types required by e)
            // combine all these denotations
            val dens: List[DenotationScores] = for {
              (arg, ptype) <- args.list zip eden(e).asInstanceOf[CallableItem].paramTypes
            } yield {
              denotationScores(arg,env).filter( ds => env.convertibleTo(typeOf(arg,ds._1), ptype) )
            }

            combineDenotationScores(dens)
          }
        }
      }.flatten.toList

      case NewExp(t, e) => throw new Exception()

      case WildExp(b: Option[(Bound,Type)]) =>
        if (b.isEmpty) NullDenotation
        else combine2Denotations(denotationScores(b.get._1, env),
          denotationScores(b.get._2, env),
          den => den(b.get._2).isInstanceOf[ClassItem] || den(b.get._2).isInstanceOf[InterfaceItem])

      case UnaryExp(op, e) => // could be UnaryExpItem
        combine2Denotations(denotationScores(e,env), denotationScores(op,env), den => env.operatorLegal(op,typeOf(e,den)))

      case BinaryExp(e0, op, e1) => combine3Denotations(denotationScores(e0,env), // could be BinaryExpItem
                                                        denotationScores(op,env),
                                                        denotationScores(e1,env),
                                                        // it's not a binary exp if it's an assign exp
                                                        den => !op.isInstanceOf[AssignOp]
                                                          && env.operatorLegal(op, typeOf(e0, den), typeOf(e1, den)))

      case CastExp(t, e) =>
        combine2Denotations(denotationScores(t,env), denotationScores(e,env), den => env.castableTo(typeOf(e,den), typeItem(t,den)) )

      case CondExp(cond, t, f) =>
        combine3Denotations(denotationScores(cond,env), denotationScores(t,env), denotationScores(f,env),
                            den => env.convertibleTo(typeOf(cond,den), BooleanItem) ) // TODO: are there restrictions on the types of t and f?

      case AssignExp(left, None, right) =>
          combine2Denotations(denotationScores(left, env),
                              denotationScores(right, env),
                              den => isVariable(left, den) && env.convertibleTo(typeOf(right, den), typeOf(left, den)))
      case AssignExp(left, Some(op), right) =>
          combine3Denotations(denotationScores(left, env),
                              denotationScores(op, env),
                              denotationScores(right, env),
                              den => isVariable(left, den) && env.convertibleTo(env.expressionType(op, typeOf(left,den), typeOf(right,den)), typeOf(left, den)))

    }

    // add the AST node bias
    biasDenotationScores(scores, ASTscore(node,env))
  }
}
