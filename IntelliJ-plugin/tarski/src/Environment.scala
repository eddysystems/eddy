package tarski

import Semantics.Score
import tarski.AST.{UnaryOp, BinaryOp, Name}
import tarski.Items._

import scala.collection.mutable

object Environment {
  /**
   * Contains the environment used for name resolution
   */
  case class JavaEnvironment(things: List[NamedItem]) {
    // used on plugin side to fill in data
    def addObjects(xs: List[NamedItem]): JavaEnvironment = {
      // TODO: this is quadratic time due to order, but order for now is important
      // TODO: filter identical things (like java.lang.String)
      JavaEnvironment(things ++ xs)
    }

    // whether from can be implicitly converted to to
    def convertibleTo(from: TypeItem, to: TypeItem): Boolean = {
      // TODO
      from == to
    }

    // whether from can be explicitly cast to to
    def castableTo(from: TypeItem, to: TypeItem): Boolean = {
      // TODO
      from == to
    }

    def operatorLegal(op: BinaryOp, t0: TypeItem, t1: TypeItem): Boolean = {
      // TODO
      t0 == t1
    }

    def operatorLegal(op: UnaryOp, t: TypeItem): Boolean = {
      // TODO
      true
    }

    def expressionType(op: BinaryOp, t0: TypeItem, t1: TypeItem): TypeItem = {
      // TODO
      t0
    }

    // Fuzzy Query interface

    // what could this name be?
    def getScores(name: String): List[(Score, EnvItem)] = {
      things.toList.filter( _.name == name ).map( x => (new Score(1.0f), x) )
    }

    // what could this name be, assuming it is a type?
    def getTypeScores(name: String): List[(Score, EnvItem)] = {
      things.toList.filter( x => x.isInstanceOf[TypeItem] && x.name == name ).map((new Score(1.0f), _))
    }

    // what could this be, assuming it is a type field of the given type?
    def getTypeFieldScores(t: TypeItem, name: String): List[(Score, EnvItem)] = {
      // TODO: this should take into account containers
      things.toList.filter( x => x.isInstanceOf[TypeItem] && x.name == name ).map((new Score(1.0f), _))
    }

    // what could this name be, assuming it is an annotation
    def getAnnotationScores(name: String): List[(Score, EnvItem)] = {
      things.toList.filter(x => x.isInstanceOf[AnnotationItem] && x.name == name).map((new Score(1.0f), _))
    }
  }

  // Environment with Java's basic types
  // add String, which really should always be there
  val stringItem = new ClassItemImpl("String", "java.lang.String", "String")
  val baseEnvironment = JavaEnvironment(List(BooleanItem, IntItem, FloatItem, LongItem, DoubleItem, CharItem, stringItem))
}