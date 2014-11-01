package tarski

import java.io.{ObjectInputStream, FileInputStream, ObjectOutputStream, FileOutputStream}

import Semantics.Score
import tarski.AST.{UnaryOp, BinaryOp}
import tarski.Items._

object Environment {

  /**
   * Contains the environment used for name resolution
   */
  case class JavaEnvironment(things: List[NamedItem]) extends scala.Serializable {
    // used on plugin side to fill in data
    def addObjects(xs: List[NamedItem]): JavaEnvironment = {
      // TODO: this is quadratic time due to order, but order for now is important
      // TODO: filter identical things (like java.lang.String)
      JavaEnvironment(things ++ xs)
    }

    // Fuzzy Query interface

    // what could this name be?
    def scores(name: String): List[(Score, EnvItem)] = {
      things.toList.filter( _.name == name ).map( x => (Score(1.0f), x) )
    }

    // what could this name be, assuming it is a type?
    def typeScores(name: String): List[(Score, EnvItem)] = {
      things.toList.filter( x => x.isInstanceOf[Type] && x.name == name ).map((Score(1.0f), _))
    }

    def fieldScores(t: Type, name: String): List[(Score, EnvItem)] = {
      things.toList.filter( x => x.name == name ).map((Score(1.0f), _))
    }

    // what could this be, assuming it is a type field of the given type? (only returns types)
    def typeFieldScores(t: Type, name: String): List[(Score, Type)] = {
      // TODO: this should take into account containers
      things.toList.collect({ case t: Type if t.name == name => (Score(1.0f),t)})
    }

    // what could this name be, assuming it is an annotation
    def annotationScores(name: String): List[(Score, EnvItem)] = {
      things.toList.filter(x => x.isInstanceOf[AnnotationItem] && x.name == name).map((Score(1.0f), _))
    }
  }

  def envToFile(env: JavaEnvironment, name: String): Unit = {
    val os = new FileOutputStream(name)
    val oos = new ObjectOutputStream(os)
    oos.writeObject(env)
    oos.close()
    os.close()
  }

  def envFromFile(name: String): JavaEnvironment = {
    val is = new FileInputStream(name)
    val ois = new ObjectInputStream(is)
    val env = ois.readObject().asInstanceOf[JavaEnvironment]
    ois.close()
    is.close()
    env
  }

  // Environment with Java's basic types
  // add String, which really should always be there
  val baseEnvironment = JavaEnvironment(List(BooleanType, IntType, FloatType, LongType, DoubleType, CharType, StringType))
}