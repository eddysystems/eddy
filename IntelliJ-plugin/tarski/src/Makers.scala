package tarski

import tarski.AST._
import tarski.Types._
import tarski.Items._

// Unpleasant imperative classes for use in processing recursive environments
object Makers {

    case class ClassItemMaker(name: Name, parent: ParentItem, var tparams: List[TypeVar],
                         isClass: Boolean, isEnum: Boolean, isFinal: Boolean) extends ClassItem {
    // To be filled in later
    private var _base: ClassType = null
    private var _implements: List[ClassType] = null

    // Public interface
    def base = { assert(_base ne null); _base }
    def implements = { assert(_base ne null); _implements }

    def set(base: ClassType, implements: List[ClassType]): Unit = {
      assert(_base eq null)
      _base = base
      _implements = implements
    }
  }

  case class TypeVarMaker(name: Name) extends TypeVar {
    // To be filled in later
    private var _base: ClassType = null
    private var _implements: List[ClassType] = null

    // Public interface
    override def supers = { assert(_base != null); _base :: _implements }
    def lo = NullType
    def hi = glb(supers)

    def set(base: ClassType, implements: List[ClassType]): Unit = {
      assert(_base eq null)
      _base = base
      _implements = implements
    }
  }
}
