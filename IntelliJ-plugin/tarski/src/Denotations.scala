package tarski

/**
 * Created by martin on 30.10.14.
 */
object Denotations {

  val whiteSpace = ' '

  sealed abstract class Den

  sealed abstract class ItemDen extends Den {
    def item: Items.NamedItem
  }

  // items in the environment
  sealed abstract class ValueDen extends ItemDen { override def item: Items.Value }

  // types
  sealed class TypeDen(val item: Items.Type) extends ItemDen
  case class ArrayTypeDen(base: TypeDen) extends TypeDen(base.item) // TODO: directly reference the global type representing the array type?
  case class ModTypeDen(mods: AST.Mod, base: TypeDen) extends TypeDen(base.item)

  // callables
  case class MethodDen(item: Items.MethodItem) extends ItemDen
  case class ConstructorDen(item: Items.ConstructorItem) extends ItemDen

  // values
  case class FieldDen(item: Items.FieldItem) extends ValueDen
  case class ParameterDen(item: Items.ParameterItem) extends ValueDen
  case class LocalVariableDen(item: Items.LocalVariableItem) extends ValueDen
  case class EnumConstantDen(item: Items.EnumConstantItem) extends ValueDen

  sealed abstract class StmtDen extends Den
  case class EmptyStmtDen() extends StmtDen
  case class ExprStmtDen(e: ExprDen) extends StmtDen
  case class BlockStmtDen(b: List[StmtDen]) extends StmtDen

  sealed abstract class ExprDen extends Den

  sealed abstract class LitDen extends ExprDen { def text: String }
  case class ByteLit(b: Byte, text: String) extends LitDen
  case class ShortLit(s: Short, text: String) extends LitDen
  case class IntLit(i: Int, text: String) extends LitDen
  case class LongLit(l: Long, text: String) extends LitDen
  case class BooleanLit(b: Boolean, text: String) extends LitDen
  case class StringLit(s: String, text: String) extends LitDen
  case class FloatLit(f: Float, text: String) extends LitDen
  case class DoubleLit(d: Double, text: String) extends LitDen
  case class CharLit(c: Char, text: String) extends LitDen
  case class NullLit() extends LitDen { val text = "null" }

  case class CastExprDen(t: TypeDen, e: ExprDen) extends ExprDen
  case class UnaryExprDen(op: AST.UnaryOp, e: ExprDen) extends ExprDen
  case class BinaryExprDen(e0: ExprDen, op: AST.BinaryOp, e1: ExprDen) extends ExprDen
  case class AssignExprDen(left: ExprDen, op: AST.AssignOp, right: ExprDen) extends ExprDen
  case class ParenExprDen(e: ExprDen) extends ExprDen
  case class ObjMethodExprDen(obj: ExprDen, method: MethodDen) extends ExprDen
  case class MethodCallDen(called: ObjMethodExprDen, params: List[ExprDen]) extends ExprDen
  case class ConstructorCallDen(cons: ConstructorDen, params: List[ExprDen]) extends ExprDen
  case class FieldExprDen(obj: ExprDen, field: FieldDen) extends ExprDen
  case class IndexExprDen(e: ExprDen, idx: ExprDen) extends ExprDen
  
  def pretty(ds: List[Den], delim: String = "," + whiteSpace): String = 
    if (ds.isEmpty) "" else ds.tail.foldLeft[String](pretty(ds.head))( (x,y) => x + ", " + pretty(y))

  def pretty(op: AST.BinaryOp, assign: Boolean): String = {
    val os = op match {
      case AST.MulOp() => "*"
      case AST.DivOp() => "/"
      case AST.ModOp() => "%"
      case AST.AddOp() => "+"
      case AST.SubOp() => "-"
      case AST.LShiftOp() => "<<"
      case AST.RShiftOp() => ">>"
      case AST.UnsignedRShiftOp() => ">>>"
      case AST.LtOp() => "<"
      case AST.GtOp() => ">"
      case AST.LeOp() => "<="
      case AST.GeOp() => ">="
      case AST.InstanceofOp() => "instanceof"
      case AST.EqOp() => "=="
      case AST.NeOp() => "!="
      case AST.AndOp() => "&"
      case AST.XorOp() => "^"
      case AST.OrOp() => "|"
      case AST.AndAndOp() => "&&"
      case AST.OrOrOp() => "||"
    } 
    if (assign) { assert(op.isInstanceOf[AST.AssignOp]); os + "=" } else os
  }

  def pretty(d: Den): String = d match {
    case d: ItemDen => d.item.relativeName
    case d: LitDen => d.text
    case UnaryExprDen(op,e) => op match {
      case AST.PreIncOp() => "++" + pretty(e)
      case AST.PostIncOp() => pretty(e) + "++"
      case AST.PreDecOp() => "--" + pretty(e)
      case AST.PostDecOp() => pretty(e) + "--"
      case AST.PosOp() => '+' + pretty(e)
      case AST.NegOp() => '-' + pretty(e)
      case AST.CompOp() => '~' + pretty(e)
      case AST.NotOp() => '!' + pretty(e)
    }

    case BinaryExprDen(e0,op,e1) => pretty(e0) + whiteSpace + pretty(op, assign=false) + whiteSpace + pretty(e1)
    case AssignExprDen(left,op,right) => pretty(left) + whiteSpace + pretty(op, assign=true) + whiteSpace + pretty(right)
    case ParenExprDen(e) => '(' + pretty(e) + ')'
    case ObjMethodExprDen(o,m) => (if (o != null) pretty(o) + '.' else "") + pretty(m)
    case MethodCallDen(m,p) => pretty(m) + '(' + pretty(p) + ')'
    case ConstructorCallDen(c,p) => pretty(c) + '(' + pretty(p) + ')'
    case FieldExprDen(obj,f) => pretty(obj) + '.' + pretty(f)
    case IndexExprDen(e,i) => pretty(e) + '[' + pretty(i) + ']'

    case EmptyStmtDen() => ""
    case ExprStmtDen(e) => pretty(e) + ';'
    case BlockStmtDen(b) => pretty(b, " ")

    case _ => throw new RuntimeException("cannot pretty-print " + d)
  }

  // return the callable represented by this node, or null if this node is not callable
  def callable(d: Den): Items.Callable = d match {
    case MethodDen(m) => m
    case ConstructorDen(c) => c
    case _ => null
  }
  def isCallable(d: Den): Boolean = callable(d) != null

  // return the type represented by this Denotation, or null this node is not a type
  def typeItem(d: Den): Items.Type = d match {
    case t: TypeDen => t.item
    case _ => null
  }
  def isType(d: Den): Boolean = typeItem(d) != null

  def typeOf(d: Den): Items.Type = d match {
    case d: ValueDen => d.item.ourType
    case ByteLit(_,_) => Items.ByteType
    case ShortLit(_,_) => Items.ShortType
    case IntLit(_,_) => Items.IntType
    case LongLit(_,_) => Items.LongType
    case BooleanLit(_,_) => Items.BooleanType
    case StringLit(_,_) => Items.StringType
    case FloatLit(_,_) => Items.FloatType
    case DoubleLit(_,_) => Items.DoubleType
    case CharLit(_,_) => Items.CharType
    case NullLit() => Items.NullType

    // propagate types through expressions
    case BinaryExprDen(e0,op,e1) => Types.binaryType(op, typeOf(e0), typeOf(e1)).orNull
    case UnaryExprDen(op,e) => Types.unaryType(op, typeOf(e)).orNull
    case CastExprDen(t,e) => t.item
    case AssignExprDen(left,op,right) => typeOf(left)
    case ParenExprDen(e) => typeOf(e)
    case MethodCallDen(objmethod, params) => objmethod.method.item.retVal
    case FieldExprDen(obj: ExprDen, field: FieldDen) => field.item.ourType
    case IndexExprDen(e: ExprDen, idx: ExprDen) => typeOf(e).asInstanceOf[Items.ArrayType].inner

    case _ => null
  }
  def isValue(d: Den): Boolean = typeOf(d) != null

  // is this an lvalue
  def isVariable(d: Den): Boolean = true // TODO


}
