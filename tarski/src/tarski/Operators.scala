package tarski

/**
 * Created by martin on 11.12.14.
 */
object Operators {
  sealed abstract class UnaryOp
  sealed abstract class ImpOp extends UnaryOp
  sealed abstract class NonImpOp extends UnaryOp
  case object PreDecOp extends ImpOp
  case object PreIncOp extends ImpOp
  case object PostDecOp extends ImpOp
  case object PostIncOp extends ImpOp
  case object PosOp extends NonImpOp
  case object NegOp extends NonImpOp
  case object CompOp extends NonImpOp
  case object NotOp extends NonImpOp

  sealed abstract class BinaryOp
  sealed abstract class AssignOp extends BinaryOp
  case object MulOp extends AssignOp
  case object DivOp extends AssignOp
  case object ModOp extends AssignOp
  case object AddOp extends AssignOp
  case object SubOp extends AssignOp
  case object LShiftOp extends AssignOp
  case object RShiftOp extends AssignOp
  case object UnsignedRShiftOp extends AssignOp
  case object LtOp extends BinaryOp
  case object GtOp extends BinaryOp
  case object LeOp extends BinaryOp
  case object GeOp extends BinaryOp
  case object EqOp extends BinaryOp
  case object NeOp extends BinaryOp
  case object AndOp extends AssignOp
  case object XorOp extends AssignOp
  case object OrOp extends AssignOp
  case object AndAndOp extends BinaryOp
  case object OrOrOp extends BinaryOp
}
