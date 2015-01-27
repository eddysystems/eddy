package tarski

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
  sealed abstract class CompareOp extends BinaryOp
  case object MulOp extends AssignOp
  case object DivOp extends AssignOp
  case object ModOp extends AssignOp
  case object AddOp extends AssignOp
  case object SubOp extends AssignOp
  case object LShiftOp extends AssignOp
  case object RShiftOp extends AssignOp
  case object UnsignedRShiftOp extends AssignOp
  case object LtOp extends CompareOp
  case object GtOp extends CompareOp
  case object LeOp extends CompareOp
  case object GeOp extends CompareOp
  case object EqOp extends CompareOp
  case object NeOp extends CompareOp
  case object AndOp extends AssignOp
  case object XorOp extends AssignOp
  case object OrOp extends AssignOp
  case object AndAndOp extends BinaryOp
  case object OrOrOp extends BinaryOp
}
