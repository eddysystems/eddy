package tarski

import org.testng.annotations.{BeforeClass, Test}
import tarski.AST._
import Types.binaryType
import tarski.Items.{DoubleType, IntType}

class Tests {

  @BeforeClass
  def init(): Unit = {
    // this happens once

    // read default java environment from file
    // TODO
  }

  @Test
  def expressionTypes(): Unit = {

    val b: Boolean = false
    val i: Int = 4
    val d: Double = 1.0
    val s: String = "blah"

    val vals = (b,i,d,s)
    val ops = (AddOp(),MulOp(),RShiftOp(),LtOp(),EqOp(),AndOp(),AndAndOp())

    assert((i + i).getClass.getName == binaryType(new AddOp(), IntType, IntType).get.qualifiedName)
    assert((d + i).getClass.getName == binaryType(new AddOp(), DoubleType, IntType).get.qualifiedName)
  }
}
