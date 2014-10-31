package tarski

import com.intellij.util.SmartList
import org.testng.annotations.{BeforeClass, Test}
import tarski.AST._
import tarski.Tarski.fix
import tarski.Environment.{JavaEnvironment, envFromFile}
import tarski.Items.{IntType, LocalVariableItemImpl}
import tarski.Tokens._

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class Tests {

  @BeforeClass
  def init(): Unit = {
    // this happens once

    // read default java environment from file
    // TODO
  }

  @Test
  def simpleExpression(): Unit = {
    val env = JavaEnvironment(List(new LocalVariableItemImpl("x", IntType)))
    println("environment: " + env)

    val tokens = new SmartList[Token](WhiteSpaceTok(), IdentTok("x"), WhiteSpaceTok(), EqTok(), WhiteSpaceTok(), IntLitTok("1"))

    val result = fix(tokens, env)
    val ast = ExpStmt(AssignExp(NameExp("x"), None, LitExp(IntLit("1"))))

    assert( result.asScala.toList.exists( p => p._1 == ast ))
  }
}
