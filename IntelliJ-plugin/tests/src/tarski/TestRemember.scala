package tarski

import utility.Utility._
import com.amazonaws.AmazonServiceException
import com.amazonaws.AmazonServiceException.ErrorType
import tarski.Remember._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.testng.annotations.Test

class TestRemember {
  // A large random string for unit test purposes
  val install = "46240bbb2d11736a1a0fd86ac0d31c37bab17e13d02bde533668d0f8"
  val rem = new Remember(install)

  @Test def remember() = {
    val log = rem.log("x = 3")
    Await.result(log,10.second)
  }

  @Test def writeOnly(): Unit = {
    val get = rem.table map { table => table.getItem("install",install,"time","1421091238.125") }
    try {
      Await.result(get,Duration.Inf)
      impossible // If get succeeds, we have a security problem
    } catch {
      case e:AmazonServiceException =>
        assert(e.getErrorType == ErrorType.Client)
        assert(e.getErrorMessage contains "eddy-public is not authorized to perform: dynamodb:GetItem", e.getErrorMessage)
    }
  }
}
