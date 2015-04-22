package tarski

import utility.Utility._
import com.amazonaws.AmazonServiceException
import com.amazonaws.AmazonServiceException.ErrorType
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.testng.annotations.Test

class TestMemory {
  // A large random string for unit test purposes
  val install = "46240bbb2d11736a1a0fd86ac0d31c37bab17e13d02bde533668d0f8"
  val base = Memory.basics(install=install,version="0.1",project="TestMemory",ideaVersion="test")

  @Test def remember() = {
    val log = Memory.log(base.add("kind","TestMemory.remember").add("input","x = 3"), noLog=false)
    Await.result(log,10.second)
  }

  @Test def writeOnly(): Unit = {
    val get = Memory.table map { table => table.getItem("install",install,"time","1421091238.125") }
    try {
      Await.result(get,Duration.Inf)
      impossible // If get succeeds, we have a security problem
    } catch {
      case e:AmazonServiceException =>
        assert(e.getErrorType == ErrorType.Client)
        assert(e.getErrorMessage contains "eddy-public is not authorized to perform: dynamodb:GetItem", e.getErrorMessage)
    }
  }

  @Test def error() = {
    try throw new AssertionError("an assertion")
    catch { case e:Throwable =>
      val log = Memory.log(base.add("kind","TestMemory.error").error(e), noLog=false)
      Await.result(log,10.second)
    }
  }
}
