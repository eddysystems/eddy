/* Memory: Log what eddy sees to a write-only Amazon DynamoDB */

package tarski

import tarski.Scores.Alt
import tarski.Tokens.{Token,abbrevShowFlags}
import tarski.Tarski.ShowStmts
import utility.Locations.{SLoc, Loc}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import com.amazonaws.auth.{BasicAWSCredentials,AWSCredentials}
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import com.amazonaws.services.dynamodbv2.document.{DynamoDB,Item}
import scala.collection.JavaConverters._
import scala.language.implicitConversions

object Memory {
  private implicit val showFlags = abbrevShowFlags

  // User credentials for eddy-public.  This user has permissions only for putItem into eddy-log.
  val cred: AWSCredentials = {
    val accessKeyId = "AKIAIQBAJAY2GBWGADPQ"
    val secretAccessKey = "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"
    new BasicAWSCredentials(accessKeyId,secretAccessKey)
  }

  // Grab table.  This doesn't touch the network, but we still wrap it in a Future to reduce user error.
  val table = Future { new DynamoDB(new AmazonDynamoDBClient(cred)).getTable("eddy-log") }

  def now(): Double = 1e-3*System.currentTimeMillis()

  // Conversions to types that can be safely consumed by Item.with
  class S(val x: Object) extends AnyVal
  object S { def apply(x: Object) = new S(x) }
  type Safe[-A] = A => S
  def safe[A](x: A)(implicit s: Safe[A]): Object = if (x == null) null else s(x).x
  type JList[A] = java.util.List[A]

  implicit def safeString(x: String): S = S(x)
  implicit def safeInt(x: Int): S = S(x:java.lang.Integer)
  implicit def safeBoolean(x: Boolean): S = S(x:java.lang.Boolean)
  implicit def safeDouble(x: Double): S = S(x:java.lang.Double)
  implicit def safeDouble(x: java.lang.Double): S = S(x)
  implicit def safeSeq[A](x: Seq[A])(implicit s: Safe[A]): S = S(x.map(safe(_)).asJava : java.util.List[Object])
  implicit def safeList[A](x: JList[A])(implicit s: Safe[A]): S = S(x.asScala.map(safe(_)).asJava)
  implicit def safeArray[A](x: Array[A])(implicit s: Safe[A]): S = {
    val y = new java.util.ArrayList[Object]
    x foreach (x => y.add(safe(x)))
    S(y)
  }
  implicit def safeSLoc(x: SLoc): S = S(x.raw:java.lang.Integer)
  implicit def safeLocated[A](x: Loc[A])(implicit s: Safe[A]): S = S(Map("x"->safe(x.x),"lo"->safe(x.r.lo),"hi"->safe(x.r.hi)).asJava)
  implicit def safeAlt[A](x: Alt[A])(implicit s: Safe[A]): S = S(Map("x"->safe(x.x),"p"->safe(x.p)).asJava)
  implicit def safeToken(x: Token): S = S(Map("c"->x.getClass.getName,"s"->x.show).asJava)
  implicit def safeException(x: Throwable): S = S(Map("c"->x.getClass.getName,"s"->x.getMessage).asJava)
  implicit def safeStack(x: StackTraceElement): S = S(x.toString)

  case class Info(install: String, fs: List[Item => Item]) {
    def itemNow(): Item = {
      val n = now()
      fs.foldRight((new Item).withPrimaryKey("install",install,"time",n)
        .withString("date",(n/86400).toInt.toString))(_(_))
    }
    def add[A](k: String, v: A)(implicit to: Safe[A]): Info = {
      val s = safe(v)
      Info(install,((i:Item) => i.`with`(k,s)) :: fs)
    }
    def error(e: Throwable): Info = if (e==null) this else add("error",e).add("stack",e.getStackTrace)
  }

  // Basic information about an install and a project
  def basics(install: String, version: String, project: String, ideaVersion: String): Info =
    Info(install,Nil).add("version",version)
                     .add("project",project)
                     .add("ideaVersion",ideaVersion)

  def eddyBase(base: Info, noCode: Boolean, start: Double, kind: String, line: Int, input: JList[Loc[Token]], results: JList[Alt[ShowStmts]]) = {
    // Use explicit types to enforce the format of the database
    val denotations: Seq[String] = if (results==null) null else results.asScala.map(_.x.den)
    val tokens: Seq[Alt[String]] = if (results==null) null else results.asScala.map(_ map (_.show))
    val formatted: Seq[String]   = if (results==null) null else results.asScala.map(_.x.abbrev)
    if (noCode)
      base.add("kind",kind)
          .add("start",start)
          .add("line",line)
    else
      base.add("kind",kind)
          .add("start",start)
          .add("line",line)
          .add("input", input)
          .add("results", tokens)
          .add("denotations", denotations) // same order as results
          .add("formatted",formatted) // same order as results
  }

  // Specific kinds of messages
  def eddyApply(base: Info, noCode: Boolean, start: Double, line: Int, input: JList[Loc[Token]], results: JList[Alt[ShowStmts]], choice: Int) =
    eddyBase(base, noCode, start, "Eddy.Apply", line, input, results)
      .add("choice",choice)

  def eddyAutoApply(base: Info, noCode: Boolean, start: Double, line: Int, input: JList[Loc[Token]], results: JList[Alt[ShowStmts]], choice: String) =
    eddyBase(base, noCode: Boolean, start, "Eddy.AutoApply", line, input, results)
      .add("choice",choice)

  def eddyProcess(base: Info, noCode: Boolean, start: Double, line: Int, input: JList[Loc[Token]], results: JList[Alt[ShowStmts]], delays: JList[java.lang.Double]) =
    eddyBase(base, noCode, start, "Eddy.process", line, input, results)
      .add("delay",delays)

  def eddyHint(base: Info, noCode: Boolean, start: Double, line: Int, input: JList[Loc[Token]], results: JList[Alt[ShowStmts]]) =
    eddyBase(base, noCode, start, "Eddy.hint", line, input, results)

  def eddySuggestion(base: Info, noCode: Boolean, start: Double, line: Int, input: JList[Loc[Token]], results: JList[Alt[ShowStmts]], suggestion: String) =
    eddyBase(base, noCode, start, "Eddy.suggestion", line, input, results)
      .add("suggestion",suggestion)

  def eddyProps(base: Info, autoApply: Boolean, autoApplyThreshold: Double, autoApplyFactor: Double,
                minProbability: Double, minRelativeProbability: Double, startDelay: Double, email: String, logPreference: String): Info =
    base.add("kind","Eddy.preferences")
        .add("autoApply",autoApply)
        .add("autoApplyThreshold",autoApplyThreshold)
        .add("autoApplyFactor",autoApplyFactor)
        .add("minProbability",minProbability)
        .add("minRelativeProbability",minRelativeProbability)
        .add("startDelay",startDelay)
        .add("email", if ("".equals(email)) "none" else email) // none is the only non-email string to ever appear here
        .add("logPreference", logPreference)

  def eddyError(base:Info, e: Throwable) =
    base.add("kind", "Eddy.error").error(e)

  // If we can't find a host, don't retry for a while
  private val waitAfterFail = 300.0 // 5 min
  private var lastFailedTime = Double.MinValue

  abstract class OnError {
    def error(i: Info, e: Throwable): Unit
  }

  // Log to DynamoDB
  //   PutItem: http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/JavaDocumentAPIItemCRUD.html#PutDocumentAPIJava
  def log(i: Info, noLog: Boolean, onError: OnError = null): Future[Unit] = if (!noLog) table map { table =>
    if (lastFailedTime < now()-waitAfterFail) {
      // Suppress exceptions to be quiet if the internet is down
      try table putItem i.itemNow()
      catch { case e: Throwable => {
        lastFailedTime = now()
        if (onError != null)
          onError.error(i,e)
      }}
    }
  } else Future[Unit]({}) // do nothing if no logging allowed
}
