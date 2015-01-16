package tarski

import tarski.Scores.Alt
import tarski.Tokens.{Token,show}
import utility.Locations.{SLoc, Located}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import com.amazonaws.auth.{BasicAWSCredentials,AWSCredentials}
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import com.amazonaws.services.dynamodbv2.document.{DynamoDB,Item}
import scala.collection.JavaConverters._
import scala.language.implicitConversions

object Memory {
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

  implicit def safeString(x: String) = S(x)
  implicit def safeDouble(x: Double) = S(x:java.lang.Double)
  implicit def safeList[A](x: JList[A])(implicit s: Safe[A]) = S(x.asScala.map(safe(_)).asJava)
  implicit def safeArray[A](x: Array[A])(implicit s: Safe[A]) = {
    val y = new java.util.ArrayList[Object]
    x foreach (x => y.add(safe(x)))
    S(y)
  }
  implicit def safeSLoc(x: SLoc) = S(x.x:java.lang.Integer)
  implicit def safeLocated[A](x: Located[A])(implicit s: Safe[A]) = S(Map("x"->safe(x.x),"lo"->safe(x.r.lo),"hi"->safe(x.r.hi)).asJava)
  implicit def safeAlt[A](x: Alt[A])(implicit s: Safe[A]) = S(Map("x"->safe(x.x),"p"->safe(x.p)).asJava)
  implicit def safeToken(x: Token) = S(Map("c"->x.getClass.getName,"s"->show(x)).asJava)
  implicit def safeException(x: Throwable) = S(Map("c"->x.getClass.getName,"s"->x.getMessage).asJava)
  implicit def safeStack(x: StackTraceElement) = S(x.toString)

  case class Info(install: String, fs: List[Item => Item]) {
    def itemNow(): Item = fs.foldRight((new Item).withPrimaryKey("install",install,"time",now()))(_(_))
    def add[A](k: String, v: A)(implicit to: Safe[A]): Info = {
      val s = safe(v)
      Info(install,((i:Item) => i.`with`(k,s)) :: fs)
    }
    def error(e: Throwable): Info = add("error",e).add("stack",e.getStackTrace)
  }

  // Basic information about an install and a project
  def basics(install: String, version: String, project: String): Info =
    Info(install,Nil).add("version",version)
                     .add("project",project)

  // Specific kinds of messages
  def eddyApply(base: Info, input: JList[Located[Token]], results: JList[Alt[JList[String]]], choice: String) =
    base.add("kind","Eddy.apply")
        .add("input",input)
        .add("results",results)
        .add("choice",choice)

  def eddyProcess(base: Info, start: Double, input: JList[Located[Token]], results: JList[Alt[JList[String]]]) =
    base.add("kind","Eddy.process")
        .add("start",start)
        .add("input",input)
        .add("results",results)

  // Log to DynamoDB
  //   PutItem: http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/JavaDocumentAPIItemCRUD.html#PutDocumentAPIJava
  def log(i: Info): Future[Unit] = table map { _ putItem i.itemNow() }
}
