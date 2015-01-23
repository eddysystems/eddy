package tarski

import tarski.Denotations.Stmt
import tarski.Scores.Alt
import tarski.Tokens.{Token,abbrevShowFlags}
import tarski.Tarski.ShowStmt
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

  implicit def safeString(x: String) = S(x)
  implicit def safeDouble(x: Double) = S(x:java.lang.Double)
  implicit def safeDouble(x: java.lang.Double) = S(x)
  implicit def safeSeq[A](x: Seq[A])(implicit s: Safe[A]) = S(x.map(safe(_)).asJava : java.util.List[Object])
  implicit def safeList[A](x: JList[A])(implicit s: Safe[A]) = S(x.asScala.map(safe(_)).asJava)
  implicit def safeArray[A](x: Array[A])(implicit s: Safe[A]) = {
    val y = new java.util.ArrayList[Object]
    x foreach (x => y.add(safe(x)))
    S(y)
  }
  implicit def safeSLoc(x: SLoc) = S(x.raw:java.lang.Integer)
  implicit def safeLocated[A](x: Loc[A])(implicit s: Safe[A]) = S(Map("x"->safe(x.x),"lo"->safe(x.r.lo),"hi"->safe(x.r.hi)).asJava)
  implicit def safeAlt[A](x: Alt[A])(implicit s: Safe[A]) = S(Map("x"->safe(x.x),"p"->safe(x.p)).asJava)
  implicit def safeToken(x: Token) = S(Map("c"->x.getClass.getName,"s"->x.show).asJava)
  implicit def safeException(x: Throwable) = S(Map("c"->x.getClass.getName,"s"->x.getMessage).asJava)
  implicit def safeStack(x: StackTraceElement) = S(x.toString)

  case class Info(install: String, fs: List[Item => Item]) {
    def itemNow(): Item = fs.foldRight((new Item).withPrimaryKey("install",install,"time",now()))(_(_))
    def add[A](k: String, v: A)(implicit to: Safe[A]): Info = {
      val s = safe(v)
      Info(install,((i:Item) => i.`with`(k,s)) :: fs)
    }
    def error(e: Throwable): Info = if (e==null) this else add("error",e).add("stack",e.getStackTrace)
  }

  // Basic information about an install and a project
  def basics(install: String, version: String, project: String): Info =
    Info(install,Nil).add("version",version)
                     .add("project",project)

  def eddyApplyBase(base: Info, kind: String, input: JList[Loc[Token]], results: JList[Alt[JList[ShowStmt]]], choice: String) = {
    // Use explicit types to enforce the format of the database
    val denotations: Seq[Seq[String]] = if (results==null) null else results.asScala.map(_.x.asScala.map(_.den))
    val tokens: Seq[Alt[Seq[String]]] = if (results==null) null else results.asScala.map(_ map (_.asScala.map(_.show)))
    val formatted: Seq[Seq[String]]   = if (results==null) null else results.asScala.map(_.x.asScala map (_.abbrev))
    base.add("kind",kind)
        .add("input",input)
        .add("results",tokens)
        .add("denotations",denotations) // same order as results
        .add("formatted",formatted) // same order as results
        .add("choice",choice)
  }

  // Specific kinds of messages
  def eddyApply(base: Info, input: JList[Loc[Token]], results: JList[Alt[JList[ShowStmt]]], choice: String) = {
    eddyApplyBase(base, "Eddy.Apply", input, results, choice);
  }

  def eddyAutoApply(base: Info, input: JList[Loc[Token]], results: JList[Alt[JList[ShowStmt]]], choice: String) = {
    eddyApplyBase(base, "Eddy.AutoApply", input, results, choice);
  }

  def eddyProcess(base: Info, start: Double, input: JList[Loc[Token]], results: JList[Alt[JList[ShowStmt]]], delays: JList[java.lang.Double]) = {
    // Use explicit types to enforce the format of the database
    val denotations: Seq[Seq[String]] = if (results==null) null else results.asScala.map(_.x.asScala.map(_.den))
    val tokens: Seq[Alt[Seq[String]]] = if (results==null) null else results.asScala.map(_ map (_.asScala.map(_.show)))
    val formatted: Seq[Seq[String]]   = if (results==null) null else results.asScala.map(_.x.asScala map (_.abbrev))
    base.add("kind","Eddy.process")
        .add("start",start)
        .add("input",input)
        .add("results",tokens)
        .add("denotations",denotations) // same order as results
        .add("formatted",formatted) // same order as results
        .add("delay",delays)
  }

  // Log to DynamoDB
  //   PutItem: http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/JavaDocumentAPIItemCRUD.html#PutDocumentAPIJava
  def log(i: Info): Future[Unit] = table map { _ putItem i.itemNow() }
}