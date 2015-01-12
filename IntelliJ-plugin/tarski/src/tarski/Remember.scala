package tarski

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import com.amazonaws.auth.{BasicAWSCredentials,AWSCredentials}
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import com.amazonaws.services.dynamodbv2.document.{DynamoDB,Item}

object Remember {
  class Remember(val install: String) {
    // User credentials for eddy-public.  This user has permissions only for putItem into eddy-log.
    val cred: AWSCredentials = {
      val accessKeyId = "AKIAIQBAJAY2GBWGADPQ"
      val secretAccessKey = "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"
      new BasicAWSCredentials(accessKeyId,secretAccessKey)
    }

    // Grab table.  TODO: Refresh when temporary credentials expire?
    val table = Future { new DynamoDB(new AmazonDynamoDBClient(cred)).getTable("eddy-log") }

    // Log to DynamoDB
    //   PutItem: http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/JavaDocumentAPIItemCRUD.html#PutDocumentAPIJava
    def log(input: String): Future[Unit] =
      table map { table =>
        val time = 1e-3*System.currentTimeMillis()
        table.putItem(new Item()
          .withPrimaryKey("install",install,"time",time)
          .withString("input",input))
      }
  }
}
