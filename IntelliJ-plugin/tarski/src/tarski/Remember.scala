package tarski

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import com.amazonaws.auth.{BasicAWSCredentials, AWSCredentials}
import com.amazonaws.services.cognitoidentity.AmazonCognitoIdentityClient
import com.amazonaws.services.cognitoidentity.model.{GetOpenIdTokenRequest, GetIdRequest, GetIdResult}
import com.amazonaws.services.securitytoken.AWSSecurityTokenServiceClient
import com.amazonaws.services.securitytoken.model.{Credentials, AssumeRoleWithWebIdentityRequest}
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient
import com.amazonaws.services.dynamodbv2.document.{DynamoDB, Item}

object Remember {
  // A large random string.  TODO
  val install = "46240bbb2d11736a1a0fd86ac0d31c37bab17e13d02bde533668d0f8"

  class Remember {
    // Authenticate: http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/Welcome.html
    //   GetId: http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_GetId.html
    //   AssumeRoleWithWebIdentity: http://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRoleWithWebIdentity.html
    private val cred: Future[AWSCredentials] = Future {
      val cognito = new AmazonCognitoIdentityClient
      val security = new AWSSecurityTokenServiceClient
      val id = cognito.getId(new GetIdRequest()
        .withAccountId("909287615191")
        .withIdentityPoolId("us-east-1:1e628723-d78f-4de3-9e1e-dd686855feff"))
      // TODO: open is valid for at most 15 minutes
      val open = cognito.getOpenIdToken(new GetOpenIdTokenRequest().withIdentityId(id.getIdentityId))
      val role = security.assumeRoleWithWebIdentity(new AssumeRoleWithWebIdentityRequest()
        .withWebIdentityToken(open.getToken))
      val cred = role.getCredentials
      new BasicAWSCredentials(cred.getAccessKeyId,cred.getSecretAccessKey)
    }

    // Grab table
    private val table = cred map { cred => new DynamoDB(new AmazonDynamoDBClient(cred)).getTable("eddy-log") }

    // Log to DynamoDB
    def log(input: String): Unit =
      table map { table =>
        val time = System.currentTimeMillis
        table.putItem(new Item()
          .withPrimaryKey("install",install,"time",time)
          .withString("input",input))
      }
  }
}
