/*
# Make sure not to over-use our read capacity: free read capacity is 25k/s
# for index lookups (stats-only), each item is
#   install key: 39ed747039a83c9280 -- 18 bytes
#   time stamp: 1422408151.549 -- Number: 8 bytes?
# so probably 24 bytes, meaning we should be able to read 1000 items per second
# so we will
# - connect to dynamoDB
# - use paginated queries to download 500 items at a time
# - if a request took less than a second, wait until the second is over
# - write the data into a csv file for later
*/

import com.amazonaws.auth.profile.ProfileCredentialsProvider;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.ReturnConsumedCapacity;
import com.amazonaws.services.dynamodbv2.model.ScanRequest;
import com.amazonaws.services.dynamodbv2.model.ScanResult;
import com.sun.deploy.util.StringUtils;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Map;

import static java.lang.Math.max;
import static java.lang.Thread.sleep;

public class Main {

  // retrieve only this many per request
  private static int maxItems = 500;
  // this many requests per second
  private static double maxRequestRate = 1;

  private static void error(String err) {
    System.err.println(err);
    System.exit(2);
  }

  private static void statScan(String filename) {
    AmazonDynamoDBClient client = new AmazonDynamoDBClient(new ProfileCredentialsProvider());

    try {
      PrintWriter fo = new PrintWriter(filename);

      int nrequests = 0;
      int count = 0, totalCount = 0;
      double consumed = 0;
      long start = System.nanoTime(), last=start;

      Map<String, AttributeValue> lastKeyEvaluated = null;
      do {
        ScanRequest scanRequest = new ScanRequest()
          .withTableName("stats-only")
          .withLimit(maxItems)
          .withExclusiveStartKey(lastKeyEvaluated);

        scanRequest.setReturnConsumedCapacity(ReturnConsumedCapacity.TOTAL);

        ScanResult result = client.scan(scanRequest);

        System.out.println("got " + result.getCount() + "/" + result.getScannedCount() + " items using " + result.getConsumedCapacity().getCapacityUnits() + " capacity");

        nrequests++;
        count += result.getCount();
        totalCount += result.getScannedCount();
        consumed += result.getConsumedCapacity().getCapacityUnits();

        for (Map<String, AttributeValue> item : result.getItems()) {
          fo.println(item.get("install") + "," + item.get("time"));
        }
        lastKeyEvaluated = result.getLastEvaluatedKey();

        long next = System.nanoTime();
        try {
          sleep((int) (max(0., 1000./maxRequestRate - 1e-6 * (next - last))));
        } catch (InterruptedException e) {
          error("interrupted.");
        }
        last=next;
      } while (lastKeyEvaluated != null);

      double time = 1e-9*(System.nanoTime() - start);

      System.out.println("retrieved " + count + "/" + totalCount + " records in " + time + "seconds, using " + nrequests + " requests and " + consumed + " capacity.");
    } catch (FileNotFoundException e) {
      error("could not create file " + filename);
    }
  }

  private static void fullScan(String filename) {
    error("full retrieval not implemented.");
  }

  private static void usage(String[] args) {
    System.out.println(
      "usage: accessDB stats|full <file> (got " + StringUtils.join(Arrays.asList(args)," ") + ")\n" +
      "Gets eddy usage data from dynamoDB and stores it in <file>.\n" +
      "  stats : get install IDs and times from the stats index and store as CSV\n" +
      "  full  : get all data and store as JSON. There's an additional guarantee that\n" +
      "          each row from the DB starts at a newline, and all rows except the first\n" +
      "          and last line are DB rows.");
  }

  public static void main(String[] args) {
    if (args.length < 2) {
      usage(args);
      return;
    }
    String filename = args[1];
    String command = args[0];

    if (command.equals("stats"))
      statScan(filename);
    else if (command.equals("full")) {
      fullScan(filename);
    } else {
      usage(args);
    }
  }
}
