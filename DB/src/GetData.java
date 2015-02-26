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
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.text.ParseException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static java.lang.Math.max;
import static java.lang.Thread.sleep;

public class GetData {

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
      // retrieve only new objects (database is write only from the plugin, updates are guaranteed to not happen ever)
      Map<String, AttributeValue> lastKeyEvaluated = null;
      try {
        RandomAccessFile fi = new RandomAccessFile(filename,"r");

        // this is terribly inefficient, should really buffer. But our lines are short, so who cares.
        long fileLength = fi.length() - 1;
        StringBuilder sb = new StringBuilder();
        for (long filePointer = fileLength; filePointer != -1; filePointer--){
          fi.seek(filePointer);
          int readByte = fi.readByte();
          if (readByte == 0xA) {
            if (filePointer != fileLength)
              break;
          } else if(readByte == 0xD) {
            if( filePointer != fileLength - 1 )
            break;
          }
          sb.append((char) readByte);
        }

        // parse the line into install,time and make a key object
        String s = sb.reverse().toString().trim();

        System.out.println("Last key (raw): " + s);

        int comma = s.indexOf(',');

        if (comma == -1)
          throw new ParseException("s", comma);

        lastKeyEvaluated = new HashMap<>(2);
        lastKeyEvaluated.put("install", new AttributeValue().withS(s.substring(0,comma)));
        lastKeyEvaluated.put("time", new AttributeValue().withN(s.substring(comma+1)));
      } catch (Throwable t) {
        // can't get the last index? fine.
        System.err.println("Can't get the last key. Starting from scratch.");
      }

      // append to existing file
      PrintWriter fo = new PrintWriter(new FileOutputStream(filename,true));

      int nrequests = 0;
      int count = 0, totalCount = 0;
      double consumed = 0;
      long start = System.nanoTime(), last=start;

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

        for (Map<String, AttributeValue> item : result.getItems())
          fo.println(item.get("install").getS() + "," + item.get("time").getN());
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
