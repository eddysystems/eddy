/*
 Make sure not to over-use our read capacity: free read capacity is 25k/s
 for index lookups (stats-only), each item is
   install key: 39ed747039a83c9280 -- 18 bytes
   time stamp: 1422408151.549 -- Number: 8 bytes?
 so probably 24 bytes, meaning we should be able to read 1000 items per second.
 for full items, empirical data suggests we can read about 25 using a read capacity of 5, with spikes to 50,
 which leads to total average capacity usage of around 10
*/

import com.amazonaws.auth.profile.ProfileCredentialsProvider;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.services.dynamodbv2.model.*;
import com.amazonaws.util.json.JSONException;
import com.sun.deploy.util.StringUtils;

import java.io.*;
import java.util.*;

import static java.lang.Math.max;
import static java.lang.Thread.sleep;

public class GetData {

  // retrieve only this many per request
  private static int maxFullItems = 100;
  // this many requests per second
  private static double maxRequestRate = 1;

  private static void error(String err) {
    System.err.println(err);
    System.exit(2);
  }

  private static Object toObject(AttributeValue x) {
    Object obj = null;
    if (x.getS() != null) obj = x.getS(); // String
    if (x.getN() != null) obj = x.getN(); // String (number inside)
    if (x.getSS() != null) obj = x.getSS(); // List<String>
    if (x.getB() != null) obj = x.getB(); // ByteBuffer
    if (x.getNS() != null) obj = x.getNS(); // number set
    if (x.getBS() != null) obj = x.getBS(); // binary set
    if (x.getM() != null) { // map
      Map<String, AttributeValue> vs = x.getM();
      Map<String, Object> objs = new HashMap<>();
      for (final Map.Entry<String, AttributeValue> v : vs.entrySet())
        objs.put(v.getKey(), toObject(v.getValue()));
      obj = objs;
    }
    if (x.getL() != null) { // list
      List<AttributeValue> vs = x.getL();
      List<Object> objs = new ArrayList<>();
      for (final AttributeValue v : vs)
        objs.add(toObject(v));
      obj = objs;
    }
    if (x.getNULL() != null) obj = x.getNULL(); // boolean
    if (x.getBOOL() != null) obj = x.getBOOL(); // boolean
    return obj;
  }

  private static String toJSON(Map<String,AttributeValue> vs) {
    // make AttributeValue into an Item, then use toJSONPretty()
    Item item = new Item();
    for (final Map.Entry<String, AttributeValue> v : vs.entrySet()) {
      final String name = v.getKey();
      final Object obj = toObject(v.getValue());
      item.with(name, obj);
    }
    return item.toJSON();
  }

  private static void scan(String filename, boolean restart) {
    AmazonDynamoDBClient client = new AmazonDynamoDBClient(new ProfileCredentialsProvider());

    try {
      // retrieve only new objects (database is write only from the plugin, updates are guaranteed to not happen ever)
      int lastDate = 0;
      double lastTime = 0;

      if (!restart) {

        // find the last timestamp we saw in the data
        BufferedReader br = new BufferedReader(new FileReader(filename + ".json"));

        String line;
        int lineNumber = 0;
        do {
          lineNumber++;
          try {
            line = br.readLine();
          } catch (IOException e) {
            break;
          }

          if (line == null)
            break;

          // parse line as JSON
          try {
            Record record = new Record(line);

            // find max date and time fields
            double time = record.time();
            int date = record.date();

            lastDate = max(date, lastDate);
            lastTime = max(time, lastTime);

          } catch (JSONException e) {
            System.err.println("failed to parse line " + lineNumber + ": " + line + ": " + e);
          }
        } while (true);

        System.out.println("continuing, last date/time retrieved: " + lastDate + ", " + lastTime);

      } else {
        System.out.println("restarting from scratch, existing data will be deleted.");
      }

      // append to existing file
      PrintWriter full_fo = new PrintWriter(new FileOutputStream(filename + ".json", !restart));

      int nrequests = 0;
      int count = 0, totalCount = 0;
      double consumed = 0;
      long start = System.nanoTime(), last=start;
      Map<String, AttributeValue> lastKeyEvaluated = null;

      if (restart) {
        // for pagination
        do {
          ScanRequest scanRequest = new ScanRequest();
          scanRequest.withTableName("eddy-log").withLimit(maxFullItems);
          scanRequest.withExclusiveStartKey(lastKeyEvaluated);
          scanRequest.setReturnConsumedCapacity(ReturnConsumedCapacity.TOTAL);

          ScanResult result = client.scan(scanRequest);
          System.out.println("got " + result.getCount() + '/' + result.getScannedCount() + " items using " + result.getConsumedCapacity().getCapacityUnits() + " capacity");

          nrequests++;
          count += result.getCount();
          totalCount += result.getScannedCount();
          consumed += result.getConsumedCapacity().getCapacityUnits();

          for (Map<String, AttributeValue> item : result.getItems()) {
            full_fo.println(toJSON(item));
          }
          lastKeyEvaluated = result.getLastEvaluatedKey();

          if (lastKeyEvaluated != null) {
            System.out.println("last key: ");
            for (final Map.Entry<String, AttributeValue> e : lastKeyEvaluated.entrySet()) {
              System.out.println("  " + e.getKey() + ": " + e.getValue());
            }
          }

          long next = System.nanoTime();
          try {
            full_fo.flush();
            sleep((int) (max(0., 1000. / maxRequestRate - 1e-6 * (next - last))));
          } catch (InterruptedException e) {
            error("interrupted.");
          }
          last=next;
        } while (lastKeyEvaluated != null);

      } else {

        int today = new Double(Util.now/86400.).intValue();

        // ask for all days from the last one we saw to today (inclusive)
        for (int date = lastDate; date <= today; ++date) {

          System.out.println("getting items for date " + date + " (today is " + today + ')');

          do {
            QueryRequest query = new QueryRequest("eddy-log");
            query.withIndexName("date-index").withLimit(maxFullItems);
            query.withExclusiveStartKey(lastKeyEvaluated);
            query.setReturnConsumedCapacity(ReturnConsumedCapacity.TOTAL);

            Map<String,Condition> cond = new HashMap<>();
            cond.put("date", new Condition()
              .withComparisonOperator(ComparisonOperator.EQ)
              .withAttributeValueList(new AttributeValue(String.format(filename, "%d", date))));
            query.withKeyConditions(cond);

            QueryResult result = client.query(query);

            System.out.println("got " + result.getCount() + '/' + result.getScannedCount() + " items for date " + date + " using " + result.getConsumedCapacity().getCapacityUnits() + " capacity");

            nrequests++;
            count += result.getCount();
            totalCount += result.getScannedCount();
            consumed += result.getConsumedCapacity().getCapacityUnits();

            for (Map<String, AttributeValue> item : result.getItems()) {
              int used = 0;
              if (item.containsKey("time") && Double.parseDouble(item.get("time").getN()) > lastTime) {
                used++;
                full_fo.println(toJSON(item));
              }
              if (used < result.getCount()) {
                System.out.println("  used " + used + '/' + result.getCount() + " items.");
              }
            }

            long next = System.nanoTime();
            try {
              full_fo.flush();
              sleep((int) (max(0., 1000. / maxRequestRate - 1e-6 * (next - last))));
            } catch (InterruptedException e) {
              error("interrupted.");
            }
            last=next;

            lastKeyEvaluated = result.getLastEvaluatedKey();
          } while (lastKeyEvaluated != null);

        }

      }

      full_fo.close();

      double time = 1e-9*(System.nanoTime() - start);

      System.out.println("retrieved " + count + "/" + totalCount + " records in " + time + "seconds, using " + nrequests + " requests and " + consumed + " capacity");
    } catch (FileNotFoundException e) {
      error(e.toString());
    }
  }

  private static void usage(String[] args) {
    System.out.println(
      "usage: accessDB restart|continue <basename> (got " + StringUtils.join(Arrays.asList(args)," ") + ")\n" +
      "Gets eddy usage data from dynamoDB and stores it in <basename>.csv (for stats) and <basename>.json (for full data).\n" +
      "  restart : get all data. This overwrites any existing csv/json file of the same name.\n" +
      "  full    : get new data. The last index is read from <basename>.csv. If reading the last index fails, bail.\n");
  }

  public static void main(String[] args) {
    if (args.length < 2) {
      usage(args);
      return;
    }
    String filename = args[1];
    String command = args[0];

    if (command.equals("restart")) {
      scan(filename, true);
    } else if (command.equals("continue")) {
      scan(filename, false);
    } else {
      usage(args);
    }
  }
}
