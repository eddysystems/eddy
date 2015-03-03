import com.amazonaws.util.json.JSONArray;
import com.amazonaws.util.json.JSONException;
import com.amazonaws.util.json.JSONObject;
import com.sun.deploy.util.StringUtils;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

public class Stats {

  // don't consider logs and ignore all installations that appear before this date
  static Calendar launchCal = new GregorianCalendar(2015,Calendar.FEBRUARY,22,0,0,0);
  static double launchDate = launchCal.getTimeInMillis() * 1e-3;
  static double now = System.currentTimeMillis() * 1e-3;
  static int nhours = (int)((now - launchDate) / 3600.);

  static double toTime(String s) {
    return Double.parseDouble(s);
  }

  static int hourIndex(double time) {
    return (int)((time - launchDate) / (now - launchDate) * nhours);
  }

  // data about eddy actions
  static class ActionData {
    // total number of times this action appears in the log
    int count = 0;
  }

  static class InstallData {
    double activeSince = now;
    double lastActive = 0;

    Map<String,ActionData> actions = new HashMap<>();
  }

  static void process(String filename) {
    Map<String,ActionData> actionData = new HashMap<String,ActionData>();

    // data about each installation
    Map<String,InstallData> installData = new HashMap<String,InstallData>();

    // by hour activity stats
    int[] hourlyActivity = new int[nhours];

    final GregorianCalendar weekAgoC = new GregorianCalendar();
    weekAgoC.add(GregorianCalendar.WEEK_OF_YEAR, -1);
    final double weekAgo = weekAgoC.getTimeInMillis() * 1e-3;

    // read from filename
    try {
      BufferedReader fi = new BufferedReader(new FileReader(filename));
      String line;
      int lineNumber = 0;
      while ((line = fi.readLine()) != null) {
        lineNumber++;
        try {
          // parse JSON
          JSONObject obj = new JSONObject(line);

          // prefilter
          Set<String> preLaunchInstalls = new HashSet<>();

          String time = obj.getString("time");
          String install = obj.getString("install");

          double ts = toTime(time);

          // ignore actions before launch
          if (ts < launchDate) {
            preLaunchInstalls.add(install);
            continue;
          }

          if (ts >= now) {
            System.err.println("found time in the future (now = " + now + ") for entry: " + obj.toString(2));
            continue;
          }

          // ignore all installation ids that occur before launch
          if (preLaunchInstalls.contains(install)) {
            continue;
          }

          // ignore all IntelliJ tests (everything in which the project is light_temp_*)
          String project = obj.getString("project");
          if (project.startsWith("light_temp_")) {
            continue;
          }

          // ignore all tarski tests
          String action = obj.getString("kind");
          if (action.startsWith("TestMemory.")) {
            continue;
          }

          // global action count
          ActionData adata = actionData.get(action);
          if (adata == null) {
            adata = new ActionData();
            actionData.put(action, adata);
          }
          adata.count += 1;

          // install data
          InstallData idata = installData.get(install);
          if (idata == null) {
            idata = new InstallData();
            installData.put(install, idata);
          }
          idata.activeSince = Double.min(ts, idata.activeSince);
          idata.lastActive = Double.max(ts, idata.lastActive);

          // install action count
          ActionData iadata = idata.actions.get(action);
          if (iadata == null) {
            iadata = new ActionData();
            idata.actions.put(action,iadata);
          }
          iadata.count += 1;

          // activity
          hourlyActivity[hourIndex(ts)] += 1;

        } catch (JSONException e) {
          System.err.println("failed to parse line " + lineNumber + ": " + line + ": " + e);
        }
      }

      // print installation statistics
      System.out.println("all numbers exclude installation active before launch: " + launchCal.getTime().toString());
      System.out.println("total installations: " + installData.size());

      int wai = 0, wni = 0;
      for (final InstallData i : installData.values()) {
        if (i.lastActive > weekAgo)
          wai++;
        if (i.activeSince > weekAgo)
          wni++;
      }
      System.out.println("active installations last 7 days: " + wai);
      System.out.println("new installations last 7 days: " + wni);

      // print action statistics (this is pretty coarse, we may want this on a weekly basis too)
      int ta = 0;
      for (final Map.Entry<String, ActionData> e : actionData.entrySet()) {
        ta += e.getValue().count;
        System.out.println("number of " + e.getKey() + " actions: " + e.getValue().count);
      }
      System.out.println("total actions: " + ta);

      // print time based usage for visualization
      int[] dailyActivity = new int[nhours/24+1];
      for (int i = 0; i < nhours; ++i) {
        dailyActivity[i/24] += hourlyActivity[i];
      }
      JSONArray da = new JSONArray(dailyActivity);
      System.out.println("daily data (" + da.length() + " days): " + da.toString());

      JSONArray ha = new JSONArray(hourlyActivity);
      System.out.println("hourly data (" + ha.length() + " hours): " + ha.toString());

    } catch (Exception e) {
      error(e.toString());
    }
  }

  private static void error(String err) {
    System.err.println(err);
    System.exit(2);
  }

  private static void usage(String[] args) {
    System.out.println(
      "usage: stats <filename> (got " + StringUtils.join(Arrays.asList(args), " ") + ")\n" +
      "Compute metrics based on the DB data stored in <filename>. The data in the file is expected to have one row per line.\n" +
      "Each line should contain one JSON object, whose fields are the columns of the table.\n");
  }

  public static void main(String[] args) {
    if (args.length < 1) {
      usage(args);
      return;
    }
    String filename = args[0];
    process(filename);
  }
}
