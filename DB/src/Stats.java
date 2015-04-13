import com.amazonaws.util.StringUtils;
import com.amazonaws.util.json.JSONArray;
import com.amazonaws.util.json.JSONException;
import com.amazonaws.util.json.JSONObject;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.*;

public class Stats {

  // don't consider logs and ignore all installations that appear before this date
  static Calendar launchCal = new GregorianCalendar(2015,Calendar.FEBRUARY,22,0,0,0);
  static double launchDate = launchCal.getTimeInMillis() * 1e-3;
  static int nhours = (int)((Util.now - launchDate) / 3600.);
  static int ndays = nhours/24+1;

  static int dayIndex(double time) {
    return (int)((time-launchDate) / (Util.now - launchDate) * ndays);
  }
  static int hourIndex(double time) {
    return (int)((time - launchDate) / (Util.now - launchDate) * nhours);
  }

  // data about eddy actions
  static class ActionData {
    // total number of times this action appears in the log
    int count = 0;
  }

  static class EmailInfo implements Comparable<EmailInfo> {
    String email; // empty for removal
    double time;

    EmailInfo(String email, double time) {
      this.email = email;
      this.time = time;
    }

    @Override
    public int compareTo(EmailInfo o) {
      return Double.compare(time,o.time);
    }
  }

  static class InstallData {
    double activeSince = Util.now;
    double lastActive = 0; // last user action
    double lastSeen = 0; // last user or automatic action

    // mapped value is last seen timestamp
    Map<String,Double> versions = new HashMap<>();

    Map<String,ActionData> actions = new HashMap<>();

    // email info (sorted by time so we know what the most current info is)
    TreeSet<EmailInfo> emails = new TreeSet<>();
  }

  static class Suggestion {
    Suggestion(String i, double t, String in, String s) {
      install = i;
      time = t;
      input = in;
      suggestion = s;
    }

    final String install;
    final double time;
    final String input;
    final String suggestion;

    public String toString() {
      return install + ":" + Util.toDate(time) + " " + input + " => " + suggestion;
    }
  }

  static boolean wasShown(JSONObject obj) throws JSONException {
    assert obj.getString("kind").equals("Eddy.process");
    // check whether input is equal to output ignoring whitespace (the original works on tokens, which we don't have here,
    // but this should be close enough for statistics)
    JSONArray input = obj.optJSONArray("input");
    if (input == null)
      return false;
    JSONArray formatted = obj.optJSONArray("formatted");
    if (formatted == null)
      return false;
    for (int i = 0; i < formatted.length(); ++i) {
      String output = formatted.getString(i);
      if (Util.sameIgnoringWhiteSpace(Util.show(input), output)) {
        return true;
      }
    }
    return false;
  }

  static void process(String filename) {
    Map<String,ActionData> actionData = new HashMap<String,ActionData>();

    // data about each installation
    Map<String,InstallData> installData = new HashMap<String,InstallData>();

    // by hour activity stats
    HashSet<String>[] dailySeen = new HashSet[ndays];
    HashSet<String>[] dailyActive = new HashSet[ndays];
    int[] hourlyData = new int[nhours];
    int[] hourlyActivity = new int[nhours];

    // project names
    Set<String> projectNames = new HashSet<String>();

    // suggestions
    List<Suggestion> suggestions = new ArrayList<>();

    final GregorianCalendar weekAgoC = new GregorianCalendar();
    weekAgoC.add(GregorianCalendar.WEEK_OF_YEAR, -1);
    final double weekAgo = weekAgoC.getTimeInMillis() * 1e-3;

    // read from filename
    try {
      BufferedReader fi = new BufferedReader(new FileReader(filename));
      String line;
      int lineNumber = 0;
      int actionCount = 0;
      while ((line = fi.readLine()) != null) {
        lineNumber++;
        try {
          // parse JSON
          Record record = new Record(line);

          // prefilter
          Set<String> preLaunchInstalls = new HashSet<>();

          double ts = record.time();
          String install = record.install();

          // ignore actions before launch
          if (ts < launchDate) {
            preLaunchInstalls.add(install);
            continue;
          }

          if (ts >= Util.now) {
            System.err.println("found time in the future (now = " + Util.now + "), ignoring.");
            continue;
          }

          // ignore all installation ids that occur before launch
          if (preLaunchInstalls.contains(install)) {
            continue;
          }

          // ignore all tarski tests
          String action = record.kind();
          if (action.startsWith("TestMemory.")) {
            continue;
          }

          String project = record.project();

          // ignore all IntelliJ tests (everything in which the project is light_temp_*)
          if (project.startsWith("light_temp_")) {
            continue;
          }

          projectNames.add(project);

          // global action count
          ActionData adata = actionData.get(action);
          if (adata == null) {
            adata = new ActionData();
            actionData.put(action, adata);
          }
          adata.count += 1;
          actionCount += 1;

          if (action.equals("Eddy.process") && wasShown(record.obj)) {
            action = "Eddy.process (shown)";
            adata = actionData.get(action);
            if (adata == null) {
              adata = new ActionData();
              actionData.put(action, adata);
            }
            adata.count += 1;
          }

          if (action.equals("Eddy.suggestion")) {
            suggestions.add(new Suggestion(install, ts, record.inputString(), record.suggestion()));
          }

          // install data
          InstallData idata = installData.get(install);
          if (idata == null) {
            idata = new InstallData();
            installData.put(install, idata);
          }
          idata.activeSince = Double.min(ts, idata.activeSince);
          idata.lastSeen = Double.max(ts, idata.lastSeen);
          String version = record.version();
          double versionSeen = ts;
          if (idata.versions.containsKey(version)) {
            versionSeen = Double.max(ts,idata.versions.get(version));
          }
          idata.versions.put(version,versionSeen);

          if (action.equals("Eddy.preferences")) {
            // remember email data
            EmailInfo ed = new EmailInfo(record.email(), ts);
            if (idata.emails.isEmpty() || !idata.emails.last().email.equals(ed.email))
              idata.emails.add(ed);
          }

          // remember daily active installations
          HashSet<String> seenToday = dailySeen[dayIndex(ts)];
          if (seenToday == null) {
            seenToday = new HashSet<>();
            dailySeen[dayIndex(ts)] = seenToday;
          }
          seenToday.add(install);

          if (action.equals("Eddy.AutoApply") || action.equals("Eddy.Apply") || action.equals("Eddy.preferences") || action.equals("Eddy.suggestion")) {
            idata.lastActive = Double.max(ts, idata.lastActive);
            hourlyActivity[hourIndex(ts)] += 1;

            // remember daily active users
            HashSet<String> activeToday = dailyActive[dayIndex(ts)];
            if (activeToday == null) {
              activeToday = new HashSet<>();
              dailyActive[dayIndex(ts)] = activeToday;
            }
            activeToday.add(install);
          }

          // install action count
          ActionData iadata = idata.actions.get(action);
          if (iadata == null) {
            iadata = new ActionData();
            idata.actions.put(action,iadata);
          }
          iadata.count += 1;

          // activity
          hourlyData[hourIndex(ts)] += 1;

        } catch (JSONException e) {
          System.err.println("failed to parse line " + lineNumber + ": " + line + ": " + e);
        }
      }

      System.out.println("read " + lineNumber + " lines.");

      // print installation statistics
      System.out.println("all numbers exclude installation active before launch: " + launchCal.getTime().toString());
      System.out.println("total installations: " + installData.size());

      //int wai = 0, wni = 0, wau = 0;
      Set<String> emails = new HashSet<String>();
      for (final InstallData i : installData.values()) {
//        if (i.lastSeen > weekAgo)
//          wai++;
//        if (i.lastActive > weekAgo)
//          wau++;
//        if (i.activeSince > weekAgo)
//          wni++;
        if (!i.emails.isEmpty() && !i.emails.last().email.isEmpty())
          emails.add(i.emails.last().email);
      }
      //System.out.println("active installations last 7 days: " + wai);
      //System.out.println("active users last 7 days: " + wau);
      //System.out.println("new installations last 7 days: " + wni);

      // project statistics
      System.out.println("projects: " + projectNames.size());
      for (final String p : projectNames)
        System.out.println("  " + p);

      // collected emails
      System.out.println("collected emails: " + emails.size());
      for (final String e : emails)
        System.out.println(e);

      // suggestions
      System.out.println("suggestions: ");
      for (final Suggestion s : suggestions)
        System.out.println(s);

      // print action statistics (this is pretty coarse, we may want this on a weekly basis too)
      for (final Map.Entry<String, ActionData> e : actionData.entrySet()) {
        System.out.println("number of " + e.getKey() + " actions: " + e.getValue().count);
      }
      System.out.println("total actions: " + actionCount);
      System.out.println("ratio of hints to applies: " + (100. / actionData.get("Eddy.hint").count * (actionData.get("Eddy.Apply").count + actionData.get("Eddy.AutoApply").count)) + '%');

      // print time based usage for visualization
      int[] dailyActivity = new int[ndays];
      int[] dailyData = new int[ndays];
      for (int i = 0; i < nhours; ++i) {
        dailyActivity[i/24] += hourlyActivity[i];
        dailyData[i/24] += hourlyData[i];
      }
      JSONArray dd = new JSONArray(dailyData);
      System.out.println("daily data     (" + dd.length() + " days): " + dd.toString());
      JSONArray da = new JSONArray(dailyActivity);
      System.out.println("daily activity (" + da.length() + " days): " + da.toString());

      JSONArray hd = new JSONArray(hourlyData);
      System.out.println("hourly data     (" + hd.length() + " hours): " + hd.toString());
      JSONArray ha = new JSONArray(hourlyActivity);
      System.out.println("hourly activity (" + ha.length() + " hours): " + ha.toString());

      int[] daiData = new int[ndays];
      int[] dauData = new int[ndays];
      for (int i = 0; i < ndays; ++i) {
        daiData[i] = dailySeen[i] == null ? 0 : dailySeen[i].size();
        dauData[i] = dailyActive[i] == null ? 0 : dailyActive[i].size();
      }
      JSONArray dai = new JSONArray(daiData);
      System.out.println("daily active installations: " + dai.toString());
      JSONArray dau = new JSONArray(dauData);
      System.out.println("daily active users        : " + dau.toString());

      int[] waiData = new int[ndays-6];
      int[] wauData = new int[ndays-6];
      HashSet<String> tmpi = new HashSet<>();
      HashSet<String> tmpu = new HashSet<>();
      for (int i = 0; i < ndays-6; ++i) {
        tmpi.clear();
        tmpu.clear();
        for (int k = 0; k < 7; ++k) {
          if (dailySeen[i+k] != null)
            tmpi.addAll(dailySeen[i+k]);
          if (dailyActive[i+k] != null)
            tmpu.addAll(dailyActive[i+k]);
        }
        waiData[i] = tmpi.size();
        wauData[i] = tmpu.size();
      }

      JSONArray wai = new JSONArray(waiData);
      System.out.println("weekly active installations: " + wai.toString());
      JSONArray wau = new JSONArray(wauData);
      System.out.println("weekly active users        : " + wau.toString());

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
      "usage: stats <filename> (got " + StringUtils.join(" ", args) + ")\n" +
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
