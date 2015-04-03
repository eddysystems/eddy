import com.amazonaws.util.json.JSONArray;
import com.amazonaws.util.json.JSONException;
import com.amazonaws.util.json.JSONObject;

public class Record {

  JSONObject obj;

  public Record(String line) throws JSONException {
    obj = new JSONObject(line);
  }

  public String timeString() throws JSONException { return obj.getString("time"); }
  public double time() throws JSONException { return Util.toTime(timeString()); }
  // old entries do not have a date field, in those cases, compute it from the time field
  public String dateString() throws JSONException {
    String date = obj.optString("date");
    if (date == null) {
      return Integer.toString(Util.toDate(time()));
    } else {
      return date;
    }
  }
  public int date() throws JSONException {
    String date = obj.optString("date");
    if ("".equals(date)) {
      return Util.toDate(time());
    } else {
      return Integer.parseInt(date);
    }
  }
  public String install() throws JSONException { return obj.getString("install"); }
  public String project() throws JSONException { return obj.getString("project"); }
  public String kind() {
    return obj.optString("kind", "Eddy.error"); // (some old error entries (up to 0.3) have no kind)
  }
  public String version() throws JSONException { return obj.getString("version"); }
  public String inputString() throws JSONException { // interpret null (=skip) as empty string.
    JSONArray input = obj.optJSONArray("input");
    if (input == null)
      return "";
    else
      return Util.show(input);
  }
  public String suggestion() throws JSONException { return obj.getString("suggestion"); }
}
