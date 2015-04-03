import com.amazonaws.util.json.JSONArray;
import com.amazonaws.util.json.JSONException;

public class Util {
  static double now = System.currentTimeMillis() * 1e-3;

  static double toTime(String s) {
    return Double.parseDouble(s);
  }

  static int toDate(double time) {
    return new Double(time/86400.).intValue();
  }

  static boolean sameIgnoringWhiteSpace(String input, String output) {
    return input.replaceAll("\\s*","").equals(output.replaceAll("\\s*", ""));
  }

  static String show(JSONArray tokens) throws JSONException {
    StringBuilder sb = new StringBuilder();

    for (int i = 0; i < tokens.length(); ++i)
      sb.append(tokens.getJSONObject(i).getJSONObject("x").getString("s"));

    return sb.toString();
  }
}
