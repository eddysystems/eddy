/* JavaParse: Parsing routines for Java literals */

package tarski;

import org.apache.commons.lang.StringUtils;

public class JavaParse {

  // Parse integer literals according to Java 7 syntax.
  // Binary notation and underscores are allowed; whitespace is not.
  // A valid literal is assumed, except that the trailing [lL] is optional.
  public static long parseLongLit(String s) {
    // Ignore underscores
    s = StringUtils.replace(s, "_", "");

    // Determine length, ignoring trailing l
    int n = s.length();
    final char end = s.charAt(n - 1);
    final boolean l = end == 'l' || end == 'L';
    if (l) n--;

    // Parse decimals
    if (n == 1 || s.charAt(0) != '0')
      return Long.parseLong(l?s.substring(0,n):s);

    // Parse hex, octal, binary
    final char t = s.charAt(1);
    if (t == 'x' || t == 'X') return Long.parseLong(s.substring(2,n),16); // Hexadecimal
    if (t == 'b' || t == 'B') return Long.parseLong(s.substring(2,n),2); // Binary
    return Long.parseLong(s.substring(1,n),8); // Octal
  }

  // Downgrade a possibly Java 7 long literal into Java 6
  public static String longLitToJava6(String s) {
    s = StringUtils.replace(s,"_","");
    final int n = s.length();
    if (n >= 3 && s.charAt(0) == '0') {
      final char t = s.charAt(1);
      if (t == 'b' || t == 'B') {
        final char end = s.charAt(n - 1);
        final boolean l = end == 'l' || end == 'L';
        s = "0x" + Long.toHexString(Long.parseLong(s.substring(2,l?n-1:n),2));
        if (t == 'B') s = s.toUpperCase(); // Preserve case
        if (l) s += end; // Preserve the trailing [lL]
      }
    }
    return s;
  }
}
