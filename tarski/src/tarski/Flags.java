package tarski;

public class Flags {
  // Include nullary methods in byItem lookups.  Currently too slow to enable by default.
  public static final boolean nullaryMethods = false;

  // If true, failure causes are tracked via Bad.  If false, only Empty and Best are used.
  public static final boolean trackErrors = false;

  // Turn on to skip all approximate lookups
  public static final boolean exactOnly = false;
}
