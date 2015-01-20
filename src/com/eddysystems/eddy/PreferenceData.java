package com.eddysystems.eddy;

import org.jetbrains.annotations.NotNull;

public class PreferenceData {
  private boolean autoApply;
  private String autoApplyThreshold;
  private double numericAutoApplyThreshold;
  private String autoApplyFactor;
  private double numericAutoApplyFactor;

  public PreferenceData() {
  }

  public static double toNumber(@NotNull String s) throws NumberFormatException {
    boolean percentage = false;
    s = s.trim();
    if (s.endsWith("%")) {
      percentage = true;
      s = s.substring(0,s.length()-1);
    }
    double d = Double.parseDouble(s);

    if (percentage)
      d /= 100;

    return d;
  }

  public static double toPercentage(@NotNull String s) throws NumberFormatException {
    double d = 100*toNumber(s);

    if (d > 100) d = 100;
    else if (d < 0) d = 0;

    return d;
  }

  public double getNumericAutoApplyThreshold() {
    return numericAutoApplyThreshold;
  }

  public double getNumericAutoApplyFactor() {
    return numericAutoApplyFactor;
  }

  public boolean isAutoApply() {
    return autoApply;
  }

  public void setAutoApply(final boolean autoApply) {
    this.autoApply = autoApply;
  }

  public String getAutoApplyThreshold() {
    return autoApplyThreshold;
  }

  public void setAutoApplyThreshold(final String autoApplyThreshold) {
    this.autoApplyThreshold = autoApplyThreshold;
    try {
      this.numericAutoApplyThreshold = toNumber(autoApplyThreshold);
    } catch (NumberFormatException e) {
      this.numericAutoApplyThreshold = 1.;
    }
  }

  public String getAutoApplyFactor() {
    return autoApplyFactor;
  }

  public void setAutoApplyFactor(final String autoApplyFactor) {
    this.autoApplyFactor = autoApplyFactor;
    try {
      this.numericAutoApplyFactor = toNumber(autoApplyFactor);
    } catch (NumberFormatException e) {
      this.numericAutoApplyFactor = 10.;
    }
}
}