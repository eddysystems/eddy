package com.eddysystems.eddy;

import org.jetbrains.annotations.NotNull;

public class PreferenceData {
  private boolean autoApply;
  private String autoApplyThreshold;
  private double numericAutoApplyThreshold;
  private String autoApplyFactor;
  private double numericAutoApplyFactor;
  private String minProbability;
  private double numericMinProbability;

  public static final boolean defaultAutoApply = false;
  public static final String defaultAutoApplyThreshold = "90%";
  public static final String defaultAutoApplyFactor = "2";
  public static final String defaultMinProbability = "1e-6";

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
      this.numericAutoApplyThreshold = toNumber(PreferenceData.defaultAutoApplyThreshold);
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
      this.numericAutoApplyFactor = toNumber(PreferenceData.defaultAutoApplyFactor);
    }
}

  public String getMinProbability() {
    return minProbability;
  }

  public void setMinProbability(final String minProbability) {
    this.minProbability = minProbability;
    try {
      this.numericMinProbability = toNumber(minProbability);
    } catch (NumberFormatException e) {
      this.numericMinProbability = toNumber(PreferenceData.defaultMinProbability);
    }
  }
}