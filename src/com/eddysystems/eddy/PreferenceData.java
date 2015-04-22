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
  private String minRelativeProbability;
  private double numericMinRelativeProbability;
  private boolean removeQualifiers;
  private String startDelay;
  private double numericStartDelay;
  private String email;
  private String licenseCode;

  enum LogPreference {
    Normal, NoCode, NoLog;

    static public LogPreference fromString(String s) {
      return Normal.name().equals(s) ? Normal :
             NoCode.name().equals(s) ? NoCode :
             NoLog.name().equals(s) ? NoLog :
             defaultLogPreference; // default to default;
    }
  }
  private LogPreference logPreference;

  public static final boolean defaultAutoApply = true;
  public static final String defaultAutoApplyThreshold = "90%";
  public static final String defaultAutoApplyFactor = "3";
  public static final String defaultMinProbability = "1e-6";
  public static final String defaultMinRelativeProbability = "0.1%";
  public static final boolean defaultRemoveQualifiers = true;
  public static final String defaultStartDelay = "0.2";
  public static final LogPreference defaultLogPreference = LogPreference.Normal;

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
      this.numericAutoApplyThreshold = toNumber(defaultAutoApplyThreshold);
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
      this.numericAutoApplyFactor = toNumber(defaultAutoApplyFactor);
    }
}

  public double getNumericMinProbability() {
    return numericMinProbability;
  }

  public String getMinProbability() {
    return minProbability;
  }

  public void setMinProbability(final String minProbability) {
    this.minProbability = minProbability;
    try {
      this.numericMinProbability = toNumber(minProbability);
    } catch (NumberFormatException e) {
      this.numericMinProbability = toNumber(defaultMinProbability);
    }
  }

  public double getNumericMinRelativeProbability() {
    return numericMinRelativeProbability;
  }

  public String getMinRelativeProbability() {
    return minRelativeProbability;
  }

  public void setMinRelativeProbability(final String minRelativeProbability) {
    this.minRelativeProbability = minRelativeProbability;
    try {
      this.numericMinRelativeProbability = toNumber(minRelativeProbability);
    } catch (NumberFormatException e) {
      this.numericMinRelativeProbability = toNumber(defaultMinRelativeProbability);
    }
  }

  public boolean isRemoveQualifiers() {
    return removeQualifiers;
  }

  public void setRemoveQualifiers(final boolean removeQualifiers) {
    this.removeQualifiers = removeQualifiers;
  }

  public double getNumericStartDelay() {
    return numericStartDelay;
  }

  public String getStartDelay() {
    return startDelay;
  }

  public void setStartDelay(final String startDelay) {
    this.startDelay = startDelay;
    try {
      numericStartDelay = Double.parseDouble(startDelay.trim());
    } catch (NumberFormatException e) {
      numericStartDelay = Double.parseDouble(defaultStartDelay);
    }
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(final String email) {
    this.email = email;
  }

  public String getLicenseCode() {
    return licenseCode;
  }

  public void setLicenseCode(final String licenseCode) {
    this.licenseCode = licenseCode;
  }

  public boolean checkLicense() {
    return Preferences.checkLicense(this.licenseCode);
  }

  public LogPreference getLogPreference() {
    return logPreference;
  }

  public void setLogPreference(final LogPreference lp) {
    // check the license before allowing this (set to default otherwise)
    if (!checkLicense())
      this.logPreference = defaultLogPreference;
    else
      this.logPreference = lp;
  }
}