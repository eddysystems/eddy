/* Preferences */

package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.Utility;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;
import tarski.Memory;

import javax.swing.*;

public class Preferences implements Configurable {
  static boolean initialized = false;
  static final private PreferencesForm form = new PreferencesForm();
  static final private PropertiesComponent props = PropertiesComponent.getInstance();
  static final private PreferenceData data = new PreferenceData();

  public Preferences() {
    // load data from file
    if (!initialized)
      load();
  }

  public static PreferenceData getData() {
    if (!initialized)
      load();
    return data;
  }

  public static boolean noCodeLog() {
    return getData().getLogPreference() == PreferenceData.LogPreference.NoCode;
  }

  public static boolean noLog() {
    return getData().getLogPreference() == PreferenceData.LogPreference.NoLog;
  }

  @Nls
  @Override
  public String getDisplayName() {
    return "Eddy";
  }

  @Nullable
  @Override
  public String getHelpTopic() {
    return null;
  }

  @Nullable
  @Override
  public JComponent createComponent() {
    return form.getPanel();
  }

  @Override
  public boolean isModified() {
    return form.isModified(data);
  }

  static public void load() {
    data.setAutoApply(props.getBoolean("com.eddysystems.Props.autoApply", PreferenceData.defaultAutoApply));
    data.setAutoApplyThreshold(props.getValue("com.eddysystems.Props.autoApplyThreshold", PreferenceData.defaultAutoApplyThreshold));
    data.setAutoApplyFactor(props.getValue("com.eddysystems.Props.autoApplyFactor", PreferenceData.defaultAutoApplyFactor));
    data.setMinProbability(props.getValue("com.eddysystems.Props.minProbability", PreferenceData.defaultMinProbability));
    data.setMinRelativeProbability(props.getValue("com.eddysystems.Props.minRelativeProbability", PreferenceData.defaultMinRelativeProbability));
    data.setRemoveQualifiers(props.getBoolean("com.eddysystems.Props.removeQualifiers", PreferenceData.defaultRemoveQualifiers));
    data.setStartDelay(props.getValue("com.eddysystems.Props.startDelay", PreferenceData.defaultStartDelay));
    data.setEmail(props.getValue("com.eddysystems.Props.email", ""));
    data.setLicenseCode(props.getValue("com.eddysystems.Props.licenseCode", "")); // must be set before logPreference is set
    data.setLogPreference(PreferenceData.LogPreference.fromString(props.getValue("com.eddysystems.Props.logPreference", PreferenceData.defaultLogPreference.name())));
    initialized = true;
  }

  static public void save() {
    // Save to file
    props.setValue("com.eddysystems.Props.autoApply", Boolean.toString(data.isAutoApply()), Boolean.toString(PreferenceData.defaultAutoApply));
    props.setValue("com.eddysystems.Props.autoApplyThreshold", data.getAutoApplyThreshold(), PreferenceData.defaultAutoApplyThreshold);
    props.setValue("com.eddysystems.Props.autoApplyFactor", data.getAutoApplyFactor(), PreferenceData.defaultAutoApplyFactor);
    props.setValue("com.eddysystems.Props.minProbability", data.getMinProbability(), PreferenceData.defaultMinProbability);
    props.setValue("com.eddysystems.Props.minRelativeProbability", data.getMinRelativeProbability(), PreferenceData.defaultMinRelativeProbability);
    props.setValue("com.eddysystems.Props.removeQualifier", Boolean.toString(data.isRemoveQualifiers()), Boolean.toString(PreferenceData.defaultRemoveQualifiers));
    props.setValue("com.eddysystems.Props.startDelay", data.getStartDelay(), PreferenceData.defaultStartDelay);
    props.setValue("com.eddysystems.Props.email", data.getEmail(), "");
    props.setValue("com.eddysystems.Props.licenseCode", data.getLicenseCode(), "");
    props.setValue("com.eddysystems.Props.logPreference", data.getLogPreference().name(), PreferenceData.defaultLogPreference.name());

    // if we ever set the email to something non-empty, don't ask for it later
    if (!"".equals(data.getEmail()))
      props.setValue("com.eddysystems.Props.emailRequested", "true");

    // Log
    Memory.log(Memory.eddyProps(EddyPlugin.basics(null),
      data.isAutoApply(), data.getNumericAutoApplyThreshold(), data.getNumericAutoApplyFactor(),
      data.getNumericMinProbability(), data.getNumericMinRelativeProbability(), data.isRemoveQualifiers(),
      data.getNumericStartDelay(), data.getEmail(), data.getLicenseCode(), data.getLogPreference().name()), Preferences.noLog(), Utility.onError);
  }

  static public void resetToDefaults() {
    data.setAutoApply(PreferenceData.defaultAutoApply);
    data.setAutoApplyThreshold(PreferenceData.defaultAutoApplyThreshold);
    data.setAutoApplyFactor(PreferenceData.defaultAutoApplyFactor);
    data.setMinProbability(PreferenceData.defaultMinProbability);
    data.setMinRelativeProbability(PreferenceData.defaultMinRelativeProbability);
    data.setRemoveQualifiers(PreferenceData.defaultRemoveQualifiers);
    data.setStartDelay(PreferenceData.defaultStartDelay);
    data.setLogPreference(PreferenceData.defaultLogPreference);

    // don't reset email or license key

    form.setData(data);
  }

  @Override
  public void apply() throws ConfigurationException {
    form.getData(data);

    // Check and correct form values
    try {
      data.setAutoApplyThreshold(String.format("%.2f%%", PreferenceData.toPercentage(data.getAutoApplyThreshold())));
    } catch (NumberFormatException e) {
      throw new ConfigurationException("auto apply threshold must be a number or percentage");
    }

    try {
      final Double d = new Double(data.getAutoApplyFactor());
      if (d <= 1)
        throw new NumberFormatException();
      data.setAutoApplyFactor(String.format("%.2f",d));
    } catch (NumberFormatException e) {
      throw new ConfigurationException("auto apply factor must be a number >= 1");
    }

    try {
      data.setMinProbability(String.format("%.2g", PreferenceData.toNumber(data.getMinProbability())));
    } catch (NumberFormatException e) {
      throw new ConfigurationException("minimum probability must be a number or percentage");
    }

    try {
      data.setMinRelativeProbability(String.format("%.2f%%", PreferenceData.toPercentage(data.getMinRelativeProbability())));
    } catch (NumberFormatException e) {
      throw new ConfigurationException("minimum relative probability must be a number or percentage");
    }

    try {
      final Double d = new Double(data.getStartDelay());
      if (d < 0 || d > 10)
        throw new NumberFormatException();
      data.setStartDelay(String.format("%.2f",d));
    } catch (NumberFormatException e) {
      throw new ConfigurationException("start delay must be a number between 0 and 10 (seconds)");
    }

    final String email = data.getEmail();
    if (!"".equals(email) && !email.matches("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2}[a-zA-Z]*$"))
      throw new ConfigurationException("Please enter a valid email address (or none at all)");

    // check the license, and if it's invalid, reset the data for logging to log all
    if (!"".equals(data.getLicenseCode()) && !data.checkLicense())
      throw new ConfigurationException("Partner/License code is invalid (leave blank if you don't have one)");

    // update the data on the form
    reset();

    // save to file
    save();
  }

  @Override
  public void reset() {
    form.setData(data);
  }

  public static void resetForm() { form.setData(data); }

  @Override
  public void disposeUIResources() {
  }

  public static boolean checkLicense(String licenseCode) {
    String lc = licenseCode.toLowerCase().replaceAll("[^0-9a-z]","");
    return "433707af69bf79af15f9".equals(lc) || // Luciad
           "9fc87cbff524e24315e4".equals(lc); // reddit
  }

}
