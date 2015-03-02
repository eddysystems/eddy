package com.eddysystems.eddy;

import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;
import tarski.Memory;

import javax.swing.*;

/**
* Created by martin on 02.02.15.
*/
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
    initialized = true;
  }

  static public void resetToDefaults() {
    data.setAutoApply(PreferenceData.defaultAutoApply);
    data.setAutoApplyThreshold(PreferenceData.defaultAutoApplyThreshold);
    data.setAutoApplyFactor(PreferenceData.defaultAutoApplyFactor);
    data.setMinProbability(PreferenceData.defaultMinProbability);
    data.setMinRelativeProbability(PreferenceData.defaultMinRelativeProbability);
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

    form.setData(data);

    // Save to file
    props.setValue("com.eddysystems.Props.autoApply", Boolean.toString(data.isAutoApply()), Boolean.toString(PreferenceData.defaultAutoApply));
    props.setValue("com.eddysystems.Props.autoApplyThreshold", data.getAutoApplyThreshold(), PreferenceData.defaultAutoApplyThreshold);
    props.setValue("com.eddysystems.Props.autoApplyFactor", data.getAutoApplyFactor(), PreferenceData.defaultAutoApplyFactor);
    props.setValue("com.eddysystems.Props.minProbability", data.getMinProbability(), PreferenceData.defaultMinProbability);
    props.setValue("com.eddysystems.Props.minRelativeProbability", data.getMinRelativeProbability(), PreferenceData.defaultMinRelativeProbability);

    // Log
    Memory.log(Memory.eddyProps(EddyPlugin.basics(null),
      data.isAutoApply(),data.getNumericAutoApplyThreshold(),data.getNumericAutoApplyFactor(),
      data.getNumericMinProbability(),data.getNumericMinRelativeProbability()));
  }

  @Override
  public void reset() {
    form.setData(data);
  }

  @Override
  public void disposeUIResources() {
  }

}
