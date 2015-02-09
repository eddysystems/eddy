package com.eddysystems.eddy;

import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurationException;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

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

  static private void load() {
    data.setAutoApply(props.getBoolean("com.eddysystems.Props.autoApply", PreferenceData.defaultAutoApply));
    data.setAutoApplyThreshold(props.getValue("com.eddysystems.Props.autoApplyThreshold", PreferenceData.defaultAutoApplyThreshold));
    data.setAutoApplyFactor(props.getValue("com.eddysystems.Props.autoApplyFactor", PreferenceData.defaultAutoApplyFactor));
    data.setMinProbability(props.getValue("com.eddysystems.Props.minProbability", PreferenceData.defaultMinProbability));
    initialized = true;
  }

  @Override
  public void apply() throws ConfigurationException {
    form.getData(data);

    // check and correct form values here
    String correctedThreshold;
    try {
      correctedThreshold = String.format("%.2f%%", PreferenceData.toPercentage(data.getAutoApplyThreshold()));
    } catch (NumberFormatException e) {
      throw new ConfigurationException("auto apply threshold must be a number or percentage");
    }
    data.setAutoApplyThreshold(correctedThreshold);

    String correctedFactor;
    try {
      Double d = new Double(data.getAutoApplyFactor());
      if (d <= 1)
        throw new NumberFormatException();
      correctedFactor = String.format("%.2f",d);
    } catch (NumberFormatException e) {
      throw new ConfigurationException("auto apply factor must be a number >= 1");
    }
    data.setAutoApplyFactor(correctedFactor);

    String correctedMinProbability;
    try {
      correctedMinProbability = String.format("%f", PreferenceData.toNumber(data.getMinProbability()));
    } catch (NumberFormatException e) {
      throw new ConfigurationException("minimum probability must be a number or percentage");
    }
    data.setMinProbability(correctedMinProbability);

    form.setData(data);

    // save to file
    props.setValue("com.eddysystems.Props.autoApply", Boolean.toString(data.isAutoApply()), Boolean.toString(PreferenceData.defaultAutoApply));
    props.setValue("com.eddysystems.Props.autoApplyThreshold", data.getAutoApplyThreshold(), PreferenceData.defaultAutoApplyThreshold);
    props.setValue("com.eddysystems.Props.autoApplyFactor", data.getAutoApplyFactor(), PreferenceData.defaultAutoApplyFactor);
    props.setValue("com.eddysystems.Props.minProbability", data.getMinProbability(), PreferenceData.defaultMinProbability);
  }

  @Override
  public void reset() {
    form.setData(data);
  }

  @Override
  public void disposeUIResources() {
  }

}
