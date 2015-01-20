package com.eddysystems.eddy;

import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurableProvider;
import com.intellij.openapi.options.ConfigurationException;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class PreferencesProvider extends ConfigurableProvider {

  private static class Preferences implements Configurable {
    static boolean initialized = false;
    static final private PreferencesForm form = new PreferencesForm();
    static final private PropertiesComponent props = PropertiesComponent.getInstance();
    static final private PreferenceData data = new PreferenceData();

    Preferences() {
      // load data from file
      if (!initialized) {
        load();
        initialized = true;
      }
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

    private void load() {
      data.setAutoApply(props.getBoolean("com.eddysystems.Props.autoApply", false));
      data.setAutoApplyThreshold(props.getValue("com.eddysystems.Props.autoApplyThreshold", "90%"));
      data.setAutoApplyFactor(props.getValue("com.eddysystems.Props.autoApplyFactor", "10"));
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

      form.setData(data);

      // save to file
      props.setValue("com.eddysystems.Props.autoApply", Boolean.toString(data.isAutoApply()), Boolean.toString(false));
      props.setValue("com.eddysystems.Props.autoApplyThreshold", data.getAutoApplyThreshold(), "90%");
      props.setValue("com.eddysystems.Props.autoApplyFactor", data.getAutoApplyFactor(), "10");
    }

    @Override
    public void reset() {
      form.setData(data);
    }

    @Override
    public void disposeUIResources() {
    }

  }

  private static Preferences eddyPreferences = new Preferences();

  public static PreferenceData getData() { return Preferences.data; }

  @Nullable
  @Override
  public Configurable createConfigurable() {
    return eddyPreferences;
  }
}
