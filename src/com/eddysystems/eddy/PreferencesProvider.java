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
    }

    @Override
    public void apply() throws ConfigurationException {
      form.getData(data);

      // save to file
      props.setValue("com.eddysystems.Props.autoApply", Boolean.toString(data.isAutoApply()), Boolean.toString(false));
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

  @Nullable
  @Override
  public Configurable createConfigurable() {
    return eddyPreferences;
  }
}
