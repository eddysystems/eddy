package com.eddysystems.eddy;

import com.intellij.openapi.options.Configurable;
import com.intellij.openapi.options.ConfigurableProvider;
import com.intellij.openapi.options.ConfigurationException;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class PreferencesProvider extends ConfigurableProvider {

  private static class Preferences implements Configurable {

    static private com.eddysystems.eddy.Preferences form = new com.eddysystems.eddy.Preferences();

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
      return false;
    }

    @Override
    public void apply() throws ConfigurationException {
    }

    @Override
    public void reset() {
    }

    @Override
    public void disposeUIResources() {
      form = null;
    }

  }

  private static Preferences eddyPreferences = new Preferences();

  @Nullable
  @Override
  public Configurable createConfigurable() {
    return eddyPreferences;
  }
}
