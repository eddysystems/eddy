package com.eddysystems.eddy;

import javax.swing.*;

public class PreferencesForm {

  private JCheckBox applyAutomaticallyOnEnterCheckBox;
  private JPanel panel;

  public JPanel getPanel() {
    return panel;
  }

  public void setData(PreferenceData data) {
    applyAutomaticallyOnEnterCheckBox.setSelected(data.isAutoApply());
  }

  public void getData(PreferenceData data) {
    data.setAutoApply(applyAutomaticallyOnEnterCheckBox.isSelected());
  }

  public boolean isModified(PreferenceData data) {
    if (applyAutomaticallyOnEnterCheckBox.isSelected() != data.isAutoApply()) return true;
    return false;
  }
}
