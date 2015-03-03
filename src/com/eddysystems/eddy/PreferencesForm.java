package com.eddysystems.eddy;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class PreferencesForm {

  private JCheckBox applyAutomaticallyOnEnterCheckBox;
  private JPanel panel;
  private JTextField autoApplyThresholdTextField;
  private JTextField autoApplyFactorTextField;
  private JTextField minProbabilityTextField;
  private JTextField minRelativeProbability;
  private JButton resetButton;
  private JCheckBox removeQualifiersCheckBox;
  private JTextField startDelay;

  public PreferencesForm() {
    resetButton.addActionListener(new ActionListener() {
      @Override
      public void actionPerformed(ActionEvent actionEvent) {
        Preferences.resetToDefaults();
      }
    });
  }

  public JPanel getPanel() {
    return panel;
  }

  public void setData(PreferenceData data) {
    applyAutomaticallyOnEnterCheckBox.setSelected(data.isAutoApply());
    autoApplyThresholdTextField.setText(data.getAutoApplyThreshold());
    autoApplyFactorTextField.setText(data.getAutoApplyFactor());
    minProbabilityTextField.setText(data.getMinProbability());
    minRelativeProbability.setText(data.getMinRelativeProbability());
    removeQualifiersCheckBox.setSelected(data.isRemoveQualifiers());
    startDelay.setText(data.getStartDelay());
  }

  public void getData(PreferenceData data) {
    data.setAutoApply(applyAutomaticallyOnEnterCheckBox.isSelected());
    data.setAutoApplyThreshold(autoApplyThresholdTextField.getText());
    data.setAutoApplyFactor(autoApplyFactorTextField.getText());
    data.setMinProbability(minProbabilityTextField.getText());
    data.setMinRelativeProbability(minRelativeProbability.getText());
    data.setRemoveQualifiers(removeQualifiersCheckBox.isSelected());
    data.setStartDelay(startDelay.getText());
  }

  public boolean isModified(PreferenceData data) {
    if (applyAutomaticallyOnEnterCheckBox.isSelected() != data.isAutoApply()) return true;
    if (autoApplyThresholdTextField.getText() != null ? !autoApplyThresholdTextField.getText().equals(data.getAutoApplyThreshold()) : data.getAutoApplyThreshold() != null)
      return true;
    if (autoApplyFactorTextField.getText() != null ? !autoApplyFactorTextField.getText().equals(data.getAutoApplyFactor()) : data.getAutoApplyFactor() != null)
      return true;
    if (minProbabilityTextField.getText() != null ? !minProbabilityTextField.getText().equals(data.getMinProbability()) : data.getMinProbability() != null)
      return true;
    if (minRelativeProbability.getText() != null ? !minRelativeProbability.getText().equals(data.getMinRelativeProbability()) : data.getMinRelativeProbability() != null)
      return true;
    if (removeQualifiersCheckBox.isSelected() != data.isRemoveQualifiers()) return true;
    if (startDelay.getText() != null ? !startDelay.getText().equals(data.getStartDelay()) : data.getStartDelay() != null)
      return true;
    return false;
  }
}
