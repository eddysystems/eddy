package com.eddysystems.eddy;

import com.eddysystems.eddy.actions.EddyAction;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.ValidationInfo;
import org.jetbrains.annotations.Nullable;
import tarski.Tokens;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class CorrectionDialog extends DialogWrapper {
  private JPanel contentPane;
  private JTextArea eddyInput;
  private JList eddyOutputList;
  private JTextArea eddyOutput;
  private JTextArea suggestion;

  public CorrectionDialog(Project project, final EddyAction action) {
    super(project, false, IdeModalityType.PROJECT);
    init();
    setTitle("Help Eddy Out!");
    if (action != null && action.getOutput().results != null) {
      eddyInput.setText(action.getOutput().input.getText());
      eddyOutputList.setListData(action.getOutput().getResultSummary());
    } else {
      // TODO: get current line to put into input field
    }

    eddyOutputList.addListSelectionListener(new ListSelectionListener() {
      @Override
      public void valueChanged(ListSelectionEvent e) {
        if (e.getValueIsAdjusting())
          return;
        if (action != null && action.getOutput().results != null && action.getOutput().results.size() > e.getFirstIndex()) {
          eddyOutput.setText(action.getOutput().format(eddyOutputList.getSelectedIndex(),new Tokens.ShowFlags(false, false, true)));
        }
      }
    });

    if (eddyOutputList.getModel().getSize() > 0) {
      eddyOutputList.setSelectedIndex(0);
    } else {
      eddyOutput.setText("no solution found.");
    }
  }

  @Nullable
  @Override
  protected JComponent createCenterPanel() {
    return contentPane;
  }

  @Override
  public JComponent getPreferredFocusedComponent() {
    return suggestion;
  }

  @Override
  protected ValidationInfo doValidate() {
    // too much information
    if (suggestion.getText().length() > 5000) {
      return new ValidationInfo("Too much information!", suggestion);
    }
    if (suggestion.getText().trim().isEmpty()) {
      return new ValidationInfo("Please suggest something...", suggestion);
    }
    return null;
  }

  public String getSuggestion() {
    return suggestion.getText();
  }

  // TODO: should override createActions to get rid of help button, or make help dialog or something
}
