package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.Utility;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.ValidationInfo;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import java.awt.event.ActionListener;

public class EmailDialog extends DialogWrapper {
  private JTextPane weWouldLoveToTextPane;
  private JTextField emailTextField;
  private JPanel dialogPanel;

  public EmailDialog() {
    super(null,false, IdeModalityType.IDE);
    init();
    setTitle("eddy - Email Contact");
    Document doc = weWouldLoveToTextPane.getDocument();
    try {
      doc.insertString(0, "We would love to be able to contact you and ask for your feedback.\n\nPlease enter your email address if you'd like to participate (or leave blank if you don't).\n\nYou can always change or remove your email address in the plugin preferences.", new SimpleAttributeSet());
    } catch (BadLocationException e) {
      Utility.log("cannot instantiate email dialog: " + e);
      throw new RuntimeException(e);
    }
  }

  @Override
  protected ValidationInfo doValidate() {
    String email = emailTextField.getText();
    if (!"".equals(email) && !email.matches("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2}[a-zA-Z]*$"))
      return new ValidationInfo("Please enter a valid email address (or none at all)", emailTextField);
    return null;
  }

  @NotNull
  @Override
  protected Action[] createActions() {
    return new Action[]{getOKAction()};
  }

  @Nullable
  @Override
  protected JComponent createCenterPanel() {
    return dialogPanel;
  }

  @Nullable
  protected ActionListener createCancelAction() {
    return null;//Prevent closing by <Esc>
  }

  @Override
  protected void doOKAction() {
    processDoNotAskOnOk(OK_EXIT_CODE);

    String email = emailTextField.getText();
    Preferences.getData().setEmail(email);
    Preferences.resetForm();
    Preferences.save();

    if (getOKAction().isEnabled()) {
      close(OK_EXIT_CODE);
    }
  }
}
