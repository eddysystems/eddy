package com.eddysystems.eddy;

import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.ValidationInfo;
import com.intellij.util.PathUtil;
import com.intellij.util.ResourceUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;

import static com.eddysystems.eddy.engine.Utility.log;

public class TOSDialog extends DialogWrapper {
  private JPanel contentPane;
  private JRadioButton loggingOkButton;
  private JRadioButton noLoggingButton;
  private JTextPane TOSTextPane;
  private JRadioButton noCodeLoggingButton;
  private ButtonGroup acceptGroup;

  public TOSDialog() {
    super(null, false, IdeModalityType.IDE);
    init();
    setTitle("eddy - Logging preferences");
    setCrossClosesWindow(false);
    // get TOS page out of our jar or directory
    final String pathname = PathUtil.getJarPathForClass(TOSDialog.class);
    final File path = new File(pathname);
    try {
      final URL url;
      if (path.isDirectory()) {
        url = new File(path, "intro.html").toURI().toURL();
      } else {
        url = ResourceUtil.getResource(TOSDialog.class, "", "intro.html");
      }
      TOSTextPane.setPage(url);
    } catch (MalformedURLException e) {
      throw new RuntimeException("Cannot load intro text. Please try reinstalling.", e);
    } catch (IOException e) {
      throw new RuntimeException("Cannot load intro text. Please try reinstalling.", e);
    }

    HyperlinkListener l = new HyperlinkListener() {
      @Override
      public void hyperlinkUpdate(HyperlinkEvent e) {
        log("got link event: " + e);
        if (HyperlinkEvent.EventType.ACTIVATED == e.getEventType()) {
          try {
            java.awt.Desktop.getDesktop().browse(e.getURL().toURI());
          } catch (IOException e1) {
            // wtf, do nothing
            log("exception: " + e1);
          } catch (URISyntaxException e2) {
            // broken link, do nothing
            log("exception: " + e2);
          }
        }
      }
    };
    TOSTextPane.addHyperlinkListener(l);
  }

  public PreferenceData.LogPreference logging() {
    if (acceptGroup.isSelected(loggingOkButton.getModel()))
      return PreferenceData.LogPreference.Normal;
    else if (acceptGroup.isSelected(noCodeLoggingButton.getModel()))
      return PreferenceData.LogPreference.NoCode;
    else
      return PreferenceData.LogPreference.NoLog;
  }

  @Override
  protected ValidationInfo doValidate() {
    if (acceptGroup.getSelection() == null) {
      return new ValidationInfo("Please choose whether and how eddy may log activity", noLoggingButton);
    }
    return null;
  }

  @Nullable
  @Override
  protected JComponent createCenterPanel() {
    return contentPane;
  }

  @NotNull
  @Override
  protected Action[] createActions() {
    return new Action[]{getOKAction()};
  }

  @Nullable
  protected ActionListener createCancelAction() {
    return null;//Prevent closing by <Esc>
  }
}
