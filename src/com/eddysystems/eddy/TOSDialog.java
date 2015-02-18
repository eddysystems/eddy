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
  private JRadioButton iAcceptTheTermsRadioButton;
  private JRadioButton iDoNotAcceptRadioButton;
  private JTextPane TOSTextPane;
  private ButtonGroup acceptGroup;

  public TOSDialog() {
    super(null, false, IdeModalityType.IDE);
    init();
    setTitle("eddy - Terms Of Service");
    setCrossClosesWindow(false);
    // get TOS page our of our jar or directory
    final String pathname = PathUtil.getJarPathForClass(TOSDialog.class);
    final File path = new File(pathname);
    try {
      final URL url;
      if (path.isDirectory()) {
        url = new File(path, "terms-of-service.html").toURI().toURL();
      } else {
        url = ResourceUtil.getResource(TOSDialog.class, "", "terms-of-service.html");
      }
      TOSTextPane.setPage(url);
    } catch (MalformedURLException e) {
      throw new RuntimeException("Cannot load terms of service. Please try reinstalling.", e);
    } catch (IOException e) {
      throw new RuntimeException("Cannot load terms of service. Please try reinstalling.", e);
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

  public boolean isAccepted() {
    return acceptGroup.isSelected(iAcceptTheTermsRadioButton.getModel());
  }

  @Override
  protected ValidationInfo doValidate() {
    if (acceptGroup.getSelection() == null) {
      return new ValidationInfo("Please accept or reject the Terms Of Service (actually, please accept them).", iDoNotAcceptRadioButton);
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
  @Override
  protected ActionListener createCancelAction() {
    return null;//Prevent closing by <Esc>
  }
}
