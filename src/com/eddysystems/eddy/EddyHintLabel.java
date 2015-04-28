/* EddyHintLabel: Colored and icon-ed hint showing eddy's suggestion */

package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.Eddy;
import com.intellij.ide.IdeTooltipManager;
import com.intellij.openapi.actionSystem.ActionManager;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.keymap.KeymapUtil;
import com.intellij.ui.HintHint;
import com.intellij.ui.JBColor;
import com.intellij.ui.LightweightHint;
import com.intellij.ui.SimpleColoredComponent;
import com.intellij.util.ui.UIUtil;
import org.apache.commons.lang.StringEscapeUtils;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

import static utility.JavaUtils.safeEquals;
import static utility.JavaUtils.scalaList;

class EddyHintLabel extends JPanel {
  private JEditorPane myPane;
  private SimpleColoredComponent myColored;
  private JLabel myIcon;

  public static final Color QUESTION_COLOR = new JBColor(new Color(181, 208, 251), new Color(55, 108, 137));
  public static final Color AUTOAPPLY_COLOR = new JBColor(new Color(255, 187, 200), new Color(111, 32, 63)); //new Color(252, 77, 120));

  EddyHintLabel() {
    setLayout(new BorderLayout());
  }

  EddyHintLabel(final @NotNull SimpleColoredComponent component) {
    this();
    setText(component);
  }

  private void setText(final @NotNull SimpleColoredComponent colored) {
    clearText();

    myColored = colored;
    add(myColored, BorderLayout.CENTER);

    setOpaque(true);
    setBackground(colored.getBackground());

    revalidate();
    repaint();
  }

  private void setText(final String text, final HintHint hintHint) {
    clearText();

    if (text != null) {
      //final Html html = new Html(StringEscapeUtils.escapeHtml(text)).setKeepFont(true);
      myPane = IdeTooltipManager.initPane(StringEscapeUtils.escapeHtml(text), hintHint, null);
      add(myPane, BorderLayout.CENTER);
    }

    setOpaque(true);
    setBackground(hintHint.getTextBackground());

    revalidate();
    repaint();
  }

  private void clearText() {
    if (myPane != null) {
      remove(myPane);
      myPane = null;
    }

    if (myColored != null) {
      remove(myColored);
      myColored = null;
    }
  }

  private void setIcon(final Icon icon) {
    if (myIcon != null) {
      remove(myIcon);
    }

    myIcon = new JLabel(icon, SwingConstants.CENTER);
    myIcon.setVerticalAlignment(SwingConstants.TOP);

    add(myIcon, BorderLayout.WEST);

    revalidate();
    repaint();
  }

  @Override
  public String toString() {
    return "Hint: text='" + (myPane != null ? myPane.getText() : "") + '\'';
  }

  // These can be compared to see if two outputs will produce the same hint
  protected static Object signature(final Eddy.Output output) {
    return output==null ? null : !output.shouldShowHint() ? "no-show" : scalaList(
      output.shouldAutoApply(),
      output.shouldAutoApply() || output.single(),
      output.bestTextAbbrev());
  }

  public static boolean sameHint(final Eddy.Output output1, final Eddy.Output output2) {
    return safeEquals(signature(output1), signature(output2));
  }

  private static HintHint getHintHint(boolean auto) {
    return new HintHint()
            .setTextBg(auto ? AUTOAPPLY_COLOR : QUESTION_COLOR)
            .setTextFg(JBColor.foreground())
            .setFont(UIUtil.getLabelFont()
            .deriveFont(Font.BOLD))
            .setAwtTooltip(true);
  }

  private static String getHintText(final Eddy.Output output) {
    // If the hint specification changes, signature() must be changed as well
    final boolean auto = output.shouldAutoApply();
    final String text = output.bestTextAbbrev() + ((auto || output.single()) ? "" : " (multiple options...)");
    return ' ' + text + ' '
      + KeymapUtil.getFirstKeyboardShortcutText(ActionManager.getInstance().getAction(
          auto ? IdeActions.ACTION_EDITOR_ENTER : IdeActions.ACTION_SHOW_INTENTION_ACTIONS));
  }

  public void setOutput(final Eddy.Output output) {
    clearText();
    setText(getHintText(output), getHintHint(output.shouldAutoApply()));
  }

  protected static LightweightHint makeHint(final Eddy.Output output) {
    final EddyHintLabel label = new EddyHintLabel();
    label.setIcon(EddyWidget.getIcon());
    label.setOutput(output);

    return new LightweightHint(label);
  }

}
