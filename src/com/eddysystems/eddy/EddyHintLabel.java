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
import com.intellij.util.ui.Html;
import com.intellij.util.ui.UIUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

class EddyHintLabel extends JPanel {
  private JEditorPane myPane;
  private SimpleColoredComponent myColored;
  private JLabel myIcon;

  public static final Color QUESTION_COLOR = new JBColor(new Color(181, 208, 251), new Color(55, 108, 137));
  public static final Color AUTOAPPLY_COLOR = new JBColor(new Color(255, 187, 200), new Color(252, 77, 120));

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
      Html html = new Html(text).setKeepFont(true);
      myPane = IdeTooltipManager.initPane(html, hintHint, null);
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

  protected static LightweightHint makeHint(final Eddy.Output output) {
    final boolean auto = output.shouldAutoApply();
    final String text = output.bestTextAbbrev() + (output.single() ? "" : " (multiple options...)");
    final String hintText = ' ' + text + ' '
      + KeymapUtil.getFirstKeyboardShortcutText(ActionManager.getInstance().getAction(
          auto ? IdeActions.ACTION_EDITOR_ENTER : IdeActions.ACTION_SHOW_INTENTION_ACTIONS));

    final HintHint hintHint = new HintHint()
            .setTextBg(auto ? AUTOAPPLY_COLOR : QUESTION_COLOR)
            .setTextFg(JBColor.foreground())
            .setFont(UIUtil.getLabelFont()
            .deriveFont(Font.BOLD))
            .setAwtTooltip(true);

    final EddyHintLabel label = new EddyHintLabel();
    label.setIcon(EddyWidget.getIcon());
    label.setText(hintText, hintHint);

    return new LightweightHint(label);
  }

}
