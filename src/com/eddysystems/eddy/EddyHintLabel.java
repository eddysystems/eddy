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

  EddyHintLabel(@NotNull SimpleColoredComponent component) {
    this();
    setText(component);
  }

  public void setText(@NotNull SimpleColoredComponent colored) {
    clearText();

    myColored = colored;
    add(myColored, BorderLayout.CENTER);

    setOpaque(true);
    setBackground(colored.getBackground());

    revalidate();
    repaint();
  }

  public void setText(String s, HintHint hintHint) {
    clearText();

    if (s != null) {
      myPane = IdeTooltipManager.initPane(s, hintHint, null);
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

  public void setIcon(Icon icon) {
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

  protected static LightweightHint makeHint(Eddy.Output output) {
    boolean auto = output.shouldAutoApply();

    final String hintText, text = output.bestTextAbbrev() + (output.single() ? "" : " (multiple options...)");

    if (auto) {
      hintText = ' ' + text + ' ' + KeymapUtil.getFirstKeyboardShortcutText(ActionManager.getInstance().getAction(IdeActions.ACTION_EDITOR_ENTER));
    } else {
      hintText = ' ' + text + ' ' + KeymapUtil.getFirstKeyboardShortcutText(ActionManager.getInstance().getAction(IdeActions.ACTION_SHOW_INTENTION_ACTIONS));
    }

    HintHint hintHint = new HintHint().setTextBg(auto ? AUTOAPPLY_COLOR : QUESTION_COLOR)
      .setTextFg(JBColor.foreground())
      .setFont(UIUtil.getLabelFont().deriveFont(Font.BOLD))
      .setAwtTooltip(true);

    final EddyHintLabel label = new EddyHintLabel();
    label.setIcon(EddyWidget.getIcon());
    label.setText(hintText, hintHint);

    return new LightweightHint(label);
  }

}
