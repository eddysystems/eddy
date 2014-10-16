import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.codeInsight.hint.HintUtil;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.CaretEvent;
import com.intellij.openapi.editor.event.CaretListener;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.LightweightHint;
import org.apache.log4j.Level;

import javax.swing.*;

/**
 * Created by martin on 15.10.14.
 */
public class Eddy implements CaretListener, DocumentListener {
  private final Editor editor;
  private final Logger logger = Logger.getInstance(getClass());

  public Eddy(TextEditor editor) {
    logger.setLevel(Level.DEBUG);

    this.editor = editor.getEditor();

    VirtualFile file = FileDocumentManager.getInstance().getFile(this.editor.getDocument());
    if (file != null)
      logger.debug("making eddy for editor for file " + file.getPresentableName());
    else
      logger.debug("making eddy for editor for file 'null'");

    // moving the caret around
    this.editor.getCaretModel().addCaretListener(this);

    // subscribe to document events
    this.editor.getDocument().addDocumentListener(this);
  }

  public void dispose() {
    editor.getCaretModel().removeCaretListener(this);
    editor.getDocument().removeDocumentListener(this);
  }

  protected boolean enabled() {
    // only with a single caret can we deal...
    return editor.getCaretModel().getCaretCount() == 1;
  }

  /**
   * Shows a hint under the cursor with what eddy understood
   * @param line what eddy understood the current line meant
   */
  protected void showHint(String line) {
    JComponent hintcomponent = HintUtil.createInformationLabel("eddy says: " + line);
    LightweightHint hint = new LightweightHint(hintcomponent);
    HintManagerImpl hm = ((HintManagerImpl) HintManager.getInstance());
    hm.showEditorHint(hint, editor, HintManager.UNDER, HintManager.HIDE_BY_ANY_KEY | HintManager.HIDE_BY_TEXT_CHANGE | HintManager.HIDE_BY_SCROLLING, 0, false);
  }

  /**
   * An action performing one of the edits that eddy suggested
   */
  protected class EddyAcceptAction extends AnAction {
    EddyAcceptAction(String line) {
      super(line);
    };
    @Override
    public void actionPerformed(AnActionEvent actionEvent) {
      logger.info("eddy thingy accepted, event: " + actionEvent.toString());
    }
  }

  @Override
  public void caretPositionChanged(CaretEvent e) {
    if (!enabled())
      return;

    int lnum = e.getNewPosition().line;
    Document doc = editor.getDocument();
    String line = doc.getText(TextRange.create(doc.getLineStartOffset(lnum), doc.getLineEndOffset(lnum)));

    logger.debug("caret moved to " + e.getNewPosition().toString() + " current line: " + line);

    showHint(line);

    // this shows a popup, but it's way too intrusive. We need something like the hint, but have the options inside the
    // hint, only accessible by hotkeys (maybe ^arrows, ^enter, ^numbers)
    /*
    DataContext context = DataManager.getInstance().getDataContext(editor.getComponent());
    DefaultActionGroup actions = new DefaultActionGroup();
    actions.add(new EddyAcceptAction(line));
    actions.add(new EddyAcceptAction("noooooo!"));
    ListPopup popup = JBPopupFactory.getInstance().createActionGroupPopup(null, actions, context, JBPopupFactory.ActionSelectionAid.NUMBERING, false, null, 4);
    popup.showInBestPositionFor(context);
    */
  }

  @Override
  public void caretAdded(CaretEvent e) {
  }

  @Override
  public void caretRemoved(CaretEvent e) {
  }

  @Override
  public void beforeDocumentChange(DocumentEvent event) {
    logger.debug("before document change");
  }

  @Override
  public void documentChanged(DocumentEvent event) {
    logger.debug("document changed");
  }
}
