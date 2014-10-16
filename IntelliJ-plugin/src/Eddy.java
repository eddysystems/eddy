import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.codeInsight.hint.HintUtil;
import com.intellij.lang.ASTNode;
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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.ui.LightweightHint;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * Created by martin on 15.10.14.
 */
public class Eddy implements CaretListener, DocumentListener {
  private final @NotNull Editor editor;
  private final @NotNull Document document;
  private final @NotNull PsiFile psifile;
  private final @NotNull Logger logger = Logger.getInstance(getClass());

  public Eddy(TextEditor editor, @NotNull PsiFile psifile) {
    logger.setLevel(Level.DEBUG);

    this.editor = editor.getEditor();
    this.document = this.editor.getDocument();
    this.psifile = psifile;

    VirtualFile file = FileDocumentManager.getInstance().getFile(this.document);
    if (file != null)
      logger.debug("making eddy for editor for file " + file.getPresentableName());
    else
      logger.debug("making eddy for editor for file 'null'");

    logger.debug("file type is " + psifile.getFileType().toString());

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
  /*
  protected class EddyAcceptAction extends AnAction {
    EddyAcceptAction(String line) {
      super(line);
    };
    @Override
    public void actionPerformed(AnActionEvent actionEvent) {
      logger.info("eddy thingy accepted, event: " + actionEvent.toString());
    }
  }
  */

  @Override
  public void caretPositionChanged(CaretEvent e) {
    if (!enabled())
      return;

    int lnum = e.getNewPosition().line;
    TextRange lrange = TextRange.create(document.getLineStartOffset(lnum), document.getLineEndOffset(lnum));
    int offset = document.getLineStartOffset(lnum) + e.getNewPosition().column;
    String line = document.getText(lrange);

    logger.debug("caret moved to " + e.getNewPosition().toString() + " current line: " + line);

    PsiElement elem = psifile.findElementAt(offset);
    if (elem != null) {
      ASTNode node = elem.getNode();
      logger.debug("AST node: " + node.toString() + ", contained in this line: " + lrange.contains(node.getTextRange()));
      node = node.getTreeParent();
      while (node != null) {
        logger.debug("  parent: " + node.toString() + ", contained in this line: " + lrange.contains(node.getTextRange()));
        node = node.getTreeParent();
      }
    } else
      logger.debug("PSI element not available.");

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
