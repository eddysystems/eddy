import com.intellij.codeInsight.completion.proc.VariablesProcessor;
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
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.scope.ElementClassHint;
import com.intellij.psi.scope.util.PsiScopesUtil;
import com.intellij.ui.LightweightHint;
import com.intellij.util.SmartList;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.List;

/**
 * Created by martin on 15.10.14.
 */
public class Eddy implements CaretListener, DocumentListener {
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final @NotNull Document document;
  private final @NotNull PsiFile psifile;
  private final @NotNull Logger logger = Logger.getInstance(getClass());

  public Eddy(Project project, TextEditor editor, @NotNull PsiFile psifile) {
    logger.setLevel(Level.DEBUG);

    this.project = project;
    this.editor = editor.getEditor();
    this.document = this.editor.getDocument();
    this.psifile = psifile;

    VirtualFile file = FileDocumentManager.getInstance().getFile(this.document);
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

  public PsiVariable[] getVariablesInScope(PsiElement where) {
    VariablesProcessor proc = new VariablesProcessor("", false);
    PsiScopesUtil.treeWalkUp(proc, where, null);
    return proc.getResultsAsArray();
  }

  public PsiClass[] getClassesInScope(PsiElement where) {
    class ClassesProcessor extends BaseScopeProcessor implements ElementClassHint {
      private final String prefix;
      private final List<PsiClass> results = new SmartList<PsiClass>();

      ClassesProcessor(String prefix) { this.prefix = prefix; }

      @Override
      public boolean shouldProcess(DeclarationKind kind) {
        return kind == DeclarationKind.CLASS;
      }

      @Override
      public boolean execute(@NotNull PsiElement element, ResolveState state) {
        if (!(element instanceof PsiClass))
          return true;

        final PsiClass aClass = (PsiClass)element;
        final String name = aClass.getName();
        if (name.startsWith(prefix) && !results.contains(aClass))
          results.add(aClass);

        // accessibility, private stuff
        //boolean accessible = myPlace == null || checkAccessibility(aClass);
        //if (!accessible) return true;
        //if (aClass.hasModifierProperty(PsiModifier.PRIVATE)) {
        //  final PsiClass containingPlaceClass = PsiTreeUtil.getParentOfType(myPlace, PsiClass.class, false);
        //  if (containingPlaceClass != null && !PsiTreeUtil.isAncestor(containingPlaceClass, aClass, false)){
        //    return true;
        //  }
        //}
        //return myCurrentFileContext instanceof PsiImportStatementBase;
        return true;
      }

      public PsiClass[] getResultsAsArray() {
        PsiClass[] res = new PsiClass[results.size()];
        return results.toArray(res);
      }
    }

    ClassesProcessor proc = new ClassesProcessor("");
    PsiScopesUtil.treeWalkUp(proc, where, null);
    return proc.getResultsAsArray();
  }

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

      logger.debug("variables in scope: ");
      for (final PsiVariable var : getVariablesInScope(elem)) {
        logger.debug("  " + var.getName() + ": " + var.getType().getCanonicalText());
      }
      logger.debug("classes in scope: ");
      for (final PsiClass cls : getClassesInScope(elem)) {
        logger.debug("  " + cls.getName() + " fq name " + cls.getQualifiedName());
      }
    } else
      logger.debug("PSI element not available.");

    showHint(line);

    /*
    // this shows a popup, but it's way too intrusive. We need something like the hint, but have the options inside the
    // hint, only accessible by hotkeys (maybe ^arrows, ^enter, ^numbers)
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
    //logger.debug("before document change");
  }

  @Override
  public void documentChanged(DocumentEvent event) {
    //logger.debug("document changed");
  }
}
