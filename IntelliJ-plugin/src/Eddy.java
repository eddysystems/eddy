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
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.RecursiveTreeElementVisitor;
import com.intellij.psi.impl.source.tree.TreeElement;
import com.intellij.ui.LightweightHint;
import com.intellij.util.SmartList;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

import ambiguity.ParseEddy;
import tarski.Tokens.Token;
import tarski.AST;
import tarski.Tarski;

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

  protected void processLineAt(int lnum, int column) {
    int offset = document.getLineStartOffset(lnum) + column;
    final TextRange lrange = TextRange.create(document.getLineStartOffset(lnum), document.getLineEndOffset(lnum));
    String line = document.getText(lrange);

    logger.debug("processing at " + lnum + "/" + column);
    logger.debug("  current line: " + line);

    // whitespace is counted toward the next token/statement, so start at the beginning of the line
    PsiElement elem = psifile.findElementAt(document.getLineStartOffset(lnum));

    // if we hit whitespace, advance until we find something substantial, or leave the line
    if (elem instanceof PsiWhiteSpace) {
      elem = elem.getNextSibling();
      logger.debug("  found whitespace, next token " + elem);
      if (!lrange.intersects(elem.getTextRange())) {
        logger.debug("out of line range");
        elem = null;
      }
    }

    if (elem != null) {
      // parse beginning of the line to the end of the line
      ASTNode node = elem.getNode();

      // walk up the tree until the line is fully contained
      while (node != null && lrange.contains(node.getTextRange())) {
        logger.debug("  PSI node: " + node.getPsi() + ", contained in this line: " + lrange.contains(node.getTextRange()));
        node = node.getTreeParent();
      }

      // then walk the node subtree and output all tokens contained in any statement overlapping with the line
      // now, node is the AST node we want to interpret.
      if (node == null) {
        logger.warn("cannot find a node to look at.");
        return;
      }

      // get token stream for this node
      assert node instanceof TreeElement;

      final List<Token> tokens = new SmartList<Token>();

      ((TreeElement) node).acceptTree(new RecursiveTreeElementVisitor() {
        @Override
        protected boolean visitNode(TreeElement element) {
          // if the element is not overlapping, don't output any of it
          if (!lrange.intersects(element.getTextRange())) {
            return false;
          }

          if (element instanceof LeafElement) {
            logger.debug("    node: " + element);
            tokens.add(Tokenizer.psiToTok(element));
          }

          return true;
        }
      });

      EnvironmentProcessor env = new EnvironmentProcessor(elem, true);
      Tarski.fix(tokens,env.getJavaEnvironment());
    }

    /*
    PsiElement elem = psifile.findElementAt(offset);
    if (elem != null) {
      ASTNode node = elem.getNode();
      logger.debug("AST node: " + node.toString() + ", contained in this line: " + lrange.contains(node.getTextRange()));
      node = node.getTreeParent();
      while (node != null) {
        logger.debug("  parent: " + node.toString() + ", contained in this line: " + lrange.contains(node.getTextRange()));
        node = node.getTreeParent();
      }

      EnvironmentProcessor env = new EnvironmentProcessor(elem, true);

      logger.debug("variables in scope: ");
      for (final PsiVariable var : env.getVariables()) {
        if (var instanceof PsiLocalVariable)
          logger.debug("  local: " + var.getName() + ": " + var.getType().getCanonicalText());
        else if (var instanceof PsiParameter)
          logger.debug("  param: " + var.getName() + ": " + var.getType().getCanonicalText());
        else if (var instanceof PsiField) {
          logger.debug("  field: " + ((PsiField)var).getContainingClass().getQualifiedName() + "." + var.getName() + ": " + var.getType().getCanonicalText());
        } else {
          logger.debug("  other: " + var.getName() + ": " + var.getType().getCanonicalText() + " kind " + var.getClass().getSimpleName());
        }
      }
      logger.debug("methods in scope:");
      for (final PsiMethod m : env.getMethods()) {
        if (m.isConstructor())
          logger.debug("  " + m.getTypeParameterList().getText() + " " + m.getModifierList().getText().replace('\n', ' ') + " " + m.getContainingClass().getQualifiedName() + "." + m.getName() + m.getParameterList().getText());
        else
          logger.debug("  " + m.getTypeParameterList().getText() + " " + m.getModifierList().getText().replace('\n', ' ') + " " + m.getReturnType().getCanonicalText() + " " + m.getContainingClass().getQualifiedName() + "." + m.getName() + m.getParameterList().getText());
      }
      logger.debug("enums in scope:");
      for (final PsiEnumConstant en : env.getEnums()) {
        logger.debug("  " + en.getName() + ": " + en.getType());
      }
      logger.debug("classes in scope (excluding java.lang.*): ");
      for (final PsiClass cls : env.getClasses()) {
        if (cls.getQualifiedName() != null && cls.getQualifiedName().startsWith("java.lang"))
          continue;
        logger.debug("  " + cls.getQualifiedName());
      }
      logger.debug("packages in scope (excluding java.lang.*):");
      for (final PsiPackage pkg : env.getPackages()) {
        if (pkg.getQualifiedName().startsWith("java.lang"))
          continue;
        logger.debug("  " + pkg.getQualifiedName());
      }

    } else
      logger.debug("PSI element not available.");
    */

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
  public void caretPositionChanged(CaretEvent e) {
    if (!enabled())
      return;

    processLineAt(e.getNewPosition().line, e.getNewPosition().column);
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
