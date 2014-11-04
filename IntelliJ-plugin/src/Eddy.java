import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.codeInsight.hint.HintUtil;
import com.intellij.ide.DataManager;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.DefaultActionGroup;
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
import com.intellij.openapi.ui.popup.JBPopupFactory;
import com.intellij.openapi.ui.popup.ListPopup;
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
import tarski.Denotations.StmtDen;
import tarski.Scores.Score;
import tarski.Tarski;
import tarski.Tokens.Token;

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
   * @param text what eddy understood the current line meant
   */
  protected void showHint(String text) {
    //JComponent hintcomponent = HintUtil.createInformationLabel(text);
    JComponent hintcomponent = HintUtil.createQuestionLabel(text);
    LightweightHint hint = new LightweightHint(hintcomponent);
    HintManagerImpl hm = ((HintManagerImpl) HintManager.getInstance());
    try {
      hm.showEditorHint(hint, editor, HintManager.UNDER, HintManager.HIDE_BY_ANY_KEY | HintManager.HIDE_BY_TEXT_CHANGE | HintManager.HIDE_BY_SCROLLING, 0, false);
    } catch (NullPointerException e) {
      logger.error(e);
    }
  }

  /**
   * An action performing one of the edits that eddy suggested
   */
  protected class EddyAcceptAction extends AnAction {
    EddyAcceptAction(String line) {
      super(line);
    }
    @Override
    public void actionPerformed(AnActionEvent actionEvent) {
      logger.info("eddy thingy accepted, event: " + actionEvent.toString());
    }
  }

  /**
   * How to reformat code
   */
  /*
    CodeStyleManager codeStyleManager = CodeStyleManager.getInstance(project);
    result = (PsiMethod) codeStyleManager.reformat(result);

    JavaCodeStyleManager javaCodeStyleManager = JavaCodeStyleManager.getInstance(project);
    result = (PsiMethod) javaCodeStyleManager.shortenClassReferences(result);
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
      final List<TextRange> tokens_ranges = new SmartList<TextRange>();

      ((TreeElement) node).acceptTree(new RecursiveTreeElementVisitor() {
        @Override
        protected boolean visitNode(TreeElement element) {
          // if the element is not overlapping, don't output any of it
          if (!lrange.intersects(element.getTextRange())) {
            return false;
          }

          if (element instanceof LeafElement) {
            logger.debug("    node: " + element);
            tokens_ranges.add(element.getTextRange());
            tokens.add(Tokenizer.psiToTok(element));
          }

          return true;
        }
      });

      // compute range to be replaced
      TextRange tokens_range = TextRange.EMPTY_RANGE;
      for (TextRange range: tokens_ranges)
        tokens_range = tokens_range.union(range);

      EnvironmentProcessor env = new EnvironmentProcessor(project, elem, true);
      List<scala.Tuple2<Score,List<StmtDen>>> results = Tarski.fixJava(tokens, env.getJavaEnvironment());

      String text = "";

      // this shows a popup, but it's way too intrusive. We need something like the hint, but have the options inside the
      // hint, only accessible by hotkeys (maybe ^arrows, ^enter, ^numbers)
      DataContext context = DataManager.getInstance().getDataContext(editor.getComponent());
      DefaultActionGroup actions = new DefaultActionGroup();

      for (scala.Tuple2<Score,List<StmtDen>> interpretation : results) {
        text += "  Interpretation with score " + interpretation._1() + "<br/>";
        for (StmtDen meaning : interpretation._2()) {
          text += "  <q>" + meaning + "</q><br/>";
          actions.add(new EddyAcceptAction(meaning.toString()));
        }
      }
      
      ListPopup popup = JBPopupFactory.getInstance().createActionGroupPopup("Eddy thinks:", actions, context, JBPopupFactory.ActionSelectionAid.NUMBERING, false, null, 4);
      popup.showInBestPositionFor(context);

      logger.debug("eddy says:" + text);
      //showHint("<html><body>" + text + "</body></html>");
    }
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
