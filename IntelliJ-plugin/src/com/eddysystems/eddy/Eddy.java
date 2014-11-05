package com.eddysystems.eddy;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.RecursiveTreeElementVisitor;
import com.intellij.psi.impl.source.tree.TreeElement;
import com.intellij.util.SmartList;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;
import tarski.Denotations;
import tarski.Scores;
import tarski.Tarski;
import tarski.Tokens;
import tarski.Pretty;

import java.util.List;

public class Eddy {
  private final @NotNull Logger logger = Logger.getInstance(getClass());

  // all these are filled in process()
  // the range to be replaced
  private TextRange tokens_range;

  // information of where we were
  private PsiFile psifile = null;
  private Document document = null;
  private Editor editor = null;
  // the results of the interpretation
  private List<scala.Tuple2<Scores.Score,List<Denotations.Stmt>>> results;

  // a bias for which result is the best one (reset in process())
  private int resultOffset = 0;
  boolean selectedExplicitly = true;

  Eddy() {
    logger.setLevel(Level.WARN);
  }

  // applies a result by modifying the psifile
  public void apply(int i) {
    apply(code(i));
  }

  public void apply(final @NotNull String code) {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override public void run() {
        if (document != null) {
          // TODO: get correct replacement string
          int newoffset = tokens_range.getEndOffset() - tokens_range.getLength() + code.length();
          document.replaceString(tokens_range.getStartOffset(), tokens_range.getEndOffset()+1, code);
          editor.getCaretModel().moveToOffset(newoffset);
        }
      }
    });
  }

  public void apply(scala.Tuple2<Scores.Score,List<Denotations.Stmt>> r) {
    apply(code(r._2()));
  }

  public void applyBest() {
    apply(resultOffset);
  }

  public Editor getEditor() {
    return editor;
  }

  public List<scala.Tuple2<Scores.Score,List<Denotations.Stmt>>> getResults() {
    return results;
  }

  public void process(@NotNull Editor editor) {
    this.editor = editor;
    document = editor.getDocument();
    Project project = editor.getProject();

    if (results != null)
      results = null;

    if (project == null)
      return;

    psifile = PsiDocumentManager.getInstance(project).getPsiFile(document);

    if (psifile == null)
      return;

    // clear the offset
    resultOffset = 0;
    selectedExplicitly = false;

    int pos = editor.getCaretModel().getCurrentCaret().getOffset();
    int lnum = document.getLineNumber(pos);

    int column = pos - document.getLineStartOffset(lnum);
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

      final List<Tokens.Token> vtokens = new SmartList<Tokens.Token>();
      final List<TextRange> vtokens_ranges = new SmartList<TextRange>();

      ((TreeElement) node).acceptTree(new RecursiveTreeElementVisitor() {
        @Override
        protected boolean visitNode(TreeElement element) {
          // if the element is not overlapping, don't output any of it
          if (!lrange.intersects(element.getTextRange())) {
            return false;
          }

          if (element instanceof LeafElement) {
            logger.debug("    node: " + element);
            vtokens_ranges.add(element.getTextRange());
            vtokens.add(Tokenizer.psiToTok(element));
          }

          return true;
        }
      });

      List<Tokens.Token> tokens = vtokens;
      List<TextRange> tokens_ranges = vtokens_ranges;

      // remove leading and trailing whitespace
      while (!tokens.isEmpty() && tokens.get(0) instanceof Tokens.WhitespaceTok) {
        tokens = tokens.subList(1,tokens.size());
        tokens_ranges = tokens_ranges.subList(1,tokens.size());
      }
      while (!tokens.isEmpty() && tokens.get(tokens.size()-1) instanceof Tokens.WhitespaceTok) {
        tokens = tokens.subList(0,tokens.size()-1);
        tokens_ranges = tokens_ranges.subList(0,tokens.size()-1);
      }

      // compute range to be replaced
      tokens_range = TextRange.EMPTY_RANGE; // TextRange is broken. Argh.
      if (!tokens_ranges.isEmpty()) {
        tokens_range = tokens_ranges.get(0);
        for (TextRange range: tokens_ranges)
          tokens_range = tokens_range.union(range);
      }

      EnvironmentProcessor env = new EnvironmentProcessor(project, elem, true);
      results = Tarski.fixJava(tokens, env.getJavaEnvironment());

      String text = "";

      for (scala.Tuple2<Scores.Score,List<Denotations.Stmt>> interpretation : results) {
        text += "  Interpretation with score " + interpretation._1() + "<br/>";
        for (Denotations.Stmt meaning : interpretation._2()) {
          text += "  <q>" + meaning + "</q><br/>";
        }
      }

      logger.debug("eddy says:" + text);
    }

  }

  public boolean foundSomethingUseful() {
    // did we find useful meanings, and are those meanings different from what's already there?
    return results != null && !results.isEmpty();
  }

  public boolean single() {
    // is there only one realistic option (or did the user explicitly select one)?
    return results != null && results.size() == 1 || selectedExplicitly;
  }

  public boolean nextBestResult() {
    if (foundSomethingUseful() && !single()) {
      selectedExplicitly = true;
      resultOffset += 1;
      if (resultOffset == results.size())
        resultOffset = 0;
      return true;
    }
    return false;
  }

  public boolean prevBestResult() {
    if (foundSomethingUseful() && !single()) {
      selectedExplicitly = true;
      resultOffset -= 1;
      if (resultOffset < 0)
        resultOffset += results.size();
      return true;
    }
    return false;
  }

  private String code(int i) {
    return code(results.get(i)._2());
  }

  private String code(List<Denotations.Stmt> stmts) {
    return Tarski.pretty(stmts);
  }

  public String code(scala.Tuple2<Scores.Score,List<Denotations.Stmt>> res) {
    return code(res._2());
  }

  public String bestText() {
    assert foundSomethingUseful();
    return code(resultOffset);
  }
}
