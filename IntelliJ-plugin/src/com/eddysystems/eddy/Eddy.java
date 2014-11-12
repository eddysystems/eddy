package com.eddysystems.eddy;

import com.intellij.lang.ASTNode;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.RecursiveTreeElementVisitor;
import com.intellij.psi.impl.source.tree.TreeElement;
import com.intellij.util.SmartList;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;
import tarski.*;

import java.util.List;

public class Eddy {
  private final @NotNull Logger logger = Logger.getInstance(getClass());

  // all these are filled in process()
  // the range to be replaced
  private TextRange tokens_range;

  // information of where we were
  private PsiFile psifile = null;
  private Document document = null;
  private Project project = null;
  private Editor editor = null;
  private PsiElement place = null;
  // the results of the interpretation
  private Environment.Env env = null;
  private List<scala.Tuple2<Scores.Prob,List<Denotations.Stmt>>> results;
  private List<String> resultStrings;
  private boolean found_existing;

  // a bias for which result is the best one (reset in process())
  private int resultOffset = 0;
  boolean selectedExplicitly = true;

  Eddy() {
    logger.setLevel(Level.DEBUG);
  }

  // applies a result by modifying the psifile
  public void apply(int i) {
    apply(code(i));
  }

  public void apply(final @NotNull String code) {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        new WriteCommandAction(project, psifile) {
          @Override
          public void run(@NotNull Result result) {
            if (document != null) {
              int newoffset = tokens_range.getEndOffset() - tokens_range.getLength() + code.length();
              logger.debug("replacing '" + document.getText(tokens_range) + "' with '" + code + "'");
              document.replaceString(tokens_range.getStartOffset(), tokens_range.getEndOffset() - 1, code);
              editor.getCaretModel().moveToOffset(newoffset);
              PsiDocumentManager.getInstance(project).commitDocument(document);
              // if we apply, we know we shouldn't show again
              found_existing = true;
            }
          }
        }.execute();
      }
    });
  }

  public void applyBest() {
    apply(resultOffset);
  }

  public Editor getEditor() {
    return editor;
  }

  public List<String> getResultStrings() { return resultStrings; }

  public void dumpEnvironment(String filename) {
    if (env != null)
      Environment.envToFile(env,filename);
  }

  public void process(@NotNull Editor editor) {
    this.editor = editor;
    document = editor.getDocument();
    project = editor.getProject();

    if (results != null)
      results = null;

    if (project == null)
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

    psifile = PsiDocumentManager.getInstance(project).getPsiFile(document);

    if (psifile == null)
      return;

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
            logger.debug("    node: " + element + " " + element.getTextRange() + " -> " + Tokenizer.psiToTok(element));
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
        tokens_ranges = tokens_ranges.subList(1,tokens_ranges.size());
      }
      while (!tokens.isEmpty() && tokens.get(tokens.size()-1) instanceof Tokens.WhitespaceTok) {
        tokens = tokens.subList(0,tokens.size()-1);
        tokens_ranges = tokens_ranges.subList(0,tokens_ranges.size()-1);
      }

      // compute range to be replaced
      tokens_range = TextRange.EMPTY_RANGE; // TextRange is broken. Argh.
      if (!tokens_ranges.isEmpty()) {
        tokens_range = tokens_ranges.get(0);
        for (TextRange range: tokens_ranges) {
          tokens_range = tokens_range.union(range);
        }
      }

      String before_text = document.getText(tokens_range);

      place = elem;
      env = (new EnvironmentProcessor(project, place, true)).getJavaEnvironment();
      results = Tarski.fixJava(tokens, env);

      found_existing = false;
      resultStrings = new SmartList<String>();
      for (scala.Tuple2<Scores.Prob,List<Denotations.Stmt>> interpretation : results) {
        // for each interpretation, compute a string
        if (interpretation._2().isEmpty()) {
          resultStrings.add("");
        } else {
          String s = "";
          for (Denotations.Stmt meaning : interpretation._2()) {
            s = s + code(meaning) + " ";
          }
          s = s.substring(0,s.length()-1); // remove trailing space
          resultStrings.add(s);
          logger.debug("eddy result: '" + s + "' existing '" + before_text + "'");
          if (s.equals(before_text)) {
            found_existing = true;
          }
        }
      }
    }
  }

  public boolean foundSomethingUseful() {
    // did we find useful meanings, and are those meanings different from what's already there?
    return !found_existing && results != null && !results.isEmpty();
  }

  public boolean single() {
    // is there only one realistic option (or did the user explicitly select one)?
    return results != null && results.size() == 1 || selectedExplicitly;
  }

  public boolean nextBestResult() {
    if (foundSomethingUseful() && results.size()>1) {
      selectedExplicitly = true;
      resultOffset += 1;
      if (resultOffset == results.size())
        resultOffset = 0;
      return true;
    }
    return false;
  }

  public boolean prevBestResult() {
    if (foundSomethingUseful() && results.size()>1) {
      selectedExplicitly = true;
      resultOffset -= 1;
      if (resultOffset < 0)
        resultOffset += results.size();
      return true;
    }
    return false;
  }

  private String code(int i) {
    return resultStrings.get(i);
  }

  private String code(Denotations.Stmt stmt) {
    return reformat(Tarski.pretty(stmt, env));
  }

  // the string should be a single syntactically valid statement
  private String reformat(@NotNull String in) {
    PsiElement elem = JavaPsiFacade.getElementFactory(project).createStatementFromText(in, place);
    CodeStyleManager.getInstance(project).reformat(elem, true);
    return elem.getText();
  }

  public String bestText() {
    assert foundSomethingUseful();
    return code(resultOffset);
  }
}
