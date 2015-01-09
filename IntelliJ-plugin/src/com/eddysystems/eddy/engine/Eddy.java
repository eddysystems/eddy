package com.eddysystems.eddy.engine;

import ambiguity.Locations.Located;
import com.eddysystems.eddy.EddyPlugin;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.impl.source.tree.*;
import com.intellij.psi.impl.source.tree.java.MethodElement;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.collection.immutable.Map;
import tarski.Environment;
import tarski.Scores;
import tarski.Tarski;
import tarski.Tokens;

import java.util.List;

import static com.eddysystems.eddy.engine.Utility.log;

public class Eddy {
  final private Project project;

  private boolean canceled;

  // all these are filled in process()
  // the range to be replaced
  private TextRange tokens_range;

  private Editor editor = null;
  private PsiElement place = null;

  // the results of the interpretation
  private Environment.Env env = null;
  private List<Scores.Alt<List<String>>> results;
  private List<String> resultStrings;
  private boolean found_existing;

  // a bias for which result is the best one (reset in process())
  private int resultOffset = 0;
  boolean selectedExplicitly = true;

  public Eddy(@NotNull final Project project) {
    this.project = project;
  }

  // applies a result in the editor
  public void apply(int i) {
    apply(code(i), editor, tokens_range);
  }

  static public void apply(final @NotNull String code,
                           final @NotNull Editor editor,
                           final @NotNull TextRange replace_range) {
    ApplicationManager.getApplication().runWriteAction(new Runnable() {
      @Override
      public void run() {
        final Project project = editor.getProject();
        final Document document = editor.getDocument();
        final PsiFile psifile = PsiDocumentManager.getInstance(project).getPsiFile(document);
        assert psifile != null;

        new WriteCommandAction(project, psifile) {
          @Override
          public void run(@NotNull Result result) {
            int newoffset = replace_range.getEndOffset() - replace_range.getLength() + code.length();
            System.out.println("replacing '" + document.getText(replace_range) + "' with '" + code + "'");
            document.replaceString(replace_range.getStartOffset(), replace_range.getEndOffset(), code);
            editor.getCaretModel().moveToOffset(newoffset);
            PsiDocumentManager.getInstance(project).commitDocument(document);
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

  public TextRange getRange() { return tokens_range; }

  public List<String> getResultStrings() { return resultStrings; }

  public List<Scores.Alt<List<String>>> getResults() { return results; }

  public Environment.Env getEnv() {
    assert env != null;
    return env;
  }

  public void dumpEnvironment(String filename) {
    if (env != null)
      Environment.envToFile(env,filename);
  }

  public void cancel() {
    canceled = true;
  }

  public void process(@NotNull Editor editor, int lastedit, final @Nullable String special) {
    log("processing eddy@" + hashCode() + "...");
    assert project == editor.getProject();

    Document document = editor.getDocument();

    // reset object variables
    this.editor = editor;
    found_existing = false;
    results = null;
    resultStrings = new SmartList<String>();

    // clear the offset
    resultOffset = 0;
    selectedExplicitly = false;

    PsiFile psifile = PsiDocumentManager.getInstance(project).getPsiFile(document);

    if (psifile == null)
      return;

    int pos = editor.getCaretModel().getCurrentCaret().getOffset();
    int lnum = document.getLineNumber(pos);

    int column = pos - document.getLineStartOffset(lnum);
    final TextRange lrange = TextRange.create(document.getLineStartOffset(lnum), document.getLineEndOffset(lnum));
    String line = document.getText(lrange);

    //log("processing at " + lnum + "/" + column);
    log("  current line: " + line);

    PsiElement elem = psifile.findElementAt(pos);

    // TODO: remove this once we can handle class/method declarations
    // Bail if we're not inside a code block
    PsiElement block = elem;
    //log("  elem: " + block);
    while (block != null &&
           !(block instanceof PsiFile) &&
           !(block instanceof PsiClass) &&
           !(block instanceof PsiMethod) &&
           !(block instanceof PsiCodeBlock)) {
      block = block.getParent();
      //log("    block: " + block);
    }
    if (!(block instanceof PsiCodeBlock)) {
      log("  not inside code block, found: " + block);
      return;
    }

    //log("    found code block");

    // walk further up, until we hit the containing method
    while (block != null && !(block instanceof PsiMethod)) {
      block = block.getParent();
      //log("    block: " + block);
    }

    if (block == null) {
      log("  not inside method");
      return;
    }

    // get the method body
    block = ((CompositeElement)(block.getNode())).findChildByRoleAsPsiElement(ChildRole.METHOD_BODY);

    ASTNode node = block.getNode();
    if (node == null) {
      log("  cannot find a node to look at.");
      return;
    }

    // then walk the node subtree and output all tokens contained in any statement overlapping with the line
    // now, node is the AST node we want to interpret.

    // get token stream for this node
    assert node instanceof TreeElement;

    final List<Located<Tokens.Token>> vtokens = new SmartList<Located<Tokens.Token>>();
    final List<TextRange> vtokens_ranges = new SmartList<TextRange>();

    ((TreeElement) node).acceptTree(new RecursiveTreeElementVisitor() {
      @Override
      protected boolean visitNode(TreeElement element) {
        // if the element is not overlapping, don't output any of it
        if (!lrange.intersects(element.getTextRange())) {
          return false;
        }

        if (element instanceof LeafElement) {
          //log("    node: " + element + " " + element.getTextRange() + " -> " + Tokenizer.psiToTok(element));
          // don't include the opening and closing brace of the outermost code block before we hit the method
          if ((element.getElementType() == JavaTokenType.LBRACE || element.getElementType() == JavaTokenType.RBRACE) &&
              element.getTreeParent().getTreeParent() instanceof MethodElement)
            return true;

          vtokens_ranges.add(element.getTextRange());

          // if this would be the first token added to the stream, remember the previous token as the place where we
          // compute the environment
          if (vtokens.isEmpty()) {
            place = element.getTreePrev().getPsi();
            assert place != null;
          }

          vtokens.add(Tokenizer.psiToTok(element));
        }

        return true;
      }
    });

    List<Located<Tokens.Token>> tokens = vtokens;
    List<TextRange> tokens_ranges = vtokens_ranges;

    // Remove leading and trailing whitespace
    while (!tokens.isEmpty() && tokens.get(0).x() instanceof Tokens.WhitespaceTok) {
      tokens = tokens.subList(1,tokens.size());
      tokens_ranges = tokens_ranges.subList(1,tokens_ranges.size());
    }
    while (!tokens.isEmpty() && tokens.get(tokens.size()-1).x() instanceof Tokens.WhitespaceTok) {
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

    log("  before: " + before_text);

    // don't do the environment if we're canceled
    if (canceled)
      return;

    env = EddyPlugin.getInstance(project).getEnv().getLocalEnvironment(place, lastedit);
    final Tarski.Enough enough = new Tarski.Enough() { @Override
      public boolean enough(Map<List<String>,Object> m) {
        if (m.size() < 4) return false;
        if (special == null) return true;
        final scala.collection.Iterator<List<String>> i =  m.keysIterator();
        while (i.hasNext()) {
          final List<String> x = i.next();
          if (reformat(x).equals(special))
            return true;
        }
        return false;
      }
    };
    results = Tarski.fixJava(tokens,env,enough);
    resultStrings = reformat(results, before_text);
  }

  private List<String> reformat(List<Scores.Alt<List<String>>> results, String before_text) {
    List<String> resultStrings = new SmartList<String>();
    for (Scores.Alt<List<String>> interpretation : results) {
      final String s = reformat(interpretation.x());
      resultStrings.add(s);
      log("eddy result: '" + s + "' existing '" + before_text + "'");
      if (s.equals(before_text))
        found_existing = true;
    }
    return resultStrings;
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

  // The string should be a single syntactically valid statement
  private String reformat(@NotNull String in) {
    PsiElement elem = JavaPsiFacade.getElementFactory(project).createStatementFromText(in, place);
    CodeStyleManager.getInstance(project).reformat(elem, true);
    return elem.getText();
  }

  private String reformat(@NotNull List<String> in) {
    String r = "";
    boolean first = true;
    for (final String s : in) {
      if (first) first = false;
      else r += " ";
      r += reformat(s);
    }
    return r;
  }

  public String bestText() {
    assert foundSomethingUseful();
    return code(resultOffset);
  }
}
