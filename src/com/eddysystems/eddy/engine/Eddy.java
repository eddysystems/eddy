package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyPlugin;
import com.eddysystems.eddy.PreferencesProvider;
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
import scala.Function2;
import scala.Unit$;
import scala.runtime.AbstractFunction2;
import tarski.Denotations.CommentStmt;
import tarski.Denotations.Stmt;
import tarski.Environment.Env;
import tarski.Memory;
import tarski.Scores.Alt;
import tarski.Tarski;
import tarski.Tarski.ShowStmt;
import tarski.Tokens;
import tarski.Tokens.Token;
import utility.Locations.Located;
import utility.Utility.Unchecked;

import java.util.ArrayList;
import java.util.List;

import static com.eddysystems.eddy.engine.Utility.*;
import static com.eddysystems.eddy.engine.Utility.log;
import static utility.Utility.unchecked;

public class Eddy {
  final private Project project;
  final private Editor editor;
  final private Memory.Info base;

  public static class Input {
    final TextRange range;
    final List<Located<Token>> input;
    final PsiElement place;
    final String before_text;

    Input(final TextRange range, final List<Located<Token>> input, final PsiElement place, final String before_text) {
      this.range = range;
      this.input = input;
      this.place = place;
      this.before_text = before_text;
    }
  }

  // The results of the interpretation
  public static class Output {
    final private Eddy eddy;
    final public Input input;
    final public List<Alt<List<ShowStmt>>> results;

    // Mutable field: which output we've selected.  If we haven't explicitly selected something, offset < 0.
    private int selected = -1;

    Output(final Eddy eddy, final Input input, final List<Alt<List<ShowStmt>>> results) {
      this.eddy = eddy;
      this.input = input;
      this.results = results;
    }

    static String format(final List<ShowStmt> ss) {
      final StringBuilder b = new StringBuilder();
      for (final ShowStmt s : ss) {
        if (b.length() > 0)
          b.append(' ');
        b.append(s.format());
      }
      return b.toString();
    }
    public String format(final int i) {
      return format(results.get(i).x());
    }
    public List<String> formats() {
      final List<String> fs = new ArrayList<String>(results.size());
      for (final Alt<List<ShowStmt>> a : results)
        fs.add(format(a.x()));
      return fs;
    }

    public boolean foundSomething() {
      return !results.isEmpty();
    }

    // Did we find useful meanings, and are those meanings different from what's already there?
    public boolean shouldShowHint() {
      for (final Alt<List<ShowStmt>> r : results)
        if (format(r.x()).equals(input.before_text))
          return false; // We found what's already there
      return !results.isEmpty();
    }

    // Is there only one realistic option (or did the user explicitly select one)?
    public boolean single() {
      return results.size() == 1 || selected >= 0;
    }

    public boolean nextBestResult() {
      if (shouldShowHint() && results.size()>1) {
        selected = (Math.max(0,selected)+1)%results.size();
        return true;
      }
      return false;
    }

    public boolean prevBestResult() {
      if (shouldShowHint() && results.size()>1) {
        selected = (Math.max(0,selected)+results.size()-1)%results.size();
        return true;
      }
      return false;
    }

    public String bestText() {
      assert shouldShowHint();
      return format(Math.max(0,selected));
    }

    public void applySelected() {
      apply(format(Math.max(0,selected)));
    }

    public int autoApply() {
      // used to automatically apply the best found result
      return rawApply(eddy.editor.getDocument(), format(0));
    }

    public boolean shouldAutoApply() {
      // check if we're confident enough to apply the best found result automatically
      double t = PreferencesProvider.getData().getNumericAutoApplyThreshold();
      double f = PreferencesProvider.getData().getNumericAutoApplyFactor();
      log("confidence based on t = " + t + ", f = " + f + ", " + results.size() + " results.");
      if (results.size() >= 1 && results.get(0).p() >= t) {
        if (results.size() == 1)
          return true;
        else
          return results.get(0).p()/results.get(1).p() > f;
      }
      return false;
    }

    public int rawApply(final @NotNull Document document, final @NotNull String code) {
      final int offsetDiff = code.length() - input.range.getLength();
      document.replaceString(input.range.getStartOffset(), input.range.getEndOffset(), code);
      Memory.log(Memory.eddyAutoApply(eddy.base,input.input,results,code));
      return offsetDiff;
    }

    public void apply(final @NotNull String code) {
      ApplicationManager.getApplication().runWriteAction(new Runnable() {
        @Override
        public void run() {
          final Editor editor = eddy.editor;
          final Project project = editor.getProject();
          final Document document = editor.getDocument();
          final PsiFile psifile = PsiDocumentManager.getInstance(project).getPsiFile(document);
          assert psifile != null;

          new WriteCommandAction(project, psifile) {
            @Override
            public void run(@NotNull Result result) {
              final int newOffset = input.range.getEndOffset() + rawApply(document, code);
              editor.getCaretModel().moveToOffset(newOffset);
              PsiDocumentManager.getInstance(project).commitDocument(document);
            }
          }.execute();
        }
      });
      Memory.log(Memory.eddyApply(eddy.base,input.input,results,code));
    }
  }

  public static interface Take {
    // return true if we're done absorbing output, false if more is desired
    public boolean take(Output output);
  }

  public Eddy(@NotNull final Project project, final Editor editor) {
    this.project = project;
    this.editor = editor;
    this.base = Memory.basics(EddyPlugin.installKey(), EddyPlugin.getVersion() + " - " + EddyPlugin.getBuild(), project.getName());
  }

  public static class Skip extends Exception {
    public Skip(final String s) {
      super(s);
    }
  }

  // Walk upwards until we find a code block
  private static PsiCodeBlock codeBlockAbove(PsiElement e) throws Skip {
    for (;;) {
      if (e instanceof PsiCodeBlock)
        return (PsiCodeBlock)e;
      if (e == null || e instanceof PsiFile
                    || e instanceof PsiClass
                    || e instanceof PsiMethod)
        throw new Skip("Not inside code block, found: "+e);
      e = e.getParent();
    }
  }

  // Walk upwards until we find a method
  private static PsiMethod methodAbove(PsiElement e) throws Skip {
    for (;;) {
      if (e == null)
        throw new Skip("Not inside method");
      if (e instanceof PsiMethod)
        return (PsiMethod)e;
      e = e.getParent();
    }
  }

  public static Input input(final @NotNull Editor editor) throws Skip {
    log("processing eddy...");
    final Project project = editor.getProject();
    assert project != null;

    final Document document = editor.getDocument();
    final PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(document);
    if (file == null)
      throw new Skip("File is null");

    final int pos = editor.getCaretModel().getCurrentCaret().getOffset();
    final int line = document.getLineNumber(pos);
    final TextRange range = TextRange.create(document.getLineStartOffset(line),document.getLineEndOffset(line));
    log("  processing line "+line+": " + document.getText(range));

    // TODO: Generalize to handle class/method declarations
    final PsiCodeBlock body = methodAbove(codeBlockAbove(file.findElementAt(pos))).getBody();
    assert body != null;

    // Descend from Psi to AST so that we get all the tokens
    final ASTNode node = body.getNode();
    if (node == null)
      throw new Skip("Can't find a node to look at");

    // Walk the node subtree and output all tokens contained in any statement overlapping with the line
    // now, node is the AST node we want to interpret.
    final List<Located<Token>> tokens = new SmartList<Located<Token>>();
    final PsiElement[] place = new PsiElement[1];
    ((TreeElement)node).acceptTree(new RecursiveTreeElementVisitor() {
      @Override
      protected boolean visitNode(TreeElement element) {
        // If the element is not overlapping, don't output any of it
        if (!range.intersects(element.getTextRange()))
          return false;

        if (element instanceof LeafElement) {
          //log("    node: " + element + " " + element.getTextRange() + " -> " + Tokenizer.psiToTok(element));
          // Don't include the opening and closing brace of the outermost code block before we hit the method
          if (   (element.getElementType() == JavaTokenType.LBRACE || element.getElementType() == JavaTokenType.RBRACE)
              && element.getTreeParent().getTreeParent() instanceof MethodElement)
            return true;

          // If this is the first token, remember the previous token as the place where we compute the environment
          if (tokens.isEmpty()) {
            place[0] = element.getTreePrev().getPsi();
            assert place[0] != null;
          }
          tokens.add(Tokenizer.psiToTok(element));
        }
        return true;
      }
    });

    // Remove leading and trailing whitespace
    while (!tokens.isEmpty() && tokens.get(0).x() instanceof Tokens.WhitespaceTok)
      tokens.remove(0);
    while (!tokens.isEmpty() && tokens.get(tokens.size()-1).x() instanceof Tokens.WhitespaceTok)
      tokens.remove(tokens.size()-1);

    // Compute range to be replaced
    final TextRange trim = tokens.isEmpty() ? TextRange.EMPTY_RANGE
      : Tokenizer.range(tokens.get(0)).union(Tokenizer.range(tokens.get(tokens.size()-1)));

    final String before = document.getText(trim);
    log("  before: " + before);
    return new Input(trim,tokens,place[0],before);
  }

  public Env env(final Input input, final int lastEdit) {
    return EddyPlugin.getInstance(project).getEnv().getLocalEnvironment(input.place, lastEdit);
  }

  public void process(final @NotNull Editor editor, final int lastEdit, final Take takeoutput) {
    // Use mutable variables so that we log more if an exception is thrown partway through
    class Helper {
      final double start = Memory.now();
      Input input;
      Output output;
      List<Alt<List<ShowStmt>>> results;
      Throwable error;

      void compute(final Env env) {
        if (Thread.currentThread().isInterrupted())
          throw new ThreadDeath();
        final Function2<Stmt,String,String> format = new AbstractFunction2<Stmt,String,String>() {
          @Override public String apply(final Stmt s, final String sh) {
            return reformat(input.place,s,sh);
          }
        };
        final Tarski.Take take = new Tarski.Take() {
          @Override public boolean take(final List<Alt<List<ShowStmt>>> rs) {
            results = rs;
            output = new Output(Eddy.this, input, results);
            return takeoutput.take(output);
          }
        };
        Tarski.fixTake(input.input,env,format,take);
      }

      void unsafe() throws Skip {
        input = Eddy.input(editor);
        compute(env(input,lastEdit));
        output = new Output(Eddy.this,input,results);
      }

      void safe() {
        try {
          if (isDebug()) // Run outside try so that we can see inside exceptions
            unchecked(new Unchecked<Unit$>() { @Override public Unit$ apply() throws Skip {
              unsafe();
              return Unit$.MODULE$;
            }});
          else try {
            unsafe();
          } catch (Throwable e) {
            error = e;
            if (!(e instanceof ThreadDeath))
              logError("process()",e); // Log everything except for ThreadDeath, which happens all the time.
            if (e instanceof Error && !(e instanceof AssertionError))
              throw (Error)e; // Rethrow most kinds of Errors
          }
        } finally {
          Memory.log(Memory.eddyProcess(base,start,
                                        input==null ? null : input.input,
                                        results).error(error));
        }
      }
    }
    new Helper().safe();
  }

  // The string should be a single syntactically valid statement
  private String reformat(final PsiElement place, final @NotNull Stmt s, final @NotNull String show) {
    if (s instanceof CommentStmt)
      return ((CommentStmt)s).c().content();
    PsiElement elem = JavaPsiFacade.getElementFactory(project).createStatementFromText(show,place);
    CodeStyleManager.getInstance(project).reformat(elem,true);
    return elem.getText();
  }

}
