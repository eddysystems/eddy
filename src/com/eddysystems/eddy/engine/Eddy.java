package com.eddysystems.eddy.engine;

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
import scala.Function2;
import scala.Unit;
import scala.Tuple2;
import scala.Unit$;
import scala.collection.Iterator;
import scala.collection.immutable.Map;
import scala.runtime.AbstractFunction2;
import tarski.*;
import tarski.Scores.Alt;
import tarski.Tokens.Token;
import tarski.Tarski.ShowStmt;
import utility.Locations.Located;
import utility.Utility.Unchecked;
import static utility.Utility.*;

import java.util.ArrayList;
import java.util.List;
import tarski.Environment.Env;
import tarski.Denotations.Stmt;
import tarski.Denotations.CommentStmt;
import static com.eddysystems.eddy.engine.Utility.*;

public class Eddy {
  final private Project project;
  final private Editor editor;
  final private Memory.Info base;

  private boolean canceled; // TODO: Clean up

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
    public boolean foundSomethingUseful() {
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
      if (foundSomethingUseful() && results.size()>1) {
        selected = (Math.max(0,selected)+1)%results.size();
        return true;
      }
      return false;
    }

    public boolean prevBestResult() {
      if (foundSomethingUseful() && results.size()>1) {
        selected = (Math.max(0,selected)+results.size()-1)%results.size();
        return true;
      }
      return false;
    }

    public String bestText() {
      assert foundSomethingUseful();
      return format(Math.max(0,selected));
    }

    public void applyBest() {
      apply(format(Math.max(0,selected)));
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
              final TextRange range = input.range;
              final int newOffset = range.getEndOffset() - range.getLength() + code.length();
              System.out.println("replacing '" + document.getText(range) + "' with '" + code + "'");
              document.replaceString(range.getStartOffset(), range.getEndOffset(), code);
              editor.getCaretModel().moveToOffset(newOffset);
              PsiDocumentManager.getInstance(project).commitDocument(document);
            }
          }.execute();
        }
      });
      Memory.log(Memory.eddyApply(eddy.base,input.input,results,code));
    }
  }

  public Eddy(@NotNull final Project project, final Editor editor) {
    this.project = project;
    this.editor = editor;
    this.base = Memory.basics(EddyPlugin.installKey(), EddyPlugin.getVersion() + " - " + EddyPlugin.getBuild(), project.getName());
  }

  public void cancel() {
    canceled = true;
  }

  public static class Skip extends Exception {
    public Skip(final String s) {
      super(s);
    }
  }

  public static Input input(final @NotNull Editor editor) throws Skip {
    log("processing eddy...");
    final Project project = editor.getProject();
    assert project != null;

    final Document document = editor.getDocument();
    final PsiFile psifile = PsiDocumentManager.getInstance(project).getPsiFile(document);
    if (psifile == null)
      throw new Skip("File is null");

    final int pos = editor.getCaretModel().getCurrentCaret().getOffset();
    final int lnum = document.getLineNumber(pos);

    final TextRange lrange = TextRange.create(document.getLineStartOffset(lnum), document.getLineEndOffset(lnum));
    final String line = document.getText(lrange);

    //log("processing at " + lnum + "/" + column);
    log("  current line: " + line);

    // TODO: remove this once we can handle class/method declarations
    // Bail if we're not inside a code block
    PsiElement block = psifile.findElementAt(pos);
    //log("  elem: " + block);
    while (block != null &&
           !(block instanceof PsiFile) &&
           !(block instanceof PsiClass) &&
           !(block instanceof PsiMethod) &&
           !(block instanceof PsiCodeBlock)) {
      block = block.getParent();
      //log("    block: " + block);
    }
    if (!(block instanceof PsiCodeBlock))
      throw new Skip("Not inside code block, found: "+block);

    //log("    found code block");

    // walk further up, until we hit the containing method
    while (block != null && !(block instanceof PsiMethod)) {
      block = block.getParent();
      //log("    block: " + block);
    }
    if (block == null)
      throw new Skip("Not inside method");

    // get the method body
    block = ((CompositeElement)(block.getNode())).findChildByRoleAsPsiElement(ChildRole.METHOD_BODY);

    final ASTNode node = block.getNode();
    if (node == null)
      throw new Skip("Can't find a node to look at");

    // then walk the node subtree and output all tokens contained in any statement overlapping with the line
    // now, node is the AST node we want to interpret.

    // get token stream for this node
    assert node instanceof TreeElement;

    final List<Located<Token>> vtokens = new SmartList<Located<Token>>();
    final List<TextRange> vtokens_ranges = new SmartList<TextRange>();

    final PsiElement[] place = new PsiElement[1];
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
            place[0] = element.getTreePrev().getPsi();
            assert place[0] != null;
          }

          vtokens.add(Tokenizer.psiToTok(element));
        }

        return true;
      }
    });

    List<Located<Token>> tokens = vtokens;
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
    TextRange tokens_range = TextRange.EMPTY_RANGE; // TextRange is broken. Argh.
    if (!tokens_ranges.isEmpty()) {
      tokens_range = tokens_ranges.get(0);
      for (TextRange range: tokens_ranges) {
        tokens_range = tokens_range.union(range);
      }
    }

    final String before_text = document.getText(tokens_range);
    log("  before: " + before_text);

    // Check if we're canceled.  TODO: Cleaner way?
    return new Input(tokens_range,tokens,place[0],before_text);
  }

  public Env env(final Input input, final int lastEdit) {
    return EddyPlugin.getInstance(project).getEnv().getLocalEnvironment(input.place, lastEdit);
  }

  public Output process(final @NotNull Editor editor, final int lastEdit, final @Nullable String special) {
    // Use mutable variables so that we log more if an exception is thrown partway through
    class Helper {
      final double start = Memory.now();
      Input input;
      List<Alt<List<ShowStmt>>> results;
      Output output;
      Throwable error;

      void compute(final Env env) {
        final Function2<Stmt,String,String> format = new AbstractFunction2<Stmt,String,String>() {
          @Override public String apply(final Stmt s, final String sh) {
            return reformat(input.place,s,sh);
          }
        };
        final Tarski.Take take = new Tarski.Take() {
          @Override public boolean take(final List<Alt<List<ShowStmt>>> rs) {
            results = rs;
            if (rs.size() < 4) return false;
            if (special == null) return true;
            for (final Alt<List<ShowStmt>> r : rs)
              if (Output.format(r.x()).equals(special))
                return true;
            return false;
          }
        };
        Tarski.fixTake(input.input,env,format,take);
      }

      void unsafe() throws Skip {
        input = Eddy.this.input(editor);
        if (canceled)
          throw new ThreadDeath();
        compute(env(input,lastEdit));
        output = new Output(Eddy.this,input,results);
      }

      Output safe() {
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
        return output;
      }
    }
    return new Helper().safe();
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
