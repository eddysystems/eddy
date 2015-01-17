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
import scala.Unit;
import scala.Tuple2;
import scala.Unit$;
import scala.collection.Iterator;
import scala.collection.immutable.Map;
import tarski.*;
import tarski.Scores.Alt;
import tarski.Tokens.Token;
import utility.Locations.Located;
import utility.Utility.Unchecked;
import static utility.Utility.unchecked;
import java.util.List;
import tarski.Environment.Env;
import tarski.Denotations.Stmt;
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
    final public Env env;
    final public List<Alt<List<Tuple2<Stmt,String>>>> results;
    final public List<String> strings;
    final public boolean found_existing;

    // Mutable field: which output we've selected.  If we haven't explicitly selected something, offset < 0.
    private int selected = -1;

    Output(final Eddy eddy, final Input input, final Env env, final List<Alt<List<Tuple2<Stmt,String>>>> results,
           final List<String> strings, final boolean found_existing) {
      this.eddy = eddy;
      this.input = input;
      this.env = env;
      this.results = results;
      this.strings = strings;
      this.found_existing = found_existing;
    }

    public boolean foundSomething() {
      return !results.isEmpty();
    }

    // Did we find useful meanings, and are those meanings different from what's already there?
    public boolean foundSomethingUseful() {
      return !found_existing && !results.isEmpty();
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
      return strings.get(Math.max(0,selected));
    }

    public void applyBest() {
      apply(strings.get(Math.max(0,selected)));
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
      Memory.log(Memory.eddyApply(eddy.base,input.input,results,strings,code));
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

  private Input input(final @NotNull Editor editor) throws Skip {
    log("processing eddy@" + hashCode() + "...");
    assert project == editor.getProject();

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
    if (canceled)
      throw new ThreadDeath();
    return new Input(tokens_range,tokens,place[0],before_text);
  }

  private Env env(final Input input, final int lastEdit) {
    return EddyPlugin.getInstance(project).getEnv().getLocalEnvironment(input.place,lastEdit);
  }

  private List<Alt<List<Tuple2<Stmt,String>>>> results(final Input input, final Env env, final String special) {
    return Tarski.fixJava(input.input,env,new Tarski.Enough() {
      @Override public boolean enough(Map<List<Tuple2<Stmt,String>>,Object> m) {
        if (m.size() < 4) return false;
        if (special == null) return true;
        final Iterator<List<Tuple2<Stmt, String>>> i =  m.keysIterator();
        while (i.hasNext()) {
          final List<Tuple2<Stmt, String>> x = i.next();
          if (reformat(input.place,x).equals(special))
            return true;
        }
        return false;
      }
    });
  }

  private Output output(final Input input, final Env env, final List<Alt<List<Tuple2<Stmt,String>>>> results) {
    final Tuple2<List<String>,Boolean> f = reformat(input.place,results,input.before_text);
    return new Output(this,input,env,results,f._1(),f._2());
  }

  public Output process(final @NotNull Editor editor, final int lastEdit, final @Nullable String special) {
    // Use mutable variables so that we log more if an exception is thrown partway through
    class Helper {
      final double start = Memory.now();
      Input input;
      Env env;
      List<Alt<List<Tuple2<Stmt,String>>>> results;
      Output output;
      Throwable error;

      void unsafe() throws Skip {
        input   = Eddy.this.input(editor);
        env     = Eddy.this.env(input,lastEdit);
        results = Eddy.this.results(input,env,special);
        output  = Eddy.this.output(input,env,results);
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
                                        results,
                                        output==null ? null : output.strings).error(error));
        }
        return output;
      }
    }
    return new Helper().safe();
  }

  // Returns (strings,found_existing)
  private Tuple2<List<String>,Boolean> reformat(final PsiElement place, List<Alt<List<Tuple2<Stmt,String>>>> results, String before_text) {
    List<String> strings = new SmartList<String>();
    boolean found_existing = false;
    for (Alt<List<Tuple2<Stmt,String>>> interpretation : results) {
      final String s = reformat(place,interpretation.x());
      strings.add(s);
      log("eddy result: '" + s + "' existing '" + before_text + "'");
      if (s.equals(before_text))
        found_existing = true;
    }
    return new Tuple2<List<String>,Boolean>(strings,found_existing);
  }

  // The string should be a single syntactically valid statement
  private String reformat(final PsiElement place, @NotNull Stmt s, @NotNull String in) {
    if (s instanceof Denotations.CommentStmt)
      return ((Denotations.CommentStmt)s).c().content();
    PsiElement elem = JavaPsiFacade.getElementFactory(project).createStatementFromText(in, place);
    CodeStyleManager.getInstance(project).reformat(elem, true);
    return elem.getText();
  }

  private String reformat(final PsiElement place, @NotNull List<Tuple2<Stmt,String>> in) {
    String r = "";
    boolean first = true;
    for (final Tuple2<Stmt,String> ss : in) {
      if (first) first = false;
      else r += " ";
      r += reformat(place,ss._1(),ss._2());
    }
    return r;
  }
}
