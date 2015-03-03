package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyPlugin;
import com.eddysystems.eddy.PreferenceData;
import com.eddysystems.eddy.Preferences;
import com.intellij.lang.ASTNode;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.application.RuntimeInterruptedException;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.RangeMarker;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.RecursiveTreeElementVisitor;
import com.intellij.psi.impl.source.tree.TreeElement;
import com.intellij.util.SmartList;
import com.siyeh.ipp.base.PsiElementPredicate;
import com.siyeh.ipp.fqnames.ReplaceFullyQualifiedNameWithImportIntention;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.Function2;
import scala.Unit$;
import scala.runtime.AbstractFunction1;
import scala.runtime.AbstractFunction2;
import scala.runtime.BoxedUnit;
import scala.util.Try;
import tarski.Environment.Env;
import tarski.JavaScores;
import tarski.Memory;
import tarski.Scores.Alt;
import tarski.Tarski;
import tarski.Tarski.ShowStmts;
import tarski.Tokens.*;
import utility.Locations.Loc;
import utility.Utility.Unchecked;

import java.util.ArrayList;
import java.util.List;

import static com.eddysystems.eddy.engine.Utility.log;
import static com.eddysystems.eddy.engine.Utility.logError;
import static tarski.JavaScores.ppretty;
import static tarski.Tokens.*;
import static utility.JavaUtils.isDebug;
import static utility.Utility.unchecked;

public class Eddy {
  final private Project project;
  final private Memory.Info base;
  final private Editor editor;
  final private Document document;

  public Editor getEditor() { return editor; }
  public PsiFile getFile() {
    final PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(document);
    assert file != null;
    return file;
  }

  public static class Input {
    final TextRange range;
    final List<Loc<Token>> input;
    final PsiElement place;
    final String before_text;
    final boolean atEOL;

    Input(final TextRange range, final List<Loc<Token>> input, final PsiElement place, final String before_text, final boolean atEOL) {
      this.range = range;
      this.input = input;
      this.place = place;
      this.before_text = before_text;
      this.atEOL = atEOL;
    }

    public boolean isAtEOL() { return atEOL; }

    public String getText() {
      return before_text;
    }
  }

  // The results of the interpretation
  public static class Output {
    final private Eddy eddy;
    final public Input input;
    final public List<Alt<ShowStmts>> results;

    // Mutable field: which output we've selected.  If we haven't explicitly selected something, offset < 0.
    private int selected = -1;

    Output(final Eddy eddy, final Input input, final List<Alt<ShowStmts>> results) {
      this.eddy = eddy;
      this.input = input;
      this.results = results;
    }

    public boolean skipped() {
      return results == null;
    }

    static String format(final ShowStmts ss, final ShowFlags f) {
      return f.den() ? ss.den() : f.abbreviate() ? ss.abbrev() : ss.full();
    }
    public String format(final int i, final ShowFlags f) {
      return format(results.get(i).x(),f);
    }
    public List<String> formats(final ShowFlags f, final boolean probs) {
      final List<String> fs = new ArrayList<String>(results.size());
      for (final Alt<ShowStmts> a : results)
        fs.add(format(a.x(),f));
      if (probs) {
        for (int i = 0; i < fs.size(); ++i) {
          fs.set(i, String.format("%f: %s", results.get(i).p(), fs.get(i)));
        }
      }
      return fs;
    }

    public String[] getResultSummary() {
      return formats(new ShowFlags(false,true,true), true).toArray(new String[results.size()]);
    }

    public boolean foundSomething() {
      return !skipped() && !results.isEmpty();
    }

    // Did we find useful meanings, and are those meanings different from what's already there?
    public boolean shouldShowHint() {
      if (!foundSomething())
        return false;
      for (final Alt<ShowStmts> r : results)
        if (r.x().similar(input.input))
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

    public String bestTextAbbrev() {
      assert shouldShowHint();
      return format(Math.max(0,selected),abbrevShowFlags());
    }

    public void applySelected() {
      apply(Math.max(0,selected));
    }

    public int autoApply() {
      // Automatically apply the best found result
      String code = format(0, fullShowFlags());
      int offset = rawApply(eddy.document, code);
      Memory.log(Memory.eddyAutoApply(eddy.base, Memory.now(), input.input, results, code));
      return offset;
    }

    public boolean isConfident() {
      // check if we're confident enough to apply the best found result automatically
      PreferenceData data = Preferences.getData();
      double t = data.getNumericAutoApplyThreshold();
      double f = data.getNumericAutoApplyFactor();
      if (results.size() >= 1 && results.get(0).p() >= t) {
        if (results.size() == 1)
          return true;
        else
          return results.get(0).p()/results.get(1).p() > f;
      }
      return false;
    }

    public boolean shouldAutoApply() {
      if (!Preferences.getData().isAutoApply())
        return false;

      return isConfident() && input.isAtEOL();
    }

    private void removeQualifiers(final @NotNull Document document, final RangeMarker rm) {
      // commit
      PsiDocumentManager.getInstance(eddy.project).commitDocument(eddy.document);
      // find common parent of everything that was inserted
      PsiFile file = PsiDocumentManager.getInstance(eddy.project).getPsiFile(eddy.document);
      if (file != null) {
        PsiElement elem = file.findElementAt(rm.getStartOffset());
        final TextRange tr = new TextRange(rm.getStartOffset(), rm.getEndOffset());
        while (elem != null && !elem.getTextRange().contains(tr))
          elem = elem.getParent();
        if (elem != null) {
          final ReplaceFullyQualifiedNameWithImportIntention intention = new ReplaceFullyQualifiedNameWithImportIntention();
          final PsiElementPredicate pr = intention.getElementPredicate();
          // traverse all elements in common parent that overlap the range marker
          elem.accept(new JavaRecursiveElementWalkingVisitor() {
            @Override
            public void visitReferenceElement(PsiJavaCodeReferenceElement reference) {
              if (!reference.getTextRange().intersects(tr))
                return;
              super.visitReferenceElement(reference);
              if (pr.satisfiedBy(reference))
                // TODO: inline without highlighting
                intention.processIntention(reference);
            }
          });
        }
      }
    }

    public int rawApply(final @NotNull Document document, final @NotNull String code) {
      document.replaceString(input.range.getStartOffset(), input.range.getEndOffset(), code);
      final int afterOffset = input.range.getStartOffset() + code.length();

      // remember offset
      RangeMarker rm = document.createRangeMarker(input.range.getStartOffset(), afterOffset);

      // get rid of qualifiers if imports would do instead
      // TODO: make this optional with preference
      removeQualifiers(document, rm);

      // reindent
      CodeStyleManager csm = CodeStyleManager.getInstance(eddy.project);
      final int sline = document.getLineNumber(input.range.getStartOffset());
      final int fline = document.getLineNumber(afterOffset);
      for (int i = sline; i <= fline; ++i) {
        csm.adjustLineIndent(document, document.getLineStartOffset(i));
      }

      PsiDocumentManager.getInstance(eddy.project).commitDocument(eddy.document);

      return rm.getEndOffset();
    }

    public void apply(final int index) {
      final String full = format(index,fullShowFlags());
      ApplicationManager.getApplication().runWriteAction(new Runnable() {
        @Override
        public void run() {
          final Editor editor = eddy.editor;
          new WriteCommandAction(eddy.project, eddy.getFile()) {
            @Override
            public void run(@NotNull Result result) {
              int offset = rawApply(eddy.document, full);
              editor.getCaretModel().moveToOffset(offset);
            }
          }.execute();
        }
      });
      Memory.log(Memory.eddyApply(eddy.base,Memory.now(),input.input,results,index));
    }

    public void logSuggestion(final @NotNull String suggestion) {
      Memory.log(Memory.eddySuggestion(eddy.base, Memory.now(), input.input, results, suggestion)).onComplete(new AbstractFunction1<Try<BoxedUnit>, Void>() {
        @Override
        public Void apply(Try<BoxedUnit> v) {
          final String title, msg;
          if (v.isSuccess()) {
            title = "Suggestion processed";
            msg = "Thank you! Your suggestion will help improve eddy!";
          } else {
            title = "Suggestion failed to send";
            msg = "I'm sorry, your suggestion could not be recorded. Our servers could not be reached.";
          }
          Notifications.Bus.notify(new Notification("Eddy", title, msg, NotificationType.INFORMATION), eddy.project);
          return null;
        }
      }, scala.concurrent.ExecutionContext.Implicits$.MODULE$.global());
    }
  }

  public static interface Take {
    // Returns a new cutoff probability.  To stop entirely, return 1.
    public double take(Output output);
  }

  public Eddy(@NotNull final Project project, final Editor editor) {
    this.project = project;
    this.editor = editor;
    this.document = editor.getDocument();
    this.base = EddyPlugin.basics(project);
  }

  private static class Skip extends Exception {
    public Skip(final String s) {
      super(s);
    }
  }

  public static class PsiStructureException extends RuntimeException {
    public PsiStructureException(final String s) { super(s); }
  }

  // Find the previous or immediately enclosing element (which may be null if there's no parent)
  private static @Nullable PsiElement previous(final PsiElement e) throws Skip {
    PsiElement p = e.getPrevSibling();
    if (p != null)
      return p;
    return e.getParent();
  }

  // Trim a range to not include whitespace
  private static TextRange trim(final Document doc, final TextRange r) {
    final int lo = r.getStartOffset();
    final String s = doc.getText(r);
    final String t = s.trim();
    final int st = s.indexOf(t);
    return new TextRange(lo+st,lo+st+t.length());
  }

  private static @NotNull PsiCodeBlock codeBlockAbove(PsiElement e) throws Skip {
    for (;;) {
      if (e == null)
        throw new Skip("No enclosing code block found");
      if (e instanceof PsiCodeBlock)
        return (PsiCodeBlock)e;
      e = e.getParent();
    }
  }

  private static @NotNull PsiElement stmtsAbove(PsiElement e) throws Skip {
    for (;;) {
      if (e == null)
        throw new Skip("No enclosing statements found");
      if (e instanceof PsiCodeBlock || e instanceof PsiStatement)
        return e;
      e = e.getParent();
    }
  }

  // Find the smallest consecutive sequence of statements and EOL comments that contains the given range.
  // 1. Starting at elem, go up to find the nearest enclosing code block.
  // 2. Descend to the smallest child that contains the whole trimmed range.
  // 3. Go up to the nearest enclosing statement or code block.
  // 4. If we're at a code block, return the list of children intersecting the line.
  // 5. Otherwise, return whatever we're at.
  private static List<PsiElement> elementsContaining(final Document doc, TextRange range, PsiElement e) throws Skip {
    // Trim whitespace off both ends of range
    range = trim(doc,range);

    // Go up to the nearest enclosing code block
    e = codeBlockAbove(e);

    // Descend to the smallest child of e that contains the whole (trimmed) range
    outer:
    for (;;) {
      for (final PsiElement kid : e.getChildren())
        if (kid.getTextRange().contains(range)) {
          e = kid;
          continue outer;
        }
      break;
    }

    // Go back up to find a statement or code block
    e = stmtsAbove(e);

    // Go up outside of unblocked ifs so that we don't turn an unblocked body into multiple statements
    // For an example, see testBlockYes in the IntelliJ tests.
    if (e instanceof PsiStatement)
      for (;;) {
        final PsiElement p = e.getParent();
        if (!(p instanceof PsiIfStatement))
          break;
        e = p;
      }

    // Collect results
    final List<PsiElement> results = new SmartList<PsiElement>();
    if (e instanceof PsiCodeBlock) {
      // We're a code block, so return only those children intersecting the line.
      // Also ignore the first and last children, which are left and right braces.
      final PsiElement[] block = e.getChildren();
      int lo = 1, hi = block.length-1;
      while (lo < hi && !block[lo  ].getTextRange().intersects(range)) lo++;
      while (lo < hi && !block[hi-1].getTextRange().intersects(range)) hi--;
      for (int i=lo;i<hi;i++)
        results.add(block[i]);
    } else {
      // Otherwise, return a singleton list
      results.add(e);
    }
    return results;
  }

  // Should we expand an element or leave it atomic?
  private static boolean expand(final TreeElement e, final TextRange range, final int cursor) {
    // Never expand leaves
    if (e instanceof LeafElement)
      return false;

    // Otherwise, expand or not based on psi
    final @NotNull PsiElement psi = e.getPsi();
    final TextRange r = psi.getTextRange();

    // TODO: At the moment, we never expand anonymous classes.  Actually, only their bodies should be left atomic.
    if (psi instanceof PsiAnonymousClass)
      return false;

    // Expand blocks if the cursor is strictly inside
    if (psi instanceof PsiCodeBlock) {
      // Check if we're strictly inside.  Note that r.contains(pos) is wrong here.
      //   |{}  -  r 0 2, pos 0, not inside
      //   {|}  -  r 0 2, pos 1, inside
      //   {}|  -  r 0 2, pos 2, not inside
      return r.getStartOffset() < cursor && cursor < r.getEndOffset();
    }

    // Expand statements if they overlap our line
    if (psi instanceof PsiStatement)
      return r.intersects(range);

    // Expand everything else
    return true;
  }

  public Input input() throws Skip {
    //log("processing eddy...");
    // Determine where we are
    final int cursor = editor.getCaretModel().getCurrentCaret().getOffset();
    final int line = document.getLineNumber(cursor);
    final TextRange range = TextRange.create(document.getLineStartOffset(line), document.getLineEndOffset(line));
    //log("  processing line " + line + ": " + document.getText(range));

    // Find relevant statements and comments
    final List<PsiElement> elems = elementsContaining(document,range,getFile().findElementAt(cursor));
    if (elems.isEmpty())
      throw new Skip("Empty statement list");
    final PsiElement place = previous(elems.get(0));
    if (place == null)
      throw new PsiStructureException("previous(" + elems.get(0) + ") == null");

    // Walk all relevant elements, collecting leaves and atomic code blocks.
    // We walk on AST instead of Psi to get down to the token level.
    final List<Loc<Token>> tokens = new ArrayList<Loc<Token>>();
    final RecursiveTreeElementVisitor V = new RecursiveTreeElementVisitor() {
      @Override protected boolean visitNode(final TreeElement e) {
        if (expand(e,range,cursor))
          return true;
        tokens.add(Tokenizer.psiToTok(e));
        return false;
      }
    };
    for (final PsiElement elem : elems) {
      final ASTNode node = elem.getNode();
      assert node instanceof TreeElement : "Bad AST node "+node+" for element "+elem;
      ((TreeElement)node).acceptTree(V);
    }

    // Trim whitespace at the ends of the token stream
    while (!tokens.isEmpty() && tokens.get(0).x()               instanceof WhitespaceTok) tokens.remove(0);
    while (!tokens.isEmpty() && tokens.get(tokens.size()-1).x() instanceof WhitespaceTok) tokens.remove(tokens.size()-1);
    if (tokens.isEmpty())
      throw new Skip("No tokens");

    // Skip if we're entirely comments and whitespace
    boolean allSpace = true;
    for (final Loc<Token> t : tokens)
      if (!(t.x() instanceof SpaceTok)) {
        allSpace = false;
        break;
      }
    if (allSpace)
      throw new Skip("All whitespace and comments");

    // Compute range to be replaced.  We rely on !tokens.isEmpty
    final TextRange trim = Tokenizer.range(tokens.get(0)).union(Tokenizer.range(tokens.get(tokens.size()-1)));

    final String before = document.getText(trim);
    log("eddy before: " + before.replaceAll("[\n\t ]+", " "));

    // find out whether we're functionally at the end of the line (not counting whitespace)
    boolean atEOL = editor.getDocument().getText(new TextRange(cursor, range.getEndOffset())).trim().isEmpty();

    return new Input(trim,tokens,place,before,atEOL);
  }

  public Env env(final Input input, final int lastEdit) {
    return EddyPlugin.getInstance(project).getEnv().getLocalEnvironment(input.place, lastEdit);
  }

  public void process(final int lastEdit, final Take takeOutput) {
    // Use mutable variables so that we log more if an exception is thrown partway through
    class Helper {
      final double start = Memory.now();
      Input input;
      List<Alt<ShowStmts>> results;
      List<Double> delays = new ArrayList<Double>(4);
      Throwable error;

      void compute(final Env env) {
        if (Thread.currentThread().isInterrupted())
          throw new ThreadDeath();
        final Function2<String,ShowFlags,String> format = new AbstractFunction2<String,ShowFlags,String>() {
          @Override public String apply(final String sh, final ShowFlags f) {
            return reformat(input.place,sh);
          }
        };
        final long startTime = System.nanoTime();
        final Tarski.Take take = new Tarski.Take() {
          @Override public double take(final List<Alt<ShowStmts>> rs) {
            final Eddy.Output output = new Output(Eddy.this,input,rs);
            if (!rs.isEmpty()) {
              results = rs;
              final double delay = (System.nanoTime() - startTime)/1e9;
              delays.add(delay);
              if (isDebug()) {
                System.out.println(String.format("output %.3fs: ", delay));
                if (JavaScores.trackProbabilities) {
                  for (int i = 0; i < output.results.size(); ++i) {
                    Alt<ShowStmts> ss = output.results.get(i);
                    System.out.println(ppretty(ss.dp()).prefixed("  "));
                    System.out.println(output.format(i, denotationShowFlags()));
                  }
                } else {
                  System.out.println(output.formats(denotationShowFlags(),true));
                }
              }
            }
            return takeOutput.take(output);
          }
        };
        Tarski.fixTake(input.input,env,format,take);
      }

      void unsafe() {
        try {
          input = Eddy.this.input();
          compute(env(input,lastEdit));
        } catch (Skip s) {
          // ignore skipped lines, but let caller know we did nothing
          //takeOutput.take(new Output(Eddy.this,input,null));
          //log("skipping: " + s.getMessage());
        }
      }

      void safe() {
        try {
          if (isDebug()) // Run outside try so that we can see inside exceptions
            unchecked(new Unchecked<Unit$>() { @Override public Unit$ apply() {
              unsafe();
              return Unit$.MODULE$;
            }});
          else try {
            unsafe();
          } catch (final Throwable e) {
            error = e;
            if (!(e instanceof ThreadDeath) && !(e instanceof RuntimeInterruptedException))
              logError("process()",e); // Log everything except for ThreadDeath and RuntimeInterruptedException, which happens all the time.
            if (e instanceof Error && !(e instanceof AssertionError))
              throw (Error)e; // Rethrow most kinds of Errors
          }
        } finally {
          Memory.log(Memory.eddyProcess(base,start,
                                        input==null ? null : input.input,
                                        results,
                                        delays).error(error));
        }
      }
    }
    new Helper().safe();
  }

  // The string should be a single syntactically valid statement
  private String reformat(final PsiElement place, final @NotNull String show) {
    return new Formatter(project,place).reformat(show);
  }

}
