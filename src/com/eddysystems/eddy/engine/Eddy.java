package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyPlugin;
import com.eddysystems.eddy.LightDocument;
import com.eddysystems.eddy.PreferenceData;
import com.eddysystems.eddy.Preferences;
import com.intellij.codeInsight.daemon.impl.ShowIntentionsPass;
import com.intellij.codeInsight.intention.impl.IntentionHintComponent;
import com.intellij.lang.ASTNode;
import com.intellij.notification.Notification;
import com.intellij.notification.NotificationType;
import com.intellij.notification.Notifications;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.Result;
import com.intellij.openapi.application.impl.LaterInvocator;
import com.intellij.openapi.command.WriteCommandAction;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.DocumentEx;
import com.intellij.openapi.fileEditor.impl.FileDocumentManagerImpl;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.RecursiveTreeElementVisitor;
import com.intellij.psi.impl.source.tree.TreeElement;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.Function2;
import scala.Unit$;
import scala.runtime.AbstractFunction1;
import scala.runtime.AbstractFunction2;
import scala.runtime.BoxedUnit;
import scala.util.Try;
import tarski.Environment.Env;
import tarski.Memory;
import tarski.Scores.Alt;
import tarski.Tarski;
import tarski.Tarski.ShowStmts;
import tarski.Tokens.ShowFlags;
import tarski.Tokens.Token;
import tarski.Tokens.WhitespaceTok;
import utility.Locations.Loc;
import utility.Utility.Unchecked;

import java.util.ArrayList;
import java.util.List;

import static com.eddysystems.eddy.engine.Utility.*;
import static tarski.Tokens.abbrevShowFlags;
import static tarski.Tokens.fullShowFlags;
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

    Input(final TextRange range, final List<Loc<Token>> input, final PsiElement place, final String before_text) {
      this.range = range;
      this.input = input;
      this.place = place;
      this.before_text = before_text;
    }

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

    static String format(final ShowStmts ss, final ShowFlags f) {
      return f.abbreviate() ? ss.abbrev() : ss.full();
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
      return formats(new ShowFlags(true,true), true).toArray(new String[results.size()]);
    }

    public boolean foundSomething() {
      return !results.isEmpty();
    }

    // Did we find useful meanings, and are those meanings different from what's already there?
    public boolean shouldShowHint() {
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
      return rawApply(eddy.document, format(0, abbrevShowFlags()));
    }

    public boolean shouldAutoApply() {
      // check if we're confident enough to apply the best found result automatically
      PreferenceData data = Preferences.getData();
      double t = data.getNumericAutoApplyThreshold();
      double f = data.getNumericAutoApplyFactor();
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

      // reindent
      CodeStyleManager csm = CodeStyleManager.getInstance(eddy.project);
      int sline = document.getLineNumber(input.range.getStartOffset());
      int fline = document.getLineNumber(input.range.getEndOffset() + offsetDiff);
      for (int i = sline; i <= fline; ++i) {
        csm.adjustLineIndent(document, document.getLineStartOffset(i));
      }

      Memory.log(Memory.eddyAutoApply(eddy.base,Memory.now(),input.input,results,code));
      return offsetDiff;
    }

    public void apply(final int index) {
      final String full = format(results.get(index).x(),fullShowFlags());
      ApplicationManager.getApplication().runWriteAction(new Runnable() {
        @Override
        public void run() {
          final Editor editor = eddy.editor;
          new WriteCommandAction(eddy.project, eddy.getFile()) {
            @Override
            public void run(@NotNull Result result) {
              final int newOffset = input.range.getEndOffset() + rawApply(eddy.document,full);
              editor.getCaretModel().moveToOffset(newOffset);
              PsiDocumentManager.getInstance(eddy.project).commitDocument(eddy.document);
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
    // return true if we're done absorbing output, false if more is desired
    public boolean take(Output output);
  }

  public Eddy(@NotNull final Project project, final Editor editor) {
    this.project = project;
    this.editor = editor;
    this.document = editor.getDocument();
    this.base = Memory.basics(EddyPlugin.installKey(), EddyPlugin.getVersion() + " - " + EddyPlugin.getBuild(), project.getName());
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
    log("processing eddy...");
    // Determine where we are
    final int cursor = editor.getCaretModel().getCurrentCaret().getOffset();
    final int line = document.getLineNumber(cursor);
    final TextRange range = TextRange.create(document.getLineStartOffset(line), document.getLineEndOffset(line));
    log("  processing line " + line + ": " + document.getText(range));

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

    // Compute range to be replaced.  We rely on !tokens.isEmpty
    final TextRange trim = Tokenizer.range(tokens.get(0)).union(Tokenizer.range(tokens.get(tokens.size()-1)));

    final String before = document.getText(trim);
    log("  before: " + before.replaceAll("[\n\t ]+", " "));
    return new Input(trim,tokens,place,before);
  }

  public Env env(final Input input, final int lastEdit) {
    return EddyPlugin.getInstance(project).getEnv().getLocalEnvironment(input.place, lastEdit);
  }

  private void updateIntentions() {
    if (!ApplicationManager.getApplication().isHeadlessEnvironment()) {
      LaterInvocator.invokeLater(new Runnable() {
        @Override
        public void run() {
          final PsiFile file = getFile();
          ShowIntentionsPass.IntentionsInfo intentions = new ShowIntentionsPass.IntentionsInfo();
          ShowIntentionsPass.getActionsToShow(editor, file, intentions, -1);
          if (!intentions.isEmpty()) {
            try {
              if (editor.getComponent().isDisplayable())
                IntentionHintComponent.showIntentionHint(project, file, editor, intentions, false);
            } catch (final NullPointerException e) {
              // Log and ignore
              log("updateIntentions: Can't show hint due to NullPointerException");
            }
          }
        }
      }, project.getDisposed());
    }
  }

  public void process(final @NotNull Editor editor, final int lastEdit, final Take takeoutput) {
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
            return reformat(input.place,sh,f);
          }
        };
        final long startTime = System.nanoTime();
        final Tarski.Take take = new Tarski.Take() {
          @Override public boolean take(final List<Alt<ShowStmts>> rs) {
            results = rs;
            double delay = (System.nanoTime() - startTime)/1e9;
            delays.add(delay);
            Eddy.Output output = new Output(Eddy.this,input,results);
            if (isDebug())
              System.out.println(String.format("output %.3fs: ", delay) + logString(output.formats(abbrevShowFlags(),true)));

            updateIntentions();
            return takeoutput.take(output);
          }
        };
        Tarski.fixTake(input.input,env,format,take);
      }

      void unsafe() {
        try {
          input = Eddy.this.input();
          compute(env(input,lastEdit));
        } catch (Skip s) {
          // ignore skipped lines
          log("skipping: " + s.getMessage());
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
            if (!(e instanceof ThreadDeath))
              logError("process()",e); // Log everything except for ThreadDeath, which happens all the time.
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
  private String reformat(final PsiElement place, final @NotNull String show, final ShowFlags f) {
    final CodeStyleManager csm = CodeStyleManager.getInstance(project);
    final PsiElementFactory ef = JavaPsiFacade.getElementFactory(project);
    final String blockText = '{' + show + "\n}";
    PsiCodeBlock block = ef.createCodeBlockFromText(blockText,place);

    // make sure the associated document does not require locks by making the document ourselves
    @NotNull final PsiFile file = block.getContainingFile();
    @NotNull final DocumentEx doc = new LightDocument(block.getText());
    doc.setModificationStamp(file.getModificationStamp());
    doc.setReadOnly(false);
    FileDocumentManagerImpl.registerDocument(doc, block.getContainingFile().getViewProvider().getVirtualFile());

    block = (PsiCodeBlock)csm.reformat(block,true);

    // strip whitespace at the beginning and end of the block
    PsiElement elem = block.getFirstBodyElement();
    // skip whitespace at the beginning of the block
    if (elem instanceof PsiWhiteSpace)
      elem = elem.getNextSibling();
    String result = "";
    while (elem != null && elem != block.getRBrace()) {
      if (elem instanceof PsiWhiteSpace && elem.getNextSibling() == block.getRBrace()) // don't believe IntelliJ, this is important!
        break;
      result += elem.getText();
      elem = elem.getNextSibling();
    }
    return result;
  }

}
