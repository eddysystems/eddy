package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyPlugin;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.IdeActions;
import com.intellij.openapi.actionSystem.ex.ActionManagerEx;
import com.intellij.openapi.actionSystem.ex.AnActionListener;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.StdModuleTypes;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.impl.JavaAwareProjectJdkTableImpl;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.ContentEntry;
import com.intellij.openapi.roots.FileIndexFacade;
import com.intellij.openapi.roots.LanguageLevelModuleExtension;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.pom.java.LanguageLevel;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import tarski.Environment;
import tarski.Environment.Env;
import tarski.Items;
import tarski.Items.Item;
import tarski.JavaScores;
import tarski.Scores.Alt;
import tarski.Tokens.ShowFlags;
import tarski.Tarski.ShowStmt;
import tarski.Types;

import java.util.List;

import static tarski.Tokens.fullShowFlags;
import static tarski.Tokens.abbrevShowFlags;
import static com.eddysystems.eddy.engine.Utility.log;
import static com.eddysystems.eddy.engine.Utility.pushDebug;
import static com.eddysystems.eddy.engine.Utility.popDebug;
import static utility.JavaUtils.popScope;
import static utility.JavaUtils.pushScope;

public class Tests extends LightCodeInsightFixtureTestCase {

  static class ProjectDesc implements LightProjectDescriptor {
    @Override
    public ModuleType getModuleType() {
      return StdModuleTypes.JAVA;
    }

    @Override
    public Sdk getSdk() {
      try {
        ProjectJdkImpl jdk = (ProjectJdkImpl) JavaAwareProjectJdkTableImpl.getInstanceEx().getInternalJdk().clone();
        jdk.setName("JDK");
        return jdk;
      } catch (CloneNotSupportedException e) {
        log("cloning not supported: " + e);
        return null;
      }
    }

    @Override
    public void configureModule(Module module, ModifiableRootModel model, ContentEntry contentEntry) {
      model.getModuleExtension(LanguageLevelModuleExtension.class).setLanguageLevel(LanguageLevel.JDK_1_6);
    }
  }
  static ProjectDesc desc = new ProjectDesc();

  @Override @NotNull
  public LightProjectDescriptor getProjectDescriptor() {
    return desc;
  }

  @Override
  protected String getBasePath() {
    return System.getProperty("data.dir");
  }

  private boolean isSetUp = false;
  protected void setUp() throws Exception {
    super.setUp();
    isSetUp = true;
  }

  private Eddy makeEddy() {
    if (!isSetUp) try {
      setUp();
    } catch (Exception e) { log("setup threw: " + e); }

    PsiManager.getInstance(myFixture.getProject()).dropResolveCaches();
    EddyPlugin.getInstance(myFixture.getProject()).dropEnv();
    EddyPlugin.getInstance(myFixture.getProject()).initEnv(null);
    log("Document:");
    log(myFixture.getEditor().getDocument().getCharsSequence());
    final Eddy eddy = new Eddy(myFixture.getProject(),myFixture.getEditor());
    return eddy;
  }

  private Env setupEnv(int lastEdit, final String... filename) throws Exception {
    pushScope("setup eddy env");
    try {
      myFixture.configureByFiles(filename);
      if (lastEdit < 0)
        lastEdit = myFixture.getEditor().getCaretModel().getOffset();
      final Eddy eddy = makeEddy();
      return eddy.env(eddy.input(),lastEdit);
    } finally { popScope(); }
  }

  private Eddy.Output setupEddy(final @Nullable String special, int lastEdit, final String... filename) {
    pushScope("setup eddy");
    try {
      myFixture.configureByFiles(filename);
      if (lastEdit < 0)
        lastEdit = myFixture.getEditor().getCaretModel().getOffset();

      class TestTake implements Eddy.Take {
        Eddy.Output output = null;
        @Override public boolean take(Eddy.Output output) {
          this.output = output;
          if (output.results.size() < 4) return false;
          if (special == null) return true;
          for (final Alt<List<ShowStmt>> r : output.results)
            if (Eddy.Output.format(r.x(),abbrevShowFlags()).equals(special))
              return true;
          return false;
        }
      }

      final TestTake take = new TestTake();
      makeEddy().process(myFixture.getEditor(),lastEdit,take);
      return take.output;
    } finally { popScope(); }
  }

  private void dumpResults(final Eddy.Output output, final String special) {
    if (output == null) {
      log("nothing found.");
      return;
    }
    final String sep = "  -------------------------------";
    log("results:");
    for (int i=0;i<output.results.size();i++) {
      final Alt<List<ShowStmt>> r = output.results.get(i);
      final String s = output.format(i,abbrevShowFlags());
      if (i >= 4 && (special==null || !special.equals(s))) continue;
      if (i > 0) log(sep);
      log("  " + s);
      log("  " + r);
      log(JavaScores.ppretty(r.dp()).prefixed("  "));
    }
  }

  private void checkAfterEditing(final Runnable edit, final Runnable check) {
    // we have to tack onto the action, otherwise everything will be destroyed before we get to it
    ActionManagerEx.getInstanceEx().addAnActionListener(new AnActionListener() {
      @Override public void beforeActionPerformed(AnAction action, DataContext dataContext, AnActionEvent event) {}
      @Override public void beforeEditorTyping(char c, DataContext dataContext) {}

      @Override public void afterActionPerformed(AnAction action, DataContext dataContext, AnActionEvent event) {
        // must be write action so it happens after the commit
        PsiDocumentManager.getInstance(myFixture.getProject()).commitAndRunReadAction(check);
        ActionManagerEx.getInstanceEx().removeAnActionListener(this);
      }
    });

    edit.run();
  }

  private void checkAfterActionPerformed(final String actionId, final Runnable check) {
    checkAfterEditing(new Runnable() {
      @Override
      public void run() {
        myFixture.performEditorAction(actionId);
      }
    }, check);
  }

  private void checkResult(final Eddy.Output output, final String expected) {
    dumpResults(output,expected);
    assertTrue("eddy did not find correct solution: " + expected,
      output.formats(abbrevShowFlags(),false).contains(expected));
  }

  private void checkBest(final Eddy.Output output, final String best, final double margin, final ShowFlags f) {
    dumpResults(output,best);
    final List<String> ss = output.formats(f,false);
    final String got = ss.isEmpty() ? "<none>" : ss.get(0);
    assertTrue("checkBest failed:\n  wanted = "+best+"\n  got    = "+got, best.equals(got));
    if (ss.size() >= 2) {
      final List<Alt<List<ShowStmt>>> rs = output.results;
      final double p0 = rs.get(0).p(),
                   p1 = rs.get(1).p();
      final String m = "wanted margin "+margin+", got "+p1+" / "+p0+" = "+p1/p0;
      log(m);
      assertTrue(m, p1/p0 < margin);
    }
  }

  private void checkPriority(final Eddy.Output output, final String high, final String lo) {
    dumpResults(output,null);
    double phi = 0, plo = 0;
    for (int i = 0; i < output.results.size(); ++i) {
      double p = output.results.get(i).p();
      if (output.format(i,abbrevShowFlags()).equals(high))
        phi = p;
      else if (output.format(i,abbrevShowFlags()).equals(lo))
        plo = p;
    }
    // TODO: Verify that both options are there
    assertTrue("eddy found " + lo + " likelier (" + plo + ") than " + high + " (" + phi + "), but shouldn't.", plo < phi);
  }

  private void test(String filename, String expected) {
    checkResult(setupEddy(null, -1, filename), expected);
  }

  private void testMargin(final String filename, final String best, final double margin) {
    checkBest(setupEddy(null, -1, filename), best, margin, abbrevShowFlags());
  }

  private void testMarginFull(final String filename, final String best, final double margin) {
    checkBest(setupEddy(null,-1,filename),best,margin,fullShowFlags());
  }

  private void testPriority(String filename, String hi, String lo) {
    checkPriority(setupEddy(null,-1,filename),hi,lo);
  }

  // actual tests
  public void testCreateEddy() throws Exception {
    setupEddy(null,-1,"dummy.java");
  }

  public void testProbLE1() {
    final Eddy.Output output = setupEddy(null,-1,"denote_x.java");
    for (final Alt<List<ShowStmt>> result : output.results)
      assertTrue("Probability > 1", result.p() <= 1.0);
  }

  public void testTypeVar() throws Exception {
    final Env env = setupEnv(-1,"typeVar.java");
    int As = 0, Bs = 0, Cs = 0;
    for (Item i : env.allLocalItems()) {
      final String n = i.name();
      if      (n.equals("Avar")) As++;
      else if (n.equals("Bvar")) Bs++;
      else if (n.equals("Cvar")) Cs++;
    }
    log("As "+As+", Bs "+Bs+", Cs "+Cs);
    assert As==1;
    assert Bs==1;
    assert Cs==0;
  }

  public void testImplicitConstructor() throws Exception {
    final Env env = setupEnv(-1,"ConstructorTest.java");
    for (Item i : env.allLocalItems()) {
      if (!(i instanceof Items.ClassItem))
        continue;
      if (i.name().equals("A") || i.name().equals("B") || i.name().equals("C"))
        log("found class " + i.name() + " (" + i.qualified() + ")");
      if (i.name().equals("A")) {
        assertEquals("found not exactly one constructor for " + i, 1, ((Items.ClassItem) i).constructors().length);
        assertFalse("constructor of A is private, should be inaccessible", ((Items.ClassItem)i).constructors()[0].accessible(env.place()));
      } else if (i.name().equals("B")) {
        Items.ConstructorItem cons[] = ((Items.ClassItem)i).constructors();
        assertEquals("found " + cons.length + " constructors which are not defined.", cons.length, 1);
        assertEquals("found constructor which is not defined, params: " + cons[0].params(), cons[0].params().length(), 1);
        assertEquals("found constructor which is not defined, params: " + cons[0].params(), cons[0].params().head(), Types.IntType$.MODULE$);
      } else if (i.name().equals("C")) {
        Items.ConstructorItem cons[] = ((Items.ClassItem)i).constructors();
        assertEquals("found " + cons.length + " constructors which are not defined.", cons.length, 1);
        assertEquals("found constructor which is not defined, params: " + cons[0].params(), cons[0].params().length(), 0);
      }
    }
  }

  public void testClosingBrace() {
    pushDebug();
    try {
      testMargin("closingBrace.java", "nonsense", .9);
      throw new Eddy.Skip("");
    } catch (final Eddy.Skip e) {
      assert e.getMessage().contains("No tokens");
    } finally { popDebug(); }
  }

  public void testPartialEditTypeConflict() {
    test("partialEditTypeConflict.java", "List<NewNewNewType> xs = new ArrayList<NewNewNewType>();");
  }

  public void testPartialEditTypeConflictPriority() {
    testPriority("partialEditTypeConflict.java", "List<NewNewNewType> xs = new ArrayList<NewNewNewType>();",
      "List<OldOldOldType> xs = new ArrayList<OldOldOldType>();");
  }

  public void testFizz() {
    testMargin("fizz.java", "fizz(\"s\", x, q);", .9);
  }

  public void testPsiListener() {
    PsiFile psifile = myFixture.configureByFile("psiModification.java");
    VirtualFile vf = psifile.getVirtualFile();

    EddyPlugin plugin = EddyPlugin.getInstance(myFixture.getProject());
    plugin.initEnv(null);
    final JavaEnvironment env = plugin.getEnv();

    // make sure the project scope is correct
    final GlobalSearchScope projectScope = ProjectScope.getProjectScope(myFixture.getProject());
    final FileIndexFacade facade = FileIndexFacade.getInstance(myFixture.getProject());
    assertTrue(facade.shouldBeFound(projectScope, vf));
    assertTrue(facade.isInSourceContent(vf));
    assertTrue(facade.isInContent(vf));
    assertTrue(facade.isInSource(vf));
    assertFalse(facade.isExcludedFile(vf));

    // find sub and sup objects, and the Super and Sub and Interface ClassItems
    Items.FieldItem ssub = null, ssup = null;
    Items.ClassItem sSub = null, sSuper = null, sInterface = null;
    for (Item it : env.localItems.values()) {

      log("found local item " + it);

      if (it.name().equals("sub") && it instanceof Items.FieldItem) {
        ssub = (Items.FieldItem)it;
      }
      if (it.name().equals("sup") && it instanceof Items.FieldItem) {
        ssup = (Items.FieldItem)it;
      }
      if (it.name().equals("Sub") && it instanceof Items.ClassItem) {
        sSub = (Items.ClassItem)it;
      }
      if (it.name().equals("Super") && it instanceof Items.ClassItem) {
        sSuper = (Items.ClassItem)it;
      }
      if (it.name().equals("Interface") && it instanceof Items.ClassItem) {
        sInterface = (Items.ClassItem)it;
      }
    }
    final Items.FieldItem sub = ssub, sup = ssup;
    final Items.ClassItem Sub = sSub, Super = sSuper, Interface = sInterface;

    assertNotNull(sub);
    assertNotNull(sup);
    assertNotNull(Sub);
    assertNotNull(Super);
    assertNotNull(Interface);

    final Environment.Env tenv = env.getLocalEnvironment(((Converter.PsiEquivalent) sub).psi(),-1);

    // should find sub and sup when looking for Supers
    assertEquals(tenv.byItem(Super).all().right().get().length(), 2);
    // should find sub when looking for Subs
    assertEquals(tenv.byItem(Sub).all().right().get().length(), 1);
    // should find sub when looking for Interfaces
    assertEquals(tenv.byItem(Interface).all().right().get().length(), 1);

    checkAfterActionPerformed(IdeActions.ACTION_EDITOR_DELETE_LINE, new Runnable() { @Override public void run() {
      log("  sub supers: " + sub.item().supers() + ", items " + sub.item().superItems());
      log("  sub deleted? " + sub.deleted());

      final Environment.Env tenv = env.getLocalEnvironment(((Converter.PsiEquivalent) sub).psi(),-1);

      // should find sup only when looking for Supers
      assertEquals(tenv.byItem(Super).all().right().get().length(), 1);
      // should find sub when looking for Subs
      assertEquals(tenv.byItem(Sub).all().right().get().length(), 1);
      // should find sub when looking for Interfaces
      assertEquals(tenv.byItem(Interface).all().right().get().length(), 1);
      //myFixture.performEditorAction();

      // check that sub's supers are only Object and Interface
      log("  sub superItems: " + sub.item().superItems());
      assertTrue(sub.item().superItems().contains(Interface));
      assertTrue(sub.item().superItems().contains(Items.ObjectItem$.MODULE$));
      assertTrue(sub.item().superItems().length() == 2);

      // TODO: some constructor modification tests as below

      // check that there is a single constructor to Super (with double arg)
      // check that there are two methods in Super, one f(int)->void and one Super(boolean)->void

      // go to line 5 (f(int x) {} in Super) and change the name to Super

      // check that there are two constructors: Super(int) and Super(double)
      // check that there is one method: Super(boolean)

      // go to line 4 and change the return type to void

      // check that there is one constructor: Super(int)
      // check that there are two methods: Super(boolean) and Super(double)

      // go to line 3 and change the return type to null

      // check that there are two constructors: Super(int) and Super(boolean)
      // check that there is one method: Super(double)

      // go to line 3 and change the name to f

      // check that there is one constructor: Super(int)
      // check that there are two methods: f(boolean) and Super(double)

    }});

  }
  
  public void testVisibility() throws Exception {
    // package resolution only works if directory structure is consistent
    final Env env = setupEnv(-1, "scope1/scopes1.java", "scope2/scopes2.java");
    JavaEnvironment jenv = EddyPlugin.getInstance(myFixture.getProject()).getEnv();
    log("local items: ");
    log(jenv.localItems);
    // make sure the public items are in locals, not added
    log("P1 query: ");
    log(env.exactQuery("P1"));
    log("P2 query: ");
    log(env.exactQuery("P2"));
    assertEquals(1, env.exactQuery("P1").length());
    assertEquals(1, env.exactQuery("P2").length());
    Item P1 = env.exactQuery("P1").head();
    Item P2 = env.exactQuery("P2").head();
    assertContainsElements(jenv.localItems.values(), P1);
    assertContainsElements(jenv.localItems.values(), P2);
    log("in scope: ");
    log(env.scope());
    // make from inside P1, we can see Local1, but not Local2
    log("Local1 query: " + env.exactQuery("Local1"));
    assertEquals(1, env.exactQuery("Local1").length());
    log("Local2 query: " + env.exactQuery("Local2"));
    assertEquals(0, env.exactQuery("Local2").length());

    Item L1 = env.exactQuery("Local1").head();
    assertTrue(env.scope().contains(L1));

    // TODO: add tests for static visibility, more thorough tests of package local and protected visibility
  }

  public void testLibraryObject() {
    testMargin("LibraryObject.java", "java.util.ArrayList<Object> x = new java.util.ArrayList<Object>();", .9);
  }

  public void testUnresolved() {
    testMargin("unresolved.java", "if (x != null) { ... }", .9);
  }

  public void testNullComparison() {
    testMargin("nullComparison.java", "if (x != null) { ... }", .9);
  }

  public void testSpuriousTypeArgs() {
    testMargin("spuriousTypeArgs.java", "Map<X, Y> map = A.f(y);", .9);
  }

  public void testExtraCode() {
    testMargin("extraCode.java", "return false;", .9);
  }

  public void testLiteralFalse() {
    testMargin("literalFalse.java", "return false;", .9);
  }

  public void testImport() {
    testMargin("importScope.java", "List<X> x;", .9);
  }

  public void testStaticImport() {
    testMargin("staticImport.java", "out.println(\"test\");", .9);
  }

  public void testWildcardImport() {
    testMargin("wildcardImport.java", "getRuntime().gc();", .9);
  }

  public void testOverloadedScope() {
    testMargin("overloadedScope.java", "fill(a, binarySearch(a, 5));", .9);
  }

  public void testIllegalExtends() {
    testMargin("illegalExtends.java", "int x;", .9);
  }

  public void testUnresolvedPackage() {
    testMargin("unresolvedPackage.java", "int x;", .9);
  }

  public void testInheritedVisibility() {
    testMargin("inheritedVisibility.java", "removeRange(0, 1);", .9);
  }

  public void testAnonymousClass() {
    testMargin("anonymousClass.java", "if (true) test(); else this.test();", .9);
  }

  public void testAnonymousClassSuper() {
    testMargin("anonymousClassSuper.java", "super.foo();", .9);
  }

  public void testPrivateCons() {
    testMargin("privateCons.java", "this();", .9);
  }

  public void testComment() {
    testMargin("comment.java", "return false;", .9);
  }

  public void testCommentFull() {
    testMarginFull("comment.java", "return false; // A comment", .9);
  }

  public void testParameterVsField() {
    testMargin("parameterVsField.java", "f(x);", .9);
  }

  public void testParameter() {
    testMargin("parameter.java", "f(x);", .9);
  }

  public void testElif() {
    testMargin("elif.java", "if (true) { ... } else if (false) { ... }", .9);
  }

  public void testAtomicStmt() {
    testMargin("atomicStmt.java", "if (true) ...", .9);
  }

  // TODO: make sure resolution precedence between imports is correct (do we need sublevels between import statements?)
}
