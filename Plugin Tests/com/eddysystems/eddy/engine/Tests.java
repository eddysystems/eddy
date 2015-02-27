package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyPlugin;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.actionSystem.ex.ActionManagerEx;
import com.intellij.openapi.actionSystem.ex.AnActionListener;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.StdModuleTypes;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.impl.JavaAwareProjectJdkTableImpl;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.ContentEntry;
import com.intellij.openapi.roots.LanguageLevelModuleExtension;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.pom.java.LanguageLevel;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.collection.JavaConversions;
import tarski.Environment.Env;
import tarski.Items;
import tarski.Items.Item;
import tarski.JavaScores;
import tarski.Scores.Alt;
import tarski.Tarski.ShowStmts;
import tarski.Tokens.ShowFlags;
import tarski.Types;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.eddysystems.eddy.engine.Utility.log;
import static tarski.Tokens.abbrevShowFlags;
import static tarski.Tokens.fullShowFlags;
import static utility.JavaUtils.*;

public class Tests extends LightCodeInsightFixtureTestCase {
  // Keep track of how long each test takes
  static final Map<String,Double> times = new HashMap<String,Double>();

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

  protected void tearDown() throws Exception {
    log("timing:");
    log(times);
    final int n = times.size();
    double sum = 0, prod = 1;
    for (final double t : times.values()) {
      sum += t;
      prod *= t;
    }
    log("arithmetic = "+sum/n+" s");
    log("geometric = "+Math.pow(prod,1./n)+" s");
    super.tearDown();
  }

  private Eddy makeEddy() {
    if (!isSetUp) try {
      setUp();
    } catch (Exception e) { log("setup threw: " + e); }

    //PsiManager.getInstance(myFixture.getProject()).dropResolveCaches();
    EddyPlugin.getInstance(myFixture.getProject()).dropEnv();
    EddyPlugin.getInstance(myFixture.getProject()).initEnv(null);
    log("Document:");
    log(myFixture.getEditor().getDocument().getCharsSequence());
    return new Eddy(myFixture.getProject(),myFixture.getEditor());
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
        @Override public double take(Eddy.Output output) {
          this.output = output;
          if (output.results.size() < 4) return 0;
          if (special == null) return 1;
          for (final Alt<ShowStmts> r : output.results)
            if (Eddy.Output.format(r.x(),abbrevShowFlags()).equals(special))
              return 1;
          return 0;
        }
      }

      final TestTake take = new TestTake();
      final Eddy eddy = makeEddy();
      final String name = getName();
      final long start = System.nanoTime();
      eddy.process(lastEdit, take);
      final double time = 1e-9*(System.nanoTime()-start);
      log(name+" = "+time+" s");
      times.put(name,time);
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
      final Alt<ShowStmts> r = output.results.get(i);
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
      output.formats(abbrevShowFlags(), false).contains(expected));
  }

  private void checkAvoid(final Eddy.Output output, final String avoid) {
    dumpResults(output,avoid);
    assertFalse("eddy found incorrect solution: " + avoid,
      output.formats(abbrevShowFlags(), false).contains(avoid));
  }

  private void checkBest(final Eddy.Output output, final String best, final double margin, final ShowFlags f) {
    dumpResults(output,best);
    final List<String> ss = output.formats(f,false);
    final String got = ss.isEmpty() ? "<none>" : ss.get(0);
    assertTrue("checkBest failed:\n  wanted = '"+best+"'\n  got    = '"+got+'\'', best.equals(got));
    if (ss.size() >= 2) {
      final List<Alt<ShowStmts>> rs = output.results;
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

  private void checkFail(final Eddy.Output output) {
    if (output == null)
      return;
    dumpResults(output,null);
    throw new AssertionError("Expected null output");
  }

  private void test(String filename, String expected) {
    checkResult(setupEddy(null, -1, filename), expected);
  }

  private void testAvoid(String filename, String avoid) {
    checkAvoid(setupEddy(null, -1, filename), avoid);
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
    for (final Alt<ShowStmts> result : output.results)
      assertTrue("Probability > 1", result.p() <= 1.0);
  }

  public void testTypeVar() throws Exception {
    final Env env = setupEnv(-1,"typeVar.java");
    assertEquals(1, env.exactQuery("Avar").length());
    assertEquals(1, env.exactQuery("Bvar").length());
    assertEquals(0, env.exactQuery("Cvar").length());
  }

  public void testImplicitConstructor() throws Exception {
    final Env env = setupEnv(-1,"ConstructorTest.java");
    Item i = env.exactQuery("TestClassA$").head();
    assertEquals("found not exactly one constructor for " + i, 1, ((Items.ClassItem) i).constructors().length);
    assertFalse("constructor of A is private, should be inaccessible", ((Items.ClassItem)i).constructors()[0].accessible(env.place()));

    i = env.exactQuery("TestClassB$").head();
    Items.ConstructorItem[] cons = ((Items.ClassItem)i).constructors();
    assertEquals("found " + cons.length + " constructors which are not defined.", cons.length, 1);
    assertEquals("found constructor which is not defined, params: " + cons[0].params(), cons[0].params().length(), 1);
    assertEquals("found constructor which is not defined, params: " + cons[0].params(), cons[0].params().head(), Types.IntType$.MODULE$);

    i = env.exactQuery("TestClassC$").head();
    cons = ((Items.ClassItem)i).constructors();
    assertEquals("found " + cons.length + " constructors which are not defined.", cons.length, 1);
    assertEquals("found constructor which is not defined, params: " + cons[0].params(), cons[0].params().length(), 0);
  }

  public void testClosingBrace() {
    pushDebug();
    try {
      checkFail(setupEddy(null,-1,"closingBrace.java"));
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

  public void testVisibility() throws Exception {
    // package resolution only works if directory structure is consistent
    final Env env = setupEnv(-1, "scope1/scopes1.java", "scope2/scopes2.java");
    // make sure the public items are in locals, not added
    log("P1 query: ");
    log(env.exactQuery("P1"));
    log("P2 query: ");
    log(env.exactQuery("P2"));
    assertEquals(1, env.exactQuery("P1").length());
    assertEquals(1, env.exactQuery("P2").length());
    Item P1 = env.exactQuery("P1").head();
    Item P2 = env.exactQuery("P2").head();
    log("in scope: ");
    log(env.scope());
    // make from inside P1, we can see Local1, but not Local2
    log("Local1 query: " + env.exactQuery("Local1"));
    assertEquals(1, env.exactQuery("Local1").length());
    log("Local2 query: " + env.exactQuery("Local2"));
    assertEquals(0, env.exactQuery("Local2").length());

    Item L1 = env.exactQuery("Local1").head();
    assertTrue(env.scope().contains(L1));

    // TODO: more thorough tests of package local and protected visibility
  }

  public void testStaticScope() throws Exception {
    final Env env = setupEnv(-1, "test/staticScope.java");

    class Check {

      Item find(String name) {
        // find the item of that name that's in the "test" package
        scala.collection.immutable.List<Item> items = env.exactQuery(name);
        for (Item item : JavaConversions.asJavaIterable(items)) {
          if (item.qualified().startsWith("test.") || item instanceof Items.Local) {
            return item;
          } else {
            log("ignoring item " + item + " qualified name " + item.qualified());
          }
        }
        assertFalse("did not find local item with name " + name, true);
        return null;
      }

      void inScope(String name) {
        Item item = find(name);
        assertTrue("item " + item + " is not in scope, but should be", env.inScope(item));
      }
      void notInScope(String name) {
        Item item = find(name);
        assertFalse("item " + item + " is in scope, but shouldn't be", env.inScope(item));
      }

      Check() {
        inScope("X");
        inScope("Y");
        inScope("Z");
        inScope("E");
        inScope("I");
        inScope("static_i");
        notInScope("i");
        inScope("static_f");
        notInScope("f");
        inScope("Test");
        inScope("test");
        inScope("Local");
        inScope("local_i");
        inScope("local_k");
        inScope("test2");

        // check that only Local gets a ThisItem
        boolean found = false;
        for (scala.Tuple2<Item,Object> tup: JavaConversions.asJavaIterable(env.scope())) {
          Item it = tup._1();
          if (it instanceof Items.ThisItem) {
            assertEquals("expected no this except for Local", ((Items.ThisItem) it).item(), env.exactQuery("Local").head());
            found = true;
          }
        }
        assertTrue("could not find ThisItem for Local", found);
      }
    }
    new Check();
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

  public void testWhitespace() {
    testMargin("whitespace.java", "if (x != 0) ...", .9);
  }

  public void testWhitespaceFull() {
    // default IntelliJ behavior is to indent with 4 spaces, plus four from the indent in the file.
    testMarginFull("whitespace.java", "if (x != 0) // Condition\n        return 7; // Result", .9);
  }

  public void testAbbrev() {
    testMargin("abbrev.java", "return x + x;", .9);
  }

  public void testBlockNo() {
    testMargin("blockNo.java", "if (true) g();", .9);
  }

  public void testBlockYes() {
    testMargin("blockYes.java", "if (true) { if (true) g(); } else ...", .9);
  }

  public void testStaticCons() {
    testMargin("staticCons.java", "x = 7;", .9);
  }

  public void testStaticMethod() {
    testMargin("staticMethod.java", "int x = 7;", .9);
  }

  public void testStaticThis() {
    testMargin("staticThis.java", "this.x = 7;", .9);
  }

  public void testAssertInstanceOf() { testMargin("assertInstanceOf.java", "assert elem instanceof List;", .9); }

  public void testCustomException() {
    testMargin("super.java", "super(\"prefix\" + s);", .9);
  }

  // TODO: make sure resolution precedence between imports is correct (do we need sublevels between import statements?)

  public void testSystemOut() {
    testMargin("println.java", "System.out.println(\"\");", .995);
  }

  public void testInteger() {
    testMargin("integer.java", "Integer x = new Integer(4);", .9);
  }

  public void testPackages() {
    testMargin("packages.java", "java.util.ArrayList<Integer> x = new java.util.ArrayList<Integer>();", .9);
  }

  public void testLength() {
    testMargin("length.java", "return xs.length;", .9);
  }

  public void testPrivateField() {
    testMargin("privateField.java", "myManager = new X();", .9);
  }

  public void testInfix() {
    testMargin("infixCall.java", "if (x.contains(0)) return;", .9);
  }

  public void testGetSet() { testMargin("getSet.java", "Runtime.getRuntime().gc();",.9); }

  public void testRuntime() { testMargin("runtime.java", "Runtime.getRuntime();",.9); }
}
