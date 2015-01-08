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
import scala.collection.JavaConversions;
import tarski.*;
import tarski.Items.Item;
import tarski.Scores.Alt;

import java.util.List;

import static ambiguity.JavaUtils.popScope;
import static ambiguity.JavaUtils.pushScope;
import static com.eddysystems.eddy.engine.Utility.log;

public class Tests extends LightCodeInsightFixtureTestCase {

  // TODO: initialize global environment once for all tests

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

  private Eddy makeEddy(@Nullable String special) {
    // not sure why we have to explicitly call this
    PsiManager.getInstance(myFixture.getProject()).dropResolveCaches();
    EddyPlugin.getInstance(myFixture.getProject()).dropEnv();
    EddyPlugin.getInstance(myFixture.getProject()).initEnv(null);
    log("Document:");
    log(myFixture.getEditor().getDocument().getCharsSequence());
    final Eddy eddy = new Eddy(myFixture.getProject());
    eddy.process(myFixture.getEditor(),special);

    /*
    for (Map.Entry<PsiElement,Item> it : EddyPlugin.getInstance(myFixture.getProject()).getEnv().items.entrySet()) {
      if (it.getKey() instanceof PsiClass && ((PsiClass)it.getKey()).getName().equals("Object"))
        log("static: " + it.getKey() + " => " + it.getValue());
    }
    */
    return eddy;
  }

  private Eddy setupEddy(@Nullable String special, String... filename) {
    myFixture.configureByFiles(filename);
    return makeEddy(special);
  }

  private Eddy setupEddy(@Nullable String special, final String filename) {
    pushScope("setup eddy");
    try {
      myFixture.configureByFile(filename);
      return makeEddy(special);
    } finally { popScope(); }
  }

  private void dumpResults(final Eddy eddy, final String special) {
    log("results:");
    final List<Alt<List<String>>> results = eddy.getResults();
    final List<String> strings = eddy.getResultStrings();
    final String sep = "  -------------------------------";
    for (int i=0;i<results.size();i++) {
      final Alt<List<String>> r = results.get(i);
      final String s = strings.get(i);
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

  private void checkResult(Eddy eddy, String expected) {
    dumpResults(eddy,expected);
    assertTrue("eddy did not find correct solution: " + expected, eddy.getResultStrings().contains(expected));
  }

  private void checkBest(Eddy eddy, String best, double margin) {
    dumpResults(eddy,best);
    final List<String> ss = eddy.getResultStrings();
    final String got = ss.isEmpty() ? "<none>" : ss.get(0);
    assertTrue("eddy wanted best = "+best+", got = "+got, best.equals(got));
    if (ss.size() >= 2) {
      final List<Alt<List<String>>> rs = eddy.getResults();
      final double p0 = rs.get(0).p(),
                   p1 = rs.get(1).p();
      final String m = "wanted margin "+margin+", got "+p1+" / "+p0+" = "+p1/p0;
      log(m);
      assertTrue(m, p1/p0 < margin);
    }
  }

  private void checkPriority(Eddy eddy, String high, String lo) {
    dumpResults(eddy,null);
    double phi = 0, plo = 0;
    final List<String> strings = eddy.getResultStrings();
    for (int i = 0; i < strings.size(); ++i) {
      double p = eddy.getResults().get(i).p();
      if (strings.get(i).equals(high))
        phi = p;
      else if (strings.get(i).equals(lo))
        plo = p;
    }
    assertTrue("eddy found " + lo + " likelier (" + plo + ") than " + high + " (" + phi + "), but shouldn't.", plo < phi);
  }






  // actual tests
  public void testCreateEddy() throws Exception {
    setupEddy(null,"dummy.java");
  }

  public void testProbLE1() {
    Eddy eddy = setupEddy(null,"denote_x.java");
    for (Scores.Alt<List<String>> result : eddy.getResults())
      assertTrue("Probability > 1", result.p() <= 1.0);
  }

  public void testTypeVar() {
    Eddy eddy = setupEddy(null,"typeVar.java");
    int As = 0, Bs = 0, Cs = 0;
    for (Item i : eddy.getEnv().allLocalItems()) {
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

  public void testImplicitConstructor() {
    Eddy eddy = setupEddy(null,"ConstructorTest.java");
    for (Item i : eddy.getEnv().allLocalItems()) {
      if (!(i instanceof Items.ClassItem))
        continue;
      if (i.name().equals("A") || i.name().equals("B") || i.name().equals("C"))
        log("found class " + i.name() + " (" + i.qualifiedName() + ")");
      if (i.name().equals("A"))
        assertEquals("found constructor for " + i + " which should be private and inAccessible", ((Items.ClassItem) i).constructors().length, 0);
      if (i.name().equals("B")) {
        Items.ConstructorItem cons[] = ((Items.ClassItem)i).constructors();
        assertEquals("found " + cons.length + " constructors which are not defined.", cons.length, 1);
        assertEquals("found constructor which is not defined, params: " + cons[0].params(), cons[0].params().length(), 1);
        assertEquals("found constructor which is not defined, params: " + cons[0].params(), cons[0].params().head(), Types.IntType$.MODULE$);
      }
      if (i.name().equals("C")) {
        Items.ConstructorItem cons[] = ((Items.ClassItem)i).constructors();
        assertEquals("found " + cons.length + " constructors which are not defined.", cons.length, 1);
        assertEquals("found constructor which is not defined, params: " + cons[0].params(), cons[0].params().length(), 0);
      }
    }
  }

  public void testClosingBrace() {
    Eddy eddy = setupEddy(null,"closingBrace.java");
    checkBest(eddy,"}",.9);
  }

  public void testPartialEditTypeConflict() {
    Eddy eddy = setupEddy(null,"partialEditTypeConflict.java");
    checkResult(eddy, "List<NewNewNewType> xs = new ArrayList<NewNewNewType>();");
  }

  public void testPartialEditTypeConflictPriority() {
    Eddy eddy = setupEddy(null,"partialEditTypeConflict.java");
    // because our cursor is hovering at NewType, this is the one we edited, so it should be higher probability
    checkPriority(eddy, "List<NewNewNewType> xs = new ArrayList<NewNewNewType>()", "List<OldOldOldType> xs = new ArrayList<OldOldOldType>()");
  }

  public void testFizz() {
    final String best = "fizz(\"s\", x, q);";
    Eddy eddy = setupEddy(best,"fizz.java");
    checkBest(eddy,best,.9);
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

      if (it.name().equals("sub")) {
        ssub = (Items.FieldItem)it;
      }
      if (it.name().equals("sup")) {
        ssup = (Items.FieldItem)it;
      }
      if (it.name().equals("Sub")) {
        sSub = (Items.ClassItem)it;
      }
      if (it.name().equals("Super")) {
        sSuper = (Items.ClassItem)it;
      }
      if (it.name().equals("Interface")) {
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

    final Environment.Env tenv = env.getLocalEnvironment(((Converter.PsiEquivalent) sub).psi());

    // should find sub and sup when looking for Supers
    assertEquals(tenv.byItem(Super).all().right().get().length(), 2);
    // should find sub when looking for Subs
    assertEquals(tenv.byItem(Sub).all().right().get().length(), 1);
    // should find sub when looking for Interfaces
    assertEquals(tenv.byItem(Interface).all().right().get().length(), 1);

    checkAfterActionPerformed(IdeActions.ACTION_EDITOR_DELETE_LINE, new Runnable() { @Override public void run() {
      log("  sub supers: " + sub.item().supers() + ", items " + sub.item().superItems());
      log("  sub deleted? " + sub.deleted());

      final Environment.Env tenv = env.getLocalEnvironment(((Converter.PsiEquivalent) sub).psi());

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
  
  public void testVisibility() {
    Eddy eddy = setupEddy(null, "scopes1.java", "scopes2.java");
    Environment.Env env = eddy.getEnv();
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
    // make from inside P1, we can see A1, but not A2
    log("Local1 query: ");
    log(env.exactQuery("Local1"));
    assertEquals(1, env.exactQuery("Local1").length());
    log("Local2 query: ");
    log(env.exactQuery("Local2"));
    assertEmpty(JavaConversions.asJavaCollection(env.exactQuery("Local2")));
    Item L1 = env.exactQuery("Local1").head();
    assertTrue(env.scope().contains(L1));

    // TODO: add tests for static visibility, more thorough tests of package local and protected visibility
  }

  public void testLibraryObject() {
    Eddy eddy = setupEddy(null, "LibraryObject.java");
    checkBest(eddy,"java.util.ArrayList<Object> x = new java.util.ArrayList<Object>();",.9);
  }

  // TODO: test scope resolution of imported package, * import, or static import (make sure objects are added to scope properly)
  // TODO: test codeblocks that are contained in single line (should not expand past code block)
}
