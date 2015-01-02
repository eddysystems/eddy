package com.eddysystems.eddy;

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
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.ProjectScope;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import tarski.*;
import tarski.Items.Item;
import tarski.Scores.Alt;

import java.util.List;

import static ambiguity.JavaUtils.popScope;
import static ambiguity.JavaUtils.pushScope;

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
        System.out.println("cloning not supported: " + e);
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
    EddyPlugin.getInstance(myFixture.getProject()).initEnv();
    System.out.println("Document:");
    System.out.println(myFixture.getEditor().getDocument().getCharsSequence());
    final Eddy eddy = new Eddy(myFixture.getProject());
    eddy.process(myFixture.getEditor(),special);

    /*
    for (Map.Entry<PsiElement,Item> it : EddyPlugin.getInstance(myFixture.getProject()).getEnv().items.entrySet()) {
      if (it.getKey() instanceof PsiClass && ((PsiClass)it.getKey()).getName().equals("Object"))
        System.out.println("static: " + it.getKey() + " => " + it.getValue());
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
    System.out.println("results:");
    final List<Alt<List<String>>> results = eddy.getResults();
    final List<String> strings = eddy.getResultStrings();
    final String sep = "  -------------------------------";
    for (int i=0;i<results.size();i++) {
      final Alt<List<String>> r = results.get(i);
      final String s = strings.get(i);
      if (i >= 4 && (special==null || !special.equals(s))) continue;
      if (i > 0) System.out.println(sep);
      System.out.println("  " + s);
      System.out.println("  " + r);
      System.out.println(JavaScores.ppretty(r.dp()).prefixed("  "));
    }
  }

  private void checkAfterActionPerformed(String actionId, final Runnable check) {
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

    // delete sub
    myFixture.performEditorAction(actionId);
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
      System.out.println(m);
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
  public void _testCreateEddy() throws Exception {
    Eddy eddy = setupEddy(null,"dummy.java");
    Base.checkEnv(eddy.getEnv());
  }

  public void _testProbLE1() {
    Eddy eddy = setupEddy(null,"denote_x.java");
    for (Scores.Alt<List<String>> result : eddy.getResults())
      assertTrue("Probability > 1", result.p() <= 1.0);
  }

  public void _testTypeVar() {
    Eddy eddy = setupEddy(null,"typeVar.java");
    int As = 0, Bs = 0, Cs = 0;
    for (Item i : eddy.getEnv().allItems()) {
      final String n = i.name();
      if      (n.equals("Avar")) As++;
      else if (n.equals("Bvar")) Bs++;
      else if (n.equals("Cvar")) Cs++;
    }
    System.out.println("As "+As+", Bs "+Bs+", Cs "+Cs);
    assert As==1;
    assert Bs==1;
    assert Cs==0;
  }

  public void _testImplicitConstructor() {
    Eddy eddy = setupEddy(null,"ConstructorTest.java");
    boolean Bc = false, Cc = false;
    for (Item i : eddy.getEnv().allItems()) {
      if (!(i instanceof Items.ConstructorItem))
        continue;
      if (((Items.ConstructorItem) i).parent().name().equals("A") || ((Items.ConstructorItem) i).parent().name().equals("B") || ((Items.ConstructorItem) i).parent().name().equals("C"))
        System.out.println("found constructor " + i.name() + " (" + i.qualifiedName() + ") for class " + ((Items.ConstructorItem) i).parent().name() + " info " + ((Items.ConstructorItem) i).params());
      if (i.name().equals("A"))
        throw new AssertionError("found constructor" + i.print() + " which should be private and inAccessible");
      if (i.name().equals("B") && ((Items.ConstructorItem) i).params().isEmpty())
        throw new AssertionError("found constructor" + i.print() + " which is not implicitly declared (another constructor is)");
      if (i.name().equals("B") && ((Items.ConstructorItem) i).params().contains(Types.IntType$.MODULE$))
        Bc = true;
      if (i.name().equals("C"))
        Cc = true;
    }
    assertTrue("constructor (B) not in environment", Bc);
    assertTrue("implicitly defined constructor (C) not in environment", Cc);
  }

  public void _testClosingBrace() {
    Eddy eddy = setupEddy(null,"closingBrace.java");
    checkBest(eddy,"}",.9);
  }

  public void _testPartialEditTypeConflict() {
    Eddy eddy = setupEddy(null,"partialEditTypeConflict.java");
    checkResult(eddy, "List<NewNewNewType> = new ArrayList<NewNewNewType>();");
  }

  public void _testPartialEditTypeConflictPriority() {
    Eddy eddy = setupEddy(null,"partialEditTypeConflict.java");
    // because our cursor is hovering at NewType, this is the one we edited, so it should be higher probability
    checkPriority(eddy, "List<NewNewNewType> = new ArrayList<NewNewNewType>()", "List<OldOldOldType> = new ArrayList<OldOldOldType>()");
  }

  public void _testFizz() {
    final String best = "fizz(\"s\", x, q);";
    Eddy eddy = setupEddy(best,"fizz.java");
    checkBest(eddy,best,.9);
  }

  public void testPsiListener() {
    PsiFile psifile = myFixture.configureByFile("file2.java");
    VirtualFile vf = psifile.getVirtualFile();

    EddyPlugin plugin = EddyPlugin.getInstance(myFixture.getProject());
    plugin.initEnv();
    final EnvironmentProcessor.JavaEnvironment env = plugin.getEnv();

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

      System.out.println("found local item " + it);

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
      System.out.println("  sub supers: " + sub.item().supers() + ", items " + sub.item().superItems());
      System.out.println("  sub deleted? " + sub.deleted());

      final Environment.Env tenv = env.getLocalEnvironment(((Converter.PsiEquivalent) sub).psi());

      // should find sup only when looking for Supers
      assertEquals(tenv.byItem(Super).all().right().get().length(), 1);
      // should find sub when looking for Subs
      assertEquals(tenv.byItem(Sub).all().right().get().length(), 1);
      // should find sub when looking for Interfaces
      assertEquals(tenv.byItem(Interface).all().right().get().length(), 1);
      //myFixture.performEditorAction();

      // check that sub's supers are only Object and Interface
      assertTrue(sub.inside().item().superItems().contains(Interface));
      assertTrue(sub.inside().item().superItems().contains(Items.ObjectItem$.MODULE$));
      assertTrue(sub.inside().item().superItems().length() == 2);
    }});
  }
}
