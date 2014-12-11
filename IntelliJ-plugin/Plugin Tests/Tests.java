import com.eddysystems.eddy.Eddy;
import com.eddysystems.eddy.EnvironmentProcessor;
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
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;
import tarski.Base;
import tarski.Items.Item;
import tarski.Scores;

import java.util.List;

import static ambiguity.JavaUtils.popScope;
import static ambiguity.JavaUtils.pushScope;

public class Tests extends LightCodeInsightFixtureTestCase {

  // TODO: initialize global environment once for all tests

  @Override @NotNull
  public LightProjectDescriptor getProjectDescriptor() {
    return new LightProjectDescriptor() {
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
    };
  }

  @Override
  protected String getBasePath() {
    return System.getProperty("data.dir");
  }

  private Eddy makeEddy() {
    System.out.println("Document:");
    System.out.println(myFixture.getEditor().getDocument().getCharsSequence());
    Eddy eddy = new Eddy();
    eddy.process(myFixture.getEditor());
    return eddy;
  }

  private Eddy setupEddy(String... filename) {
    myFixture.configureByFiles(filename);
    return makeEddy();
  }

  private Eddy setupEddy(String filename) {
    pushScope("setup eddy");
    myFixture.configureByFile(filename);
    Eddy e = makeEddy();
    popScope();
    return e;
  }

  private void checkResult(Eddy eddy, String expected) {
    System.out.println("results: ");
    boolean found = false;
    for (String s : eddy.getResultStrings()) {
      if (s.equals(expected))
        found = true;
      System.out.println(s);
    }
    System.out.println("result denotations: ");
    for (tarski.Scores.Alt<List<String>> r : eddy.getResults()) {
      System.out.println(r);
    }
    assertTrue("eddy did not find correct solution: " + expected, found);
  }

  // actual tests
  public void testCreateEddy() throws Exception {
    for (int i = 0; i < 2; i++) {
      System.out.println("iteration " + i);
      setupEddy("dummy.java");
    }
  }

  public void testProbLE1() {
    EnvironmentProcessor.clearGlobalEnvironment();
    Eddy eddy = setupEddy("denote_x.java");
    Base.checkEnv(eddy.getEnv());
    for (Scores.Alt<List<String>> result : eddy.getResults())
      assertTrue("Probability > 1", result.p() <= 1.0);
  }

  public void testTypeVar() {
    Eddy eddy = setupEddy("typeVar.java");
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

  public void testProject() {
    EnvironmentProcessor.clearGlobalEnvironment();
    Eddy eddy = setupEddy("dummy.java",
                          "JSON-java/JSONObject.java",
                          "JSON-java/CDL.java",
                          "JSON-java/Cookie.java",
                          "JSON-java/CookieList.java",
                          "JSON-java/HTTP.java",
                          "JSON-java/HTTPTokener.java",
                          "JSON-java/JSONArray.java",
                          "JSON-java/JSONException.java",
                          "JSON-java/JSONML.java",
                          "JSON-java/JSONString.java",
                          "JSON-java/JSONStringer.java",
                          "JSON-java/JSONTokener.java",
                          "JSON-java/JSONWriter.java",
                          "JSON-java/Kim.java",
                          "JSON-java/Property.java",
                          "JSON-java/README",
                          "JSON-java/XML.java",
                          "JSON-java/XMLTokener.java");
    Base.checkEnv(eddy.getEnv());
  }

  /* This could be handled more gracefully, but because we cannot resolve any of these types, we don't know about their
     relationships and won't find the "correct" solution.
  public void testUnresolvedTypes() {
    Eddy eddy = setupEddy("EnvironmentProcessorTest.java");
    System.out.println("scope: " + eddy.getEnv().scopeMap());
    System.out.println(" getPackage:");
    tarski.Tarski.print(eddy.getEnv().exactQuery("getPackage"));
    System.out.println(" container:");
    tarski.Tarski.print(eddy.getEnv().exactQuery("container"));
    System.out.println(" file:");
    tarski.Tarski.print(eddy.getEnv().exactQuery("file"));
    checkResult(eddy, "if (file != null && container != getValue(file))");
  }
  */
}
