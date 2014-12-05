import com.eddysystems.eddy.Eddy;
import static com.eddysystems.eddy.Utility.*;
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
import tarski.Denotations.Stmt;
import tarski.Scores;
import tarski.Base;

import java.util.List;
import java.util.concurrent.Callable;

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

  private Eddy setupEddy(String filename) {
    myFixture.configureByFile(filename);
    System.out.println("Document:");
    System.out.println(myFixture.getEditor().getDocument().getCharsSequence());
    Eddy eddy = new Eddy();
    eddy.process(myFixture.getEditor());
    return eddy;
  }

  private void checkResult(Eddy eddy, String expected) {
    System.out.println("results: ");
    boolean found = false;
    for (String s : eddy.getResultStrings()) {
      if (s.equals(expected))
        found = true;
      System.out.println(s);
    }
    assertTrue("eddy did not find correct solution.", found);
  }

  // actual tests
  public void testCreateEddy() throws Exception {
    for (int i = 0; i < 2; i++) {
      System.out.println("iteration " + i);
      timed("setupEddy", new Timed<Object>() {
        public Object call() {
          setupEddy("dummy.java");
          return null;
        }
      });
    }
  }

  public void testProbLE1() {
    Eddy eddy = setupEddy("denote_x.java");
    Base.checkEnv(eddy.getEnv());
    for (Scores.Alt<List<Stmt>> result : eddy.getResults()) {
      assertTrue("Probability > 1", result.p() <= 1.0);
    }
  }

  public void testBigFile() {
    Eddy eddy = setupEddy("EnvironmentProcessor.java");
    checkResult(eddy, "if (file != null && container != getValue(file))");
  }
}
