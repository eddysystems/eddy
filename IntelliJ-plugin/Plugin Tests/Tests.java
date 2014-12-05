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
}

/*
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

public class Tests extends LightCodeInsightFixtureTestCase {
  @Override @NotNull
  public LightProjectDescriptor getProjectDescriptor() {
    return JAVA_1_6;
  }

  class State {
    boolean running;

    public void start() {
      running = true;
    }
    public void stop() {
      running = false;
    }

    public boolean isRunning() {
      return running;
    }
  }

  public void testCreateEddy() {
    final State state = new State();
    state.start();

    try {
      setUp();
    } catch (Exception e) {
      System.out.println("exception in setUp: " + e);
    }

    ApplicationManager.getApplication().invokeLater( new Runnable() {
      @Override
      public void run() {
        myFixture.configureByFile("/Users/martin/src/eddy/eddy/IntelliJ-plugin/Test Data/dummy.java");

        System.out.println("Document:");
        System.out.println(myFixture.getEditor().getDocument().getCharsSequence());

        Eddy eddy = new Eddy();
        eddy.process(myFixture.getEditor());
        System.out.println("eddy says: ");
        for (String res: eddy.getResultStrings()) {
          System.out.println("  " + res);
        }
        state.stop();
      }
    });
    try {
      while (state.isRunning()) {
        Thread.sleep(100);
      }
    } catch (InterruptedException e) {
      System.out.println("interrupted: " + e);
    }

    try {
      tearDown();
    } catch (Exception e) {
      System.out.println("exception in tearDown: " + e);
    }
  }
}
*/