import com.eddysystems.eddy.Eddy;
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

public class Tests extends LightCodeInsightFixtureTestCase {

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

  public void testCeateEddy() {
    myFixture.configureByFile("dummy.java");

    System.out.println("Document:");
    System.out.println(myFixture.getEditor().getDocument().getCharsSequence());

    Eddy eddy = new Eddy();
    eddy.process(myFixture.getEditor());
    System.out.println("eddy says: ");
    for (String res: eddy.getResultStrings()) {
      System.out.println("  " + res);
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