package com.eddysystems.eddy.engine;

import com.intellij.openapi.fileTypes.StdFileTypes;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.StdModuleTypes;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.impl.JavaAwareProjectJdkTableImpl;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.ContentEntry;
import com.intellij.openapi.roots.LanguageLevelModuleExtension;
import com.intellij.openapi.roots.ModifiableRootModel;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.pom.java.LanguageLevel;
import com.intellij.psi.*;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.LightCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

import static com.eddysystems.eddy.engine.Utility.log;

/* Abuse the test framework to get a headless version of IntelliJ
*
* */
public class SymbolScanner extends LightCodeInsightFixtureTestCase {
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
      model.getModuleExtension(LanguageLevelModuleExtension.class).setLanguageLevel(LanguageLevel.JDK_1_8);
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
    super.tearDown();
  }

  // copy the given directory to the test directory
  // iterate over all java files in the directory, and make psi files for all of them
  private void scan(final String basepath) {
    log("Starting scan of " + basepath);

    VirtualFile dir = myFixture.copyDirectoryToProject(basepath, basepath);

    log("Copied file: " + dir.getCanonicalPath());
    log("Children: ");
    log(dir.getChildren());

    for (final VirtualFile file : dir.getChildren()) {
      if (file.getFileType() != StdFileTypes.JAVA)
        continue;

      PsiFile psifile = PsiManager.getInstance(getProject()).findFile(file);

      if (psifile == null) {
        log("can't get psi for file " + file);
        continue;
      }

      // scan the file.
      log("scanning file " + file);
      psifile.accept(new PsiRecursiveElementVisitor() {
        @Override
        public void visitElement(PsiElement element) {
          if (element instanceof PsiReference) {
            log("found reference to " + ((PsiReference)element).getCanonicalText() + " => " + ((PsiReference)element).resolve());
          }
          super.visitElement(element);
        }
      });
    }
  }

  public void testScan() {
    scan("scan1");
  }

}
