package com.eddysystems.eddy.engine;

import com.intellij.openapi.diagnostic.Logger;
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
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import static com.eddysystems.eddy.engine.Utility.log;

/* Abuse the test framework to get a headless version of IntelliJ
*
* */
public class SymbolScanner extends LightCodeInsightFixtureTestCase {
  static Logger logger = Logger.getInstance(SymbolScanner.class);

  static class ProjectDesc implements LightProjectDescriptor {
    @Override
    public ModuleType getModuleType() {
      return StdModuleTypes.JAVA;
    }

    @Override
    public Sdk getSdk() {
      try {
        JavaAwareProjectJdkTableImpl jdkTable = JavaAwareProjectJdkTableImpl.getInstanceEx();

        Sdk[] sdks = jdkTable.getAllJdks();
        for (final Sdk sdk : sdks)
          System.out.println("found sdk: " + sdk.getName() + " " + sdk.getVersionString() + " " + sdk.getSdkType());

        // just the internal one for now
        ProjectJdkImpl jdk = (ProjectJdkImpl) jdkTable.getInternalJdk().clone();
        jdk.setName("JDK");

        System.out.println("using sdk: " + jdk.getName() + " " + jdk.getVersionString() + " " + jdk.getSdkType());
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

  // should be put in a Map indexed by the fully qualified name
  static class ReferenceInfo {
    // what kind of symbol is this?
    enum Kind {PACKAGE, CLASS, INTERFACE, ENUM, FIELD, METHOD};
    Kind kind;

    static Kind getKind(PsiElement referenced) {
      Kind kind;
      if (referenced instanceof PsiClass) {
        if (((PsiClass) referenced).isEnum()) {
          kind = ReferenceInfo.Kind.ENUM;
        } else if (((PsiClass) referenced).isInterface()) {
          kind = ReferenceInfo.Kind.INTERFACE;
        } else {
          kind = ReferenceInfo.Kind.CLASS;
        }
      } else if (referenced instanceof PsiField) {
        kind = ReferenceInfo.Kind.FIELD;
      } else if (referenced instanceof PsiMethod) {
        kind = ReferenceInfo.Kind.METHOD;
      } else {
        assert referenced instanceof PsiPackage;
        kind = ReferenceInfo.Kind.PACKAGE;
      }
      return kind;
    }

    enum Context {
      CALL, // methods
      WRITE, READ, READ_WRITE, // fields (also methods in method refs)
      PACKAGE_STMT, STAR_IMPORT, IMPORT, // classes, packages
      EXTENDS, IMPLEMENTS, TYPE_BOUND, THROWS, TYPE_PARAM, PARAM_DECL, FIELD_DECL, LOCAL_DECL, RETURN_TYPE, CAST, INSTANCEOF, NEW // types
    };

    static Context getContext(PsiJavaCodeReferenceElement reference) {
      PsiElement parent = reference.getParent();
      if (parent == null || parent instanceof PsiReference) // nested or invalid reference, ignore inner parts
        return null;

      if (reference instanceof PsiReferenceExpression) { // expressions
        if (parent instanceof PsiMethodCallExpression)
          return Context.CALL;
        else if (parent instanceof PsiAssignmentExpression) {
          if (((PsiAssignmentExpression) parent).getLExpression() == reference)
            return Context.WRITE;
          else
            return Context.READ;
        } else if (parent instanceof PsiBinaryExpression)
          return Context.READ;
        else if (parent instanceof PsiPrefixExpression) {
          if (((PsiPrefixExpression) parent).getOperationTokenType() == JavaTokenType.EXCL || ((PsiPrefixExpression) parent).getOperationTokenType() == JavaTokenType.TILDE)
            return Context.READ;
          else
            return Context.READ_WRITE; // prefix ++ and --
        } else if (parent instanceof PsiPostfixExpression)
          return Context.READ_WRITE; // postfix ++ and --
        else if (parent instanceof PsiConditionalExpression)
          return Context.READ;
        else if (parent instanceof PsiTypeCastExpression)
          return Context.READ;
        else if (parent instanceof PsiInstanceOfExpression)
          return Context.READ;
        else if (parent instanceof PsiLocalVariable)
          return Context.READ; // initializer
        else if (parent instanceof PsiField)
          return Context.READ; // initializer
        else if (parent instanceof PsiStatement)
          return Context.READ; // throw, return, assert, if, while ...
        else {
          assert parent instanceof PsiExpressionList : "unexpected parent " + parent + " of expression " + reference;
          return Context.READ; // passed to a function as argument
        }

      } else { // types / imports / argument lists
        if (parent instanceof PsiPackageStatement)
          return ReferenceInfo.Context.PACKAGE_STMT;
        else if (parent instanceof PsiImportStatementBase) {
          if (((PsiImportStatement) parent).isOnDemand())
            return Context.STAR_IMPORT;
          else
            return Context.IMPORT;
        } else if (parent instanceof PsiReferenceList) {
          switch (((PsiReferenceList) parent).getRole()) {
            case THROWS_LIST: return Context.THROWS;
            case IMPLEMENTS_LIST: return Context.IMPLEMENTS;
            case EXTENDS_LIST: return Context.EXTENDS;
            case EXTENDS_BOUNDS_LIST: return Context.TYPE_BOUND;
            default:
              assert false : "unknown reference list role: " + parent;
              return null;
          }
        } else if (parent instanceof PsiTypeElement) {
          // this is a type either in a type argument list or as part of an extends clause in a type argument or as the type
          // of a variable or field
          PsiElement pp = parent.getParent();
          if (pp instanceof PsiField)
            return Context.FIELD_DECL;
          else if (pp instanceof PsiParameter)
            return Context.PARAM_DECL;
          else if (pp instanceof PsiLocalVariable)
            return Context.LOCAL_DECL;
          else if (pp instanceof PsiMethod)
            return Context.RETURN_TYPE;
          else if (pp instanceof PsiReferenceParameterList) // Stuff<Type>
            return Context.TYPE_PARAM;
          else if (pp instanceof PsiTypeCastExpression) // (Type)stuff
            return Context.CAST;
          else if (pp instanceof PsiInstanceOfExpression) // (Type)stuff
            return Context.INSTANCEOF;
          else {
            assert pp instanceof PsiTypeElement : "unexpected grandparent " + pp + " of " + parent; // ? extends Type
            return Context.TYPE_BOUND;
          }
        } else {
          assert parent instanceof PsiNewExpression : "unexpected parent " + parent + " of non-expression " + reference;
          return Context.NEW;
        }
      }
    }

    // counts of uses by context
    Map<Context,Integer> counts;

    public String toString() {
      StringBuilder sb = new StringBuilder(kind.toString());
      for (Map.Entry<Context,Integer> v : counts.entrySet()) {
        sb.append(' ');
        sb.append(v.getKey());
        sb.append(':');
        sb.append(v.getValue());
      }
      return sb.toString();
    }

    ReferenceInfo(Kind kind) {
      this.kind = kind;
    }
  }

  private void scan(final String basepath) {
    log("Starting scan of " + basepath);

    VirtualFile dir = myFixture.copyDirectoryToProject(basepath, basepath);

    final Map<String,ReferenceInfo> references = new HashMap<String, ReferenceInfo>();
    PsiRecursiveElementVisitor visitor = new PsiRecursiveElementVisitor() {
      @Override
      public void visitElement(PsiElement element) {
        if (element instanceof PsiJavaCodeReferenceElement) {

          // find out what we referenced
          PsiElement referenced = ((PsiReference)element).resolve();
          String fqn = null;
          if (referenced == null) {
            log(logger, Level.WARN, "cannot resolve reference " + ((PsiReference) element).getCanonicalText());
          } else {
            // get fully qualified name
            if (referenced instanceof PsiMember) {
              PsiClass cls = ((PsiMember) referenced).getContainingClass();
              fqn = cls != null ? cls.getQualifiedName() : null;
              if (fqn != null)
                fqn += '.' + ((PsiMember) referenced).getName();
            } else if (referenced instanceof PsiQualifiedNamedElement) {
              fqn = ((PsiQualifiedNamedElement) referenced).getQualifiedName();
            }
          }

          if (fqn != null) {

            // find the kind
            ReferenceInfo.Kind kind = ReferenceInfo.getKind(referenced);

            // get or add info (and check kind)
            ReferenceInfo referenceInfo = references.get(fqn);
            if (referenceInfo == null) {
              referenceInfo = new ReferenceInfo(kind);
              references.put(fqn, referenceInfo);
            } else {
              assert referenceInfo.kind == kind;
            }

            // find out in what context it's being referenced
            ReferenceInfo.Context context = ReferenceInfo.getContext((PsiJavaCodeReferenceElement) element);

            Integer count = referenceInfo.counts.get(context);
            if (count == null)
              count = 0;
            referenceInfo.counts.put(context, count+1);
          }
        }
        super.visitElement(element);
      }
    };

    Stack<VirtualFile> toProcess = new Stack<VirtualFile>();
    toProcess.push(dir);

    while (!toProcess.isEmpty()) {
      VirtualFile file = toProcess.pop();

      // recurse file hierarchy
      if (file.isDirectory()) {
        for (final VirtualFile child : file.getChildren())
          toProcess.push(child);
        continue;
      }

      // skip invalid files
      if (file.getFileType() != StdFileTypes.JAVA)
        continue;

      PsiFile psifile = PsiManager.getInstance(getProject()).findFile(file);
      if (psifile == null) {
        log("can't get psi for file " + file);
        continue;
      }

      // scan the file.
      log("scanning file " + file);
      psifile.accept(visitor);
    }

    // clean up (simply delete the file)
    try {
      dir.delete(null);
    } catch (IOException e) {
      log("failed to clean up after scan of " + basepath);
      throw new RuntimeException("failed to clean up after scan of " + basepath + ", error: " + e);
    }

    // write results to output file
    try {
      PrintStream outputrefs = new PrintStream(new File(getTestDataPath(), basepath + ".refs"));

      for (final Map.Entry<String, ReferenceInfo> e : references.entrySet()) {
        outputrefs.append(e.getKey() + ' ' + e.getValue() + '\n');
      }

      outputrefs.flush();
      outputrefs.close();
    } catch (IOException e) {
      log("failed to write results to file.");
      throw new RuntimeException("failed to write results to file, error: " + e);
    }
  }

  public void testScan() {
    PsiManager.getInstance(getProject()).startBatchFilesProcessingMode();

    // put all things to be scanned here
    // TODO: just enumerate subdirectories
    scan("scan1");

    PsiManager.getInstance(getProject()).finishBatchFilesProcessingMode();
  }

}
