package com.eddysystems.eddy.engine;

import com.intellij.history.LocalHistory;
import com.intellij.history.LocalHistoryAction;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileTypes.StdFileTypes;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.module.StdModuleTypes;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.impl.JavaAwareProjectJdkTableImpl;
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl;
import com.intellij.openapi.roots.*;
import com.intellij.openapi.roots.impl.libraries.LibraryEx;
import com.intellij.openapi.vfs.LocalFileSystem;
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
import java.util.*;

import static com.eddysystems.eddy.engine.Utility.log;

/* Abuse the test framework to get a headless version of IntelliJ
*
* */
public class SymbolScanner extends LightCodeInsightFixtureTestCase {
  static Logger logger = Logger.getInstance(SymbolScanner.class);

  private static void doWriteAction(final Runnable action) {
    final Application application = ApplicationManager.getApplication();
    application.invokeAndWait(new Runnable() {
      @Override
      public void run() {
      application.runWriteAction(action);
    }
      }, application.getDefaultModalityState());
  }

  interface FileVisitor {
    // return false to not descend into
    boolean visit(VirtualFile file);
  };

  private static void traverse(VirtualFile dir, final FileVisitor visitor){
    Stack<VirtualFile> toProcess = new Stack<VirtualFile>();
    toProcess.push(dir);

    while (!toProcess.isEmpty()) {
      VirtualFile file = toProcess.pop();

      if (!visitor.visit(file)) {
        continue;
      }

      // recurse file hierarchy
      if (file.isDirectory()) {
        for (final VirtualFile child : file.getChildren())
          toProcess.push(child);
        continue;
      }
    }
  }

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
      log("configuring module " + module);

      model.getModuleExtension(LanguageLevelModuleExtension.class).setLanguageLevel(LanguageLevel.JDK_1_8);

      // add maven's repository to the module as a library
      final LibraryEx library = (LibraryEx)model.getModuleLibraryTable().createLibrary("maven");
      final LibraryEx.ModifiableModelEx libraryModel = library.getModifiableModel();

      // recurse into maven repository and find all jars (and add the ones we need)
      Stack<VirtualFile> toProcess = new Stack<VirtualFile>();
      VirtualFile mavenDir = LocalFileSystem.getInstance().findFileByPath("/Users/martin/.m2");
      toProcess.push(mavenDir);

      while (!toProcess.isEmpty()) {
        VirtualFile file = toProcess.pop();

        // recurse file hierarchy
        if (file.isDirectory()) {
          for (final VirtualFile child : file.getChildren())
            toProcess.push(child);
          continue;
        }

        // add jar files as libraries
        if ("jar".equals(file.getExtension())) {
          //log("processing maven jar " + file.getName());
          libraryModel.addRoot("jar://" + file.getPath() + "!/", OrderRootType.CLASSES);
        }
      }

      LibraryOrderEntry entry = model.findLibraryOrderEntry(library);
      assert entry != null : library;
      entry.setScope(DependencyScope.COMPILE);
      entry.setExported(true);

      doWriteAction(new Runnable() {
        @Override
        public void run() {
          libraryModel.commit();
        }
      });
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

  protected void setUp() throws Exception {
    super.setUp();
  }

  protected void tearDown() throws Exception {
    super.tearDown();
  }

  // should be put in a Map indexed by the fully qualified name
  // this class captures information about references to a specific symbol
  // from a single project.
  public static class ReferenceInfo {
    // is this symbol from the same project?
    final boolean fromSameProject;

    // count the number of times it's been referenced from its own file
    int sameFileReferences = 0;

    // what kind of symbol is this?
    enum Kind {PACKAGE, CLASS, INTERFACE, ENUM, FIELD, METHOD};
    final Kind kind;

    public static Kind getKind(PsiElement referenced) {
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

    public enum Context {
      ANNOTATION,
      CALL, // methods
      WRITE, READ, READ_WRITE, // fields (also methods in method refs)
      PACKAGE_STMT, STAR_IMPORT, IMPORT, // classes, packages
      EXTENDS, IMPLEMENTS, TYPE_BOUND, THROWS, TYPE_PARAM, PARAM_DECL, FIELD_DECL, LOCAL_DECL, RETURN_TYPE, CAST, INSTANCEOF, NEW // types
    };

    public static Context getContext(PsiJavaCodeReferenceElement reference, PsiElement parent) {
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
          if (((PsiImportStatementBase) parent).isOnDemand())
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
        } else if (parent instanceof PsiNewExpression)
          return Context.NEW;
        else {
          assert parent instanceof PsiAnnotation : "unexpected parent " + parent + " of non-expression " + reference;
          return Context.ANNOTATION;
        }
      }
    }

    // counts of uses by context
    public final Map<Context,Integer> counts = new EnumMap<Context, Integer>(Context.class);

    public String toString() {
      StringBuilder sb = new StringBuilder(kind.toString());
      sb.append(" sameProject:"); sb.append(fromSameProject);
      sb.append(" sameFileRefs:"); sb.append(sameFileReferences);
      for (Map.Entry<Context,Integer> v : counts.entrySet()) {
        sb.append(' ');
        sb.append(v.getKey());
        sb.append(':');
        sb.append(v.getValue());
      }
      return sb.toString();
    }

    public ReferenceInfo(Kind kind, boolean sameProject) {
      this.kind = kind;
      this.fromSameProject = sameProject;
    }
  }

  private boolean visitElement(final Map<String,ReferenceInfo> references, PsiElement element) {

    if (element instanceof PsiJavaCodeReferenceElement) {

      PsiElement parent = element.getParent();
      // nested or invalid reference, ignore inner parts
      if (parent == null || parent instanceof PsiReference)
        return false;

      // find out in what context it's being referenced
      ReferenceInfo.Context context = ReferenceInfo.getContext((PsiJavaCodeReferenceElement) element, parent);

      // find out what we referenced
      PsiElement referenced = ((PsiReference)element).resolve();
      String fqn = null;
      if (referenced == null) {
        log(logger, Level.WARN, "cannot resolve reference " + ((PsiReference) element).getCanonicalText() + " in context " + context);
      } else {
        // get fully qualified name
        if (referenced instanceof PsiField || referenced instanceof PsiMethod) {
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

        // is the symbol from this project?
        boolean sameProject;
        if (referenced instanceof PsiPackage) {
          // if it's a package, and we're referencing it from a package statement, it counts as a same file reference,
          // otherwise, it's foreign (getContainingFile won't be resolvable for packages)
          sameProject = context == ReferenceInfo.Context.PACKAGE_STMT;
        } else {
          sameProject = ProjectRootManager.getInstance(getProject()).getFileIndex().getContentRootForFile(referenced.getContainingFile().getVirtualFile()) != null;
        }

        // get or add info (and check kind)
        ReferenceInfo referenceInfo = references.get(fqn);
        if (referenceInfo == null) {
          referenceInfo = new ReferenceInfo(kind, sameProject);
          references.put(fqn, referenceInfo);
        } else {
          assert referenceInfo.kind == kind;
        }

        // check if same file reference
        if (referenced.getContainingFile() == element.getContainingFile()) {
          referenceInfo.sameFileReferences++;
        }

        if (context != null) {
          Integer count = referenceInfo.counts.get(context);
          if (count == null)
            count = 0;
          referenceInfo.counts.put(context, count+1);
        } else {
          log("ignoring reference in uninteresting context: " + element);
        }
      } else {
        log("ignoring local symbol: " + element + " in context " + context);
      }
    }
    return true;
  }

  private void scan(final String basepath) {
    log("Starting scan of " + basepath);

    final VirtualFile dir = myFixture.copyDirectoryToProject(basepath, basepath);

    log("  base path is " + dir.getCanonicalPath());

    final Map<String,ReferenceInfo> references = new HashMap<String, ReferenceInfo>();
    final PsiRecursiveElementVisitor visitor = new PsiRecursiveElementVisitor() {
      @Override
      public void visitElement(PsiElement element) {
        if (SymbolScanner.this.visitElement(references, element))
          super.visitElement(element);
      }
    };

    final List<ContentEntry> roots = new ArrayList<ContentEntry>();
    final ModifiableRootModel model = ModuleRootManager.getInstance(myModule).getModifiableModel();
    traverse(dir, new FileVisitor() {
      @Override
      public boolean visit(final VirtualFile file) {
        if (file.isDirectory() && "java".equals(file.getName())) {
          log("adding root " + file.getCanonicalPath());
          ContentEntry entry = model.addContentEntry(file);
          roots.add(entry);
          return false;
        } else {
          return true;
        }
      }
    });
    doWriteAction(new Runnable() {
      @Override
      public void run() {
        model.commit();
      }
    });

    traverse(dir, new FileVisitor() {
      @Override
      public boolean visit(VirtualFile file) {
        // skip invalid files
        if (file.getFileType() != StdFileTypes.JAVA)
          return true;

        PsiFile psifile = PsiManager.getInstance(getProject()).findFile(file);
        if (psifile == null) {
          log("can't get psi for file " + file);
          return false;
        }

        // scan the file.
        log("scanning file " + file);
        psifile.accept(visitor);
        return false;
      }
    });

    // clean up (simply delete the file, and remove the root entries)
    doWriteAction(new Runnable(){
      @Override public void run() {
        try {
          for (final ContentEntry entry : roots)
            ModuleRootManager.getInstance(myModule).getModifiableModel().removeContentEntry(entry);
          dir.delete(null);
        } catch (IOException e) {
          log("failed to clean up after scan of " + basepath);
          throw new RuntimeException("failed to clean up after scan of " + basepath + ", error: " + e);
        }
      }
    });

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
    assert ApplicationManager.getApplication().isUnitTestMode();
    LocalHistoryAction action = LocalHistory.getInstance().startAction("Scan");
    PsiManager.getInstance(getProject()).startBatchFilesProcessingMode();

    // put all things to be scanned here
    // TODO: just enumerate subdirectories
    scan("picasso");

    PsiManager.getInstance(getProject()).finishBatchFilesProcessingMode();
    action.finish();
  }

}
