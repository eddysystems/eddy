package com.eddysystems.eddy.engine;

import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.DummyHolder;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.eddysystems.eddy.engine.Utility.log;

class Place {
  public final @NotNull Project project;
  public final @Nullable PsiElement place;
  public final @Nullable PsiJavaFile file;
  public final @Nullable PsiClass placeClass;
  public final @Nullable PsiPackage pkg;

  Place(@NotNull Project project, @NotNull PsiElement place) {
    this.project = project;
    this.place = place;
    file = PsiTreeUtil.getParentOfType(place, PsiJavaFile.class, false);
    placeClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
    pkg = getPackage(file, project);

    //System.out.println("Place at: " + place + " in class " + placeClass + " in package " + pkg + " in file " + file + " in project " + project);
  }

  static class UnexpectedContainerError extends RuntimeException {
    UnexpectedContainerError(PsiElement elem) {
      super("unexpected container of " + elem + ": " + elem.getParent() + " in file " + elem.getContainingFile());
    }
  }

  static class UnknownPackageError extends RuntimeException {
    UnknownPackageError(PsiElement elem) {
      super("can't determine package of " + elem + " with parent " + elem.getParent() + " in file " + elem.getContainingFile());
    }
  }

  @Nullable static PsiPackage getPackage(PsiClassOwner file, @NotNull Project project) {
    if (file == null)
      return null;
    String pkgname = file.getPackageName();

    // TODO: this only works if the directory structure corresponds to packages. Can we do better?
    PsiPackage pkg = JavaPsiFacade.getInstance(project).findPackage(pkgname);
    return pkg;
  }

  static @Nullable VirtualFile getElementFile(@NotNull PsiElement elem) {
    PsiFile file = elem.getContainingFile();
    if (file == null || file instanceof DummyHolder) {
      if (elem.getContext() != null)
        return getElementFile(elem.getContext());
      else if (elem.getParent() != null)
        return getElementFile(elem.getParent());
      else
        return null;
    } else {
      return file.getVirtualFile();
    }
  }

  static PsiPackage getElementPackage(@NotNull PsiElement elem, @NotNull Project project) {
    assert (elem instanceof DummyHolder) || !(elem instanceof PsiDirectory) && !(elem instanceof PsiPackage) && !(elem instanceof PsiFile);
    if (elem.getContainingFile() instanceof PsiClassOwner) {
      return JavaPsiFacade.getInstance(project).findPackage(((PsiClassOwner)(elem.getContainingFile())).getPackageName());
    } else if (elem.getContext() != null)
      // synthetic elements don't believe that they're in a file. Usually though, their parent (or context) knows.
      return getElementPackage(elem.getContext(), project);

    throw new UnknownPackageError(elem);
  }

  static PsiElement containing(PsiElement elem, Project project) {

    PsiElement parent = elem.getParent();
    if (parent instanceof PsiPackage) {
      return parent;
    } else if (parent instanceof PsiClassOwner) { // more general than PsiJavaFile, also works for ScalaFile
      return getPackage((PsiClassOwner)parent, project);
    } else if (parent instanceof PsiClass) {
      return parent;
    } else if (parent instanceof PsiDeclarationStatement || // local variable
              (parent instanceof PsiForeachStatement) || (parent instanceof PsiForStatement) || // declaration in for loop
              (parent instanceof PsiParameterList)) { // parameter to callable
      while (!(parent instanceof PsiMethod))
        parent = parent.getParent();
      return parent;
    } else if (parent instanceof PsiTypeParameterList) {
      assert elem instanceof PsiTypeParameter;
      return ((PsiTypeParameter)elem).getOwner();
    } else if (elem instanceof PsiClass) {
      // maybe it's a class
      parent = ((PsiClass) elem).getContainingClass();
      if (parent != null) {
        return parent;
      }
      // maybe we can deduce the proper parent from the qualified name
      String qname = ((PsiClass) elem).getQualifiedName();
      if (qname != null) {
        int idx = qname.lastIndexOf('.');
        if (idx >= 0) {
          qname = qname.substring(0, idx);
          parent = JavaPsiFacade.getInstance(project).findPackage(qname);
          if (parent != null)
            return parent;
        }
      }
      // Otherwise, we're local, and if we can't figure out what we're local to, we might as well give up.
    } else if (elem instanceof PsiMember) {
      parent = ((PsiMember) elem).getContainingClass();
      if (parent != null)
        return parent;
      // now we're out of luck.
    }
    throw new UnexpectedContainerError(elem);
  }

  boolean samePackage(PsiElement element) {
    PsiJavaFile file = PsiTreeUtil.getParentOfType(element, PsiJavaFile.class, false);
    PsiPackage epkg = getPackage(file, project);
    return epkg == pkg;
  }

  // true if the element cannot be accessed from this place because it is inside an inaccessible element, because
  // it is private (and this is not inside the containing class), or because it is protected or package-local and
  // this is not within an inheriting class or the same package.
  boolean isInaccessible(PsiModifierListOwner element) {
    // TODO: this does not fully take into account the crazier access rules for protected members (6.6.1/6.6.2)
    // TODO: this deserves some unit tests, must be multi-package

    // if place is null, and noPrivate is true, then we check whether we

    PsiElement container = null;
    try {
      container = containing(element, project);
    } catch (UnexpectedContainerError e) {
      log(e.getMessage());
      return true;
    }

    if (container instanceof PsiPackage) {
      if (element.hasModifierProperty(PsiModifier.PUBLIC)) {
        return false;
      } else {
        if (file != null && container != pkg) {
          //log(element + " is inaccessible because it is package-local in package " + container);
          return true;
        }

        return false;
      }
    }

    if (element.hasModifierProperty(PsiModifier.PRIVATE)) {
      if (container instanceof PsiClass) {
        // if the member is private we can only see it if place is contained in a class in which member is declared.
        PsiClass containingPlaceClass = placeClass;
        while (containingPlaceClass != null) {
          if (container == containingPlaceClass) {
            break;
          }
          containingPlaceClass = PsiTreeUtil.getParentOfType(containingPlaceClass, PsiClass.class, true);
        }
        if (containingPlaceClass == null) {
          //lof(element + " is inaccessible because it is private inside " + container);
          return true;
        }
      }
    } else if (element.hasModifierProperty(PsiModifier.PROTECTED)) {
      // if the member is protected we can only see it if place is contained in a subclass of the containingClass (or we're in the same package)
      if (container instanceof PsiClass) {
        PsiClass containingPlaceClass = placeClass;
        while (containingPlaceClass != null) {
          if (containingPlaceClass.isInheritor((PsiClass)container, true))
            break;
          containingPlaceClass = PsiTreeUtil.getParentOfType(containingPlaceClass, PsiClass.class, true);
        }
        if (containingPlaceClass == null) {
          if (!samePackage(container)) {
            //log(element + " is inaccessible because it is protected inside " + container);
            return true;
          } else
            return false;
        }
      }
    } else if (!element.hasModifierProperty(PsiModifier.PUBLIC)) {
      if (!samePackage(container)) {
        //log(element + " is inaccessible because it is package-local inside " + getPackage(file));
        return true;
      } else
        return false;
    }

    if (container instanceof PsiModifierListOwner) {
      boolean ii = isInaccessible((PsiModifierListOwner)container);
      //if (ii) log("  " + element + " is inaccessible because its container " + container + " is inaccessible");
      return ii;
    } else
      // null
      return false;
  }
}
