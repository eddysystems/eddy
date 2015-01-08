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

  Place(@NotNull Project project, @Nullable PsiElement place) {
    this.project = project;
    this.place = place;
    file = PsiTreeUtil.getParentOfType(place, PsiJavaFile.class, false);
    placeClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
    pkg = getPackage(file);

    //System.out.println("Place at: " + place + " in class " + placeClass + " in package " + pkg + " in file " + file + " in project " + project);
  }

  class UnexpectedContainerError extends RuntimeException {
    UnexpectedContainerError(PsiElement elem) {
      super("unexpected container of " + elem + ": " + elem.getParent() + " in file " + elem.getContainingFile());
    }
  }

  class UnknownPackageError extends RuntimeException {
    UnknownPackageError(PsiElement elem) {
      super("can't determine package of " + elem + " with parent " + elem.getParent() + " in file " + elem.getContainingFile());
    }
  }

  @Nullable PsiPackage getPackage(PsiClassOwner file) {
    return file == null ? null : JavaPsiFacade.getInstance(project).findPackage(file.getPackageName());
  }

  static @Nullable VirtualFile getElementFile(@NotNull PsiElement elem) {
    PsiFile file = elem.getContainingFile();
    if (file == null || file instanceof DummyHolder) {
      if (elem.getContext() != null)
        return getElementFile(elem.getContext());
      else
        return null;
    } else {
      return file.getVirtualFile();
    }
  }

  @NotNull PsiPackage getElementPackage(@NotNull PsiElement elem) {
    assert (elem instanceof DummyHolder) || !(elem instanceof PsiDirectory) && !(elem instanceof PsiPackage) && !(elem instanceof PsiFile);
    if (elem.getContainingFile() instanceof PsiClassOwner)
      return JavaPsiFacade.getInstance(project).findPackage(((PsiClassOwner)(elem.getContainingFile())).getPackageName());
    else if (elem.getContext() != null)
      // synthetic elements don't believe that they're in a file. Usually though, their parent (or context) knows.
      return getElementPackage(elem.getContext());

    throw new UnknownPackageError(elem);
  }

  PsiElement containing(PsiElement elem) {

    PsiElement parent = elem.getParent();
    if (parent instanceof PsiPackage) {
      return parent;
    } else if (parent instanceof PsiClassOwner) { // more general than PsiJavaFile, also works for ScalaFile
      return getPackage((PsiClassOwner)parent);
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
    PsiPackage epkg = getPackage(file);
    return epkg == pkg;
  }

  /**
   * Check if a thing (member or class) is private or protected and therefore not accessible
   * @param element The thing to check whether it's inaccessible because it may be private or protected
   * @param noProtected Consider all protected, private, or package local items inaccessible
   * @return whether the element is not accessible
   */
  boolean isInaccessible(PsiModifierListOwner element, boolean noProtected) {
    // TODO: this does not fully take into account the crazier access rules for protected members (6.6.1/6.6.2)
    // TODO: this deserves some unit tests, must be multi-package

    PsiElement container = null;
    try {
      container = containing(element);
    } catch (UnexpectedContainerError e) {
      log(e.getMessage());
      return true;
    }

    if (container instanceof PsiPackage) {
      if (element.hasModifierProperty(PsiModifier.PUBLIC)) {
        return false;
      } else {
        if (noProtected) {
          //log(element + " is not public inside package " + container);
          return true;
        }

        if (file != null && container != pkg) {
          //log(element + " is inaccessible because it is package-local in package " + container);
          return true;
        }

        return false;
      }
    }

    if (element.hasModifierProperty(PsiModifier.PRIVATE)) {
      if (noProtected) {
        //log(element + " is private inside " + container);
        return true;
      }

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
      if (noProtected) {
        //log(element + " is protected inside " + container);
        return true;
      }

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
      // package access, only allowed if we're in the same package
      if (noProtected) {
        //log(element + " is not public inside " + container);
        return true;
      }

      if (!samePackage(container)) {
        //log(element + " is inaccessible because it is package-local inside " + getPackage(file));
        return true;
      } else
        return false;
    }

    if (container instanceof PsiModifierListOwner) {
      boolean ii = isInaccessible((PsiModifierListOwner)container, noProtected);
      //if (ii) log("  " + element + " is inaccessible because its container " + container + " is inaccessible");
      return ii;
    } else
      // null
      return false;
  }
}
