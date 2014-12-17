package com.eddysystems.eddy;

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class Place {
  public final @NotNull Project project;
  public final @Nullable PsiElement place;
  public final @Nullable PsiJavaFile file;
  public final @Nullable PsiClass placeClass;
  public final @Nullable PsiPackage pkg;

  Place(Project project, PsiElement place) {
    this.project = project;
    this.place = place;
    file = PsiTreeUtil.getParentOfType(place, PsiJavaFile.class, false);
    placeClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
    pkg = getPackage(file);

    //System.out.println("Place at: " + place + " in class " + placeClass + " in package " + pkg + " in file " + file + " in project " + project);
  }

  @Nullable PsiPackage getPackage(PsiJavaFile file) {
    return file == null ? null : JavaPsiFacade.getInstance(project).findPackage(file.getPackageName());
  }

  PsiElement containing(PsiElement elem) {
    PsiElement parent = elem.getParent();
    if (parent instanceof PsiPackage) {
      return parent;
    } else if (parent instanceof PsiJavaFile) {
      return getPackage((PsiJavaFile)parent);
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
    }
    throw new RuntimeException("unexpected container of " + elem + ": " + parent);
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
   * @return whether the element is private or protected and not accessible because of it
   */
  boolean isInaccessible(PsiModifierListOwner element, boolean noProtected) {
    // TODO: this does not fully take into account the crazier access rules for protected members (6.6.1/6.6.2)
    // TODO: this deserves some unit tests, must be multi-package

    PsiElement container = containing(element);

    if (container instanceof PsiPackage) {
      if (element.hasModifierProperty(PsiModifier.PUBLIC)) {
        return false;
      } else {
        if (noProtected) {
          //System.out.println(element + " is not public inside package " + container);
          return true;
        }

        if (file != null && container != pkg) {
          //System.out.println(element + " is inaccessible because it is package-local in package " + container);
          return true;
        }

        return false;
      }
    }

    if (element.hasModifierProperty(PsiModifier.PRIVATE)) {
      if (noProtected) {
        //System.out.println(element + " is private inside " + container);
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
          //System.out.println(element + " is inaccessible because it is private inside " + container);
          return true;
        }
      }
    } else if (element.hasModifierProperty(PsiModifier.PROTECTED)) {
      if (noProtected) {
        //System.out.println(element + " is protected inside " + container);
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
            //System.out.println(element + " is inaccessible because it is protected inside " + container);
            return true;
          } else
            return false;
        }
      }
    } else if (!element.hasModifierProperty(PsiModifier.PUBLIC)) {
      // package access, only allowed if we're in the same package
      if (noProtected) {
        //System.out.println(element + " is not public inside " + container);
        return true;
      }

      if (!samePackage(container)) {
        //System.out.println(element + " is inaccessible because it is package-local inside " + getPackage(file));
        return true;
      } else
        return false;
    }

    if (container instanceof PsiModifierListOwner) {
      boolean ii = isInaccessible((PsiModifierListOwner)container, noProtected);
      //if (ii) System.out.println("  " + element + " is inaccessible because its container " + container + " is inaccessible");
      return ii;
    } else
      // null
      return false;
  }
}
