package com.eddysystems.eddy;

import com.intellij.openapi.project.Project;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class Place {
  public final @NotNull Project project;
  public final @Nullable PsiElement place;

  Place(Project project, PsiElement place) {
    this.project = project;
    this.place = place;
  }

  @Nullable PsiPackage getPackage(@NotNull PsiJavaFile file) {
    return JavaPsiFacade.getInstance(project).findPackage(file.getPackageName());
  }

  PsiElement containing(PsiElement elem) {
    PsiElement parent = elem.getParent();
    if (parent instanceof PsiJavaFile) {
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

    /**
   * Check if a thing (member or class) is private or protected and therefore not accessible
   * @param element The thing to check whether it's inaccessible because it may be private or protected
   * @param noProtected Consider all protected, private, or package local items inaccessible
   * @return whether the element is private or protected and not accessible because of it
   */
  boolean isInaccessible(PsiModifierListOwner element, boolean noProtected) {
    PsiElement container = containing(element);

    if (container instanceof PsiPackage && !element.hasModifierProperty(PsiModifier.PUBLIC)) {
      if (noProtected)
        return true;

      PsiJavaFile file = PsiTreeUtil.getParentOfType(place, PsiJavaFile.class, false);
      if (file != null && container != getPackage(file))
        return true;

      return false;
    }

    if (element.hasModifierProperty(PsiModifier.PRIVATE)) {
      if (noProtected)
        return true;

      if (container instanceof PsiClass) {
        // if the member is private we can only see it if place is contained in a class in which member is declared.
        PsiClass containingPlaceClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
        while (containingPlaceClass != null) {
          if (container == containingPlaceClass) {
            break;
          }
          containingPlaceClass = PsiTreeUtil.getParentOfType(containingPlaceClass, PsiClass.class);
        }
        if (containingPlaceClass == null) {
          return true;
        }
      }
    }

    if (element.hasModifierProperty(PsiModifier.PROTECTED)) {
      if (noProtected)
        return true;

      // if the member is protected we can only see it if place is contained in a subclass of the containingClass
      if (container instanceof PsiClass) {
        PsiClass containingPlaceClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
        while (containingPlaceClass != null) {
          if (containingPlaceClass.isInheritor((PsiClass)container, true))
            break;
          containingPlaceClass = PsiTreeUtil.getParentOfType(containingPlaceClass, PsiClass.class);
        }
        if (containingPlaceClass == null) {
          return true;
        }
      }
    }
    return false;
  }
}
