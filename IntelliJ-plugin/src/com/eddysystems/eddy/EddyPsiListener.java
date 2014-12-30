package com.eddysystems.eddy;

import com.intellij.psi.*;
import com.intellij.psi.impl.PsiTreeChangeEventImpl;
import com.intellij.psi.impl.source.tree.ChildRole;
import com.intellij.psi.impl.source.tree.CompositeElement;
import com.intellij.psi.impl.source.tree.JavaElementType;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

public class EddyPsiListener implements PsiTreeChangeListener {

  @NotNull final EnvironmentProcessor.JavaEnvironment env;

  EddyPsiListener(@NotNull final EnvironmentProcessor.JavaEnvironment env) {
    this.env = env;
  }

  // what thing is this?
  enum ElemType { MODIFIER, NAME, TYPE_PARAMETER, BASE, IMPLEMENTS, RETURN_TYPE, PARAMETER, UNKNOWN }
  ElemType elemType(PsiElement elem) {
    if (isModifier(elem)) {
      return ElemType.MODIFIER;
    } else if (isTypeParameter(elem)) {
      return ElemType.TYPE_PARAMETER;
    } else if (isBaseClass(elem)) {
      return ElemType.BASE;
    } else if (isImplemented(elem)) {
      return ElemType.IMPLEMENTS;
    } else if (isParameter(elem)) {
      return ElemType.PARAMETER;
    } else if (isReturnType(elem)) {
      return ElemType.RETURN_TYPE;
    } else if (isName(elem)) {
      return ElemType.NAME;
    } else
      return ElemType.UNKNOWN;
  }

  // map to keep elem type info around between before and after events
  final Map<PsiElement, ElemType> elemTypes = new HashMap<PsiElement, ElemType>();

  boolean isInsideCodeBlock(PsiElement elem) {
    return PsiTreeUtil.getParentOfType(elem, PsiCodeBlock.class, true) != null;
  }

  boolean isModifier(PsiElement elem) {
    return elem.getParent() instanceof PsiModifierList &&
           elem instanceof PsiKeyword;
  }

  boolean isName(PsiElement elem) {
    return elem instanceof PsiIdentifier && ((CompositeElement)elem.getParent().getNode()).getChildRole(elem.getNode()) == ChildRole.NAME;
  }

  public boolean isTypeParameter(PsiElement elem) {
    return elem instanceof PsiTypeParameter;
  }

  boolean isBaseClass(PsiElement elem) {
    return elem instanceof PsiJavaCodeReferenceElement &&
      elem.getParent() != null && elem.getParent().getParent() instanceof PsiClass &&
      elem.getParent().getNode().getElementType() == JavaElementType.EXTENDS_LIST;
  }

  boolean isImplemented(PsiElement elem) {
    return elem instanceof PsiJavaCodeReferenceElement &&
      elem.getParent() != null && elem.getParent().getParent() instanceof PsiClass &&
      elem.getParent().getNode().getElementType() == JavaElementType.IMPLEMENTS_LIST;
  }

  boolean isReturnType(PsiElement elem) {
    // types that are direct children of methods are their return type
    return elem instanceof PsiTypeElement && elem.getParent() != null && elem.getParent() instanceof PsiMethod;
  }

  boolean isParameter(PsiElement elem) {
    return elem instanceof PsiParameter;
  }


  private boolean deleteRecursive(PsiElement elem) {
    // depth first -- we want to be rid of everything below before we delete the common parent
    PsiElement child = elem.getFirstChild();
    while (child != null) {
      deleteRecursive(child);
      child = child.getNextSibling();
    }

    if (elem instanceof PsiClass || elem instanceof PsiField || elem instanceof PsiMethod) {
      env.deleteItem(elem);
      return true;
    }

    return false;
  }

  // elem must already be changed
  private void changeUpward(PsiElement p, ElemType et) {
    PsiElement gp = p.getParent();
    switch (et) {
      case BASE: env.changeBase((PsiClass)gp); break;
      case IMPLEMENTS: env.changeImplements((PsiClass)gp); break;
      case MODIFIER: if (gp instanceof PsiClass || gp instanceof PsiMethod || gp instanceof PsiField) env.changeModifiers((PsiModifierListOwner)gp); break;
      case NAME: env.changeItemName((PsiNamedElement)p); break;
      case PARAMETER: env.changeParameters((PsiMethod)gp); break;
      case RETURN_TYPE: env.changeReturnType((PsiMethod)p); break;
      case TYPE_PARAMETER: env.changeTypeParameters((PsiTypeParameterListOwner)gp); break;
      case UNKNOWN: break;
    }
  }

  // we have write access inside these callbacks

  // We care about: PsiPackageStatement, PsiClass (incl. interfaces, enums), PsiField (incl. enum constants), PsiMethod (incl. constructors).

  @Override
  public void beforeChildAddition(@NotNull PsiTreeChangeEvent event) {
  }

  @Override
  public void childAdded(@NotNull PsiTreeChangeEvent event) {
    System.out.println("child added to " + event.getParent() + ": " + event.getChild());
    PsiElement elem = event.getChild();

    // we're invisible to the outside if we're inside a code block, or if we're inside a local class (inside a code block)
    if (isInsideCodeBlock(elem))
      return;

    if (elem instanceof PsiClass || elem instanceof PsiField || elem instanceof PsiMethod) {
      env.addItem(elem);
      return;
    }

    // TODO: if a PsiPackageStatement is added, all classes in this file suddenly switch to the named package

    // the rest are item modifications of a containing item up the tree
    changeUpward(elem.getParent(), elemType(elem));
  }

  @Override
  public void beforeChildRemoval(@NotNull PsiTreeChangeEvent event) {
    System.out.println("child being removed from " + event.getParent() + ": " + event.getChild());
    if (deleteRecursive(event.getChild())) {
      // if the removed child itself was an item we can delete, we're done here.
      return;
    }

    // TODO: if a PsiPackageStatement is deleted, all classes in this file suddenly switch to LocalPkg!

    // save type of element to be deleted
    elemTypes.put(event.getChild(), elemType(event.getChild()));
  }

  @Override
  public void childRemoved(@NotNull PsiTreeChangeEvent event) {
    // propagate changes up the tree
    changeUpward(event.getParent(), elemTypes.get(event.getChild()));
    elemTypes.remove(event.getChild());
  }

  @Override
  public void beforeChildReplacement(@NotNull PsiTreeChangeEvent event) {
    System.out.println("child of " + event.getParent() + ": " + event.getOldChild() + " being replaced with something new");

    // if complete items are replaced, translate to delete/add pair
    PsiElement elem = event.getOldChild();
    if (elem instanceof PsiClass || elem instanceof PsiField || elem instanceof PsiMethod) {
      env.deleteItem(elem);
      return;
    }

    // save type of element to be deleted
    elemTypes.put(elem, elemType(elem));

    // TODO: if a PsiPackageStatement is modified, all classes in this file change to the package with the new name
  }

  @Override
  public void childReplaced(@NotNull PsiTreeChangeEvent event) {
    System.out.println("child of " + event.getParent() + ": " + event.getOldChild() + " replaced with " + event.getNewChild());

    // whole items, translate to delete/add pair
    PsiElement elem = event.getNewChild();
    if (elem instanceof PsiClass || elem instanceof PsiField || elem instanceof PsiMethod) {
      env.addItem(elem);
      return;
    }

    // partial changes, propagate up
    ElemType et = elemTypes.get(event.getOldChild());
    elemTypes.remove(event.getOldChild());
    ElemType etnew = elemType(elem);
    changeUpward(event.getParent(), et);
    if (et != etnew)
      changeUpward(event.getParent(), etnew);
  }

  @Override
  public void beforeChildrenChange(@NotNull PsiTreeChangeEvent event) {
  }

  @Override
  public void childrenChanged(@NotNull PsiTreeChangeEvent event) {
    System.out.println("children of " + event.getParent() + " changed");
    // called once per file if stuff inside the file changed
  }

  @Override
  public void beforeChildMovement(@NotNull PsiTreeChangeEvent event) {
    System.out.println("child of " + event.getOldParent() + " moving to " + event.getNewParent() + ": " + event.getChild());

    // just changing order doesn't affect us
    if (event.getOldParent() == event.getNewParent())
      return;

    // we may be able to do better in some cases, but for now, simply translate this to delete/add pair
    // TODO: do something smarter whenever we can: for instance, if this is a field or method, we can simple change its parent

    // delete here, add after the move is complete
    PsiTreeChangeEventImpl delevent = new PsiTreeChangeEventImpl(PsiManager.getInstance(env.project));
    delevent.setChild(event.getChild());
    delevent.setParent(event.getOldParent());
    beforeChildRemoval(delevent);
  }

  @Override
  public void childMoved(@NotNull PsiTreeChangeEvent event) {
    // I've never seen this callback actually happen, may be an optimization over remove/add in special cases
    System.out.println("child of " + event.getOldParent() + " moved to " + event.getNewParent() + ": " + event.getChild());

    // just changing order doesn't affect us
    if (event.getOldParent() == event.getNewParent())
      return;

    // TODO: we may be able to do better in some cases, but for now, simply translate this to delete/add pair

    // add here, delete happened before the move
    PsiTreeChangeEventImpl addevent = new PsiTreeChangeEventImpl(PsiManager.getInstance(env.project));
    addevent.setChild(event.getChild());
    addevent.setParent(event.getNewParent());
    childAdded(addevent);
  }

  @Override
  public void beforePropertyChange(@NotNull PsiTreeChangeEvent event) {
  }

  @Override
  public void propertyChanged(@NotNull PsiTreeChangeEvent event) {
    // these are not particularly interesting properties for us
    System.out.println("property " + event.getPropertyName() + " of " + event.getElement() + " changed from " + event.getOldValue() + " to " + event.getNewValue());
  }
}
