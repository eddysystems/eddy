package com.eddysystems.eddy;

import com.intellij.psi.*;
import com.intellij.psi.impl.PsiTreeChangeEventImpl;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

public class EddyPsiListener implements PsiTreeChangeListener {

  @NotNull final EnvironmentProcessor.JavaEnvironment env;

  EddyPsiListener(@NotNull final EnvironmentProcessor.JavaEnvironment env) {
    this.env = env;
  }

  public boolean isInsideCodeBlock(PsiElement elem) {
    return PsiTreeUtil.getParentOfType(elem, PsiCodeBlock.class, true) != null;
  }

  // we have write access inside these callbacks

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
    }

    // TODO: walk up tree to see if this operation changes an item (e.g. by adding a modifier or a super)
    elem = event.getParent();
    // ...
  }

  @Override
  public void beforeChildRemoval(@NotNull PsiTreeChangeEvent event) {
    System.out.println("child being removed from " + event.getParent() + ": " + event.getChild());
    // TODO: traverse children of event.getChild() (incl itself) to see which are items that need to be deleted. Delete them.
    // TODO: walk up tree to see if this operation changes an item (e.g. by deleting one of its extends, or one of its modifiers)
    // TODO: if a PsiPackageStatement is deleted, all classes in this file suddenly switch to LocalPkg!
  }

  @Override
  public void childRemoved(@NotNull PsiTreeChangeEvent event) {
    // not so useful, the removed Psi node is already dead
  }

  @Override
  public void beforeChildReplacement(@NotNull PsiTreeChangeEvent event) {
    System.out.println("child of " + event.getParent() + ": " + event.getOldChild() + " being replaced with something new");

    // TODO: properly translate to delete/add pair
    // TODO: if a PsiPackageStatement is modified, all classes in this file change to the package with the new name!
  }

  @Override
  public void childReplaced(@NotNull PsiTreeChangeEvent event) {
    // if a single thing is replaced with more than one thing, we are only notified about the last new child in here.
    System.out.println("child of " + event.getParent() + ": " + event.getOldChild() + " replaced with " + event.getNewChild());

    // TODO: translate to proper delete/add pair
  }

  @Override
  public void beforeChildrenChange(@NotNull PsiTreeChangeEvent event) {
  }

  @Override
  public void childrenChanged(@NotNull PsiTreeChangeEvent event) {
    System.out.println("children of " + event.getParent() + " changed");
    // TODO: this is where all the work should happen: all the changes are done now. This is called with the file affected after all changes are done.
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

    // we may be able to do better in some cases, but for now, simply translate this to delete/add pair

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
