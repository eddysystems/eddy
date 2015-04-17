package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.project.DumbService;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.tree.ChildRole;
import com.intellij.psi.impl.source.tree.CompositeElement;
import com.intellij.psi.impl.source.tree.JavaElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import tarski.ValueByItemQuery;

import java.util.*;

public class EddyPsiListener implements PsiTreeChangeListener, DumbService.DumbModeListener {

  final Set<PsiElement> queuedElements = new HashSet<PsiElement>();

  final ChangeTracker<String> nameTracker;
  final ChangeTracker<TypeNameItemNamePair> fieldTracker, methodTracker;

  public EddyPsiListener(final ChangeTracker<String> nameTracker,
                         final ChangeTracker<TypeNameItemNamePair> fieldTracker,
                         final ChangeTracker<TypeNameItemNamePair> methodTracker) {
    this.nameTracker = nameTracker;
    this.fieldTracker = fieldTracker;
    this.methodTracker = methodTracker;
  }

  private boolean isObject(PsiType t) {
    if (t instanceof PsiClassType && ((PsiClassType)t).getClassName().equals("Object")) {
      return isObject(((PsiClassType) t).resolve());
    }
    return false;
  }

  private boolean isObject(PsiClass c) {
    return c != null && "java.lang.Object".equals(c.getQualifiedName());
  }

  private final static ArrayList<String> empty = new ArrayList<String>();

  private Iterable<String> superTypes(final PsiType type) {
    // make sure we're not Object first
    if (isObject(type))
      return empty;
    return superTypes(type.getCanonicalText(), type.getSuperTypes());
  }

  private Iterable<String> superTypes(final PsiClass cls) {
    if (isObject(cls))
      return empty;
    return superTypes(cls.getName(), cls.getSuperTypes());
  }

  private Iterable<String> superTypes(final String name, final PsiType[] supers) {
    return new Iterable<String>() {
      @Override
      public Iterator<String> iterator() {
        final Stack<PsiType> work = new Stack<PsiType>();
        final Set<PsiType> seen = new HashSet<PsiType>();

        for (final PsiType s : supers) {
          if (!seen.contains(s)) {
            seen.add(s);
            work.push(s);
          }
        }

        return new Iterator<String>() {
          private String next = name;

          private String generateNext() {
            while (!work.empty()) {
              PsiType t = work.pop();
              // we don't want generics in here
              if (t instanceof PsiClassType) {
                // never add java.lang.Object
                if (isObject(t))
                  continue;
                t = ((PsiClassType)t).rawType();
              }

              for (final PsiType s : t.getSuperTypes()) {
                if (!seen.contains(s)) {
                  seen.add(s);
                  work.push(s);
                }
              }

              // add to map
              return t.getCanonicalText();
            }
            return null;
          }

          @Override
          public boolean hasNext() {
            return next != null;
          }

          @Override
          public String next() {
            String old = next;
            next = generateNext();
            return old;
          }

          @Override
          public void remove() {
            throw new UnsupportedOperationException();
          }
        };
      }
    };
  }

  // a new field appeared (or a field changed its name)
  private void addField(final PsiField f) {
    // put this field into the string map for its type and all its supertypes
    final @Nullable String name = f.getName();
    if (name != null)
      for (final String type : superTypes(f.getType())) {
        //log("add field " + f);
        fieldTracker.add(new TypeNameItemNamePair(type, name));
      }
  }

  // a new method appeared (or a method changed its name)
  private void addMethod(final PsiMethod m) {
    if (ValueByItemQuery.nullaryMethods) {
      final PsiType type = m.getReturnType();
      if (ByItem.considerMethod(m, type)) {
        // put this method into the string map for its type and all its supertypes
        final @NotNull String name = m.getName();
        for (final String sup : superTypes(type)) {
          //log("add method " + f);
          methodTracker.add(new TypeNameItemNamePair(sup, name));
        }
      }
    }
  }

  // all fields of this type (or any subtype) have to appear in the new superclasses
  private void updateInheritance(PsiClass cls) {
    // TODO: get all current values of cls and add them to all new supers of cls
  }

  // all fields of this type (or any supertype) to its new name
  private void changeClassName(String oldName, PsiClass cls) {
    // TODO: get all current values of oldName and add them to cls.getName()
  }

  @Override public void beforeChildAddition(@NotNull PsiTreeChangeEvent event) {}
  @Override public void beforeChildRemoval(@NotNull PsiTreeChangeEvent event) {}
  @Override public void beforeChildReplacement(@NotNull PsiTreeChangeEvent event) {}
  @Override public void beforeChildMovement(@NotNull PsiTreeChangeEvent event) {}

  boolean isName(PsiElement elem) {
    return elem instanceof PsiIdentifier &&
      ((CompositeElement)elem.getParent().getNode()).getChildRole(elem.getNode()) == ChildRole.NAME;
  }

  private boolean isBase(PsiElement elem) {
    return elem instanceof PsiJavaCodeReferenceElement &&
      elem.getParent() != null && elem.getParent().getParent() instanceof PsiClass &&
      elem.getParent().getNode().getElementType() == JavaElementType.EXTENDS_LIST;
  }

  private boolean isImplements(PsiElement elem) {
    return elem instanceof PsiJavaCodeReferenceElement &&
      elem.getParent() != null && elem.getParent().getParent() instanceof PsiClass &&
      elem.getParent().getNode().getElementType() == JavaElementType.IMPLEMENTS_LIST;
  }

  private void addElement(PsiElement elem) {
    // in dumb mode, don't try to resolve types etc.
    if (DumbService.getInstance(elem.getProject()).isDumb()) {
      queuedElements.add(elem);
      return;
    }

    if (elem instanceof PsiField || elem instanceof PsiMethod && !((PsiMethod)elem).isConstructor() || elem instanceof PsiClass) {
      final String name = ((PsiNamedElement)elem).getName();
      //log("add name " + name);
      if (name != null)
        nameTracker.add(name);
    } else if (isName(elem)) {
      //log("add name " + elem.getText());
      final String text = elem.getText();
      if (text != null)
        nameTracker.add(elem.getText());
      final PsiElement parent = elem.getParent();
      if (parent instanceof PsiField)
        addField((PsiField)parent);
      else if (parent instanceof PsiMethod)
        addMethod((PsiMethod)parent);
    }

    if (elem instanceof PsiField)
      addField((PsiField) elem);
    else if (elem instanceof PsiMethod)
      addMethod((PsiMethod)elem);
    else if (isBase(elem) && isImplements(elem))
      updateInheritance((PsiClass)elem.getParent().getParent());
    else if (isName(elem) && elem.getParent() instanceof PsiClass) {
      // TODO: update class name (but old class name is not available here, needs to be remembered
    }
  }

  private void removeElement(PsiElement elem) {
    // we don't actually care enough to track this stuff
  }

  @Override
  public void childAdded(@NotNull PsiTreeChangeEvent event) {
    //log("child " + event.getChild() + " added to " + event.getParent());
    addElement(event.getChild());
  }

  @Override
  public void childRemoved(@NotNull PsiTreeChangeEvent event) {
    //log("child " + event.getChild() + " removed from " + event.getParent());
    removeElement(event.getChild());
  }

  @Override
  public void childReplaced(@NotNull PsiTreeChangeEvent event) {
    //log("child " + event.getOldChild() + " in " + event.getParent() + " replaced with " + event.getNewChild());
    removeElement(event.getOldChild());
    addElement(event.getNewChild());
  }

  // Can ignore these because we'll also get detailed events like childReplaced
  @Override
  public void beforeChildrenChange(@NotNull PsiTreeChangeEvent event) {
    // for now, whenever the PSI changes, assume our current eddy process is out of date completely
    EddyThread.kill();
  }

  // Can ignore these because we'll also get detailed events like childReplaced
  @Override
  public void childrenChanged(@NotNull PsiTreeChangeEvent event) {
    //log("children of " + event.getParent() + " changed.");
  }

  // Probably doesn't matter, and we've never seen it anyway
  @Override
  public void childMoved(@NotNull PsiTreeChangeEvent event) {
    //log("child " + event.getChild() + " moved from " + event.getOldParent() + " to " + event.getNewParent());
  }

  // Doesn't matter
  @Override
  public void beforePropertyChange(@NotNull PsiTreeChangeEvent event) {
    // for now, whenever the PSI changes, assume our current eddy process is out of date completely
    EddyThread.kill();
  }

  // Doesn't matter
  @Override
  public void propertyChanged(@NotNull PsiTreeChangeEvent event) {
    //log("property " + event.getPropertyName() + " changed in " + event.getElement());
  }

  @Override
  public void enteredDumbMode() {
  }

  @Override
  public void exitDumbMode() {
    // process all queued elements
    ApplicationManager.getApplication().assertIsDispatchThread();
    for (final PsiElement elem : queuedElements) {
      if (elem.isValid())
        addElement(elem);
    }
    queuedElements.clear();
  }
}
