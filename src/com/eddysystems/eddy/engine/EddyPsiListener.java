package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.psi.PsiTreeChangeEvent;
import com.intellij.psi.PsiTreeChangeListener;
import org.jetbrains.annotations.NotNull;

import static com.eddysystems.eddy.engine.Utility.log;

public class EddyPsiListener implements PsiTreeChangeListener {

  // TODO: keep track of how many and which items we have changed, and request full or partial environment updates at appropriate frequencies

  // update byValue if
  // - field changes type
  // - new field
  // - regularly

  // update trie
  // - new class/field/method name (either added or name changed)

  @Override public void beforeChildAddition(@NotNull PsiTreeChangeEvent event) {}
  @Override public void beforeChildRemoval(@NotNull PsiTreeChangeEvent event) {}
  @Override public void beforeChildReplacement(@NotNull PsiTreeChangeEvent event) {}
  @Override public void beforeChildMovement(@NotNull PsiTreeChangeEvent event) {}

  @Override
  public void childAdded(@NotNull PsiTreeChangeEvent event) {
    log("child " + event.getChild() + " added to " + event.getParent());
  }

  @Override
  public void childRemoved(@NotNull PsiTreeChangeEvent event) {
    log("child " + event.getChild() + " removed from " + event.getParent());
  }

  @Override
  public void childReplaced(@NotNull PsiTreeChangeEvent event) {
    log("child " + event.getOldChild() + " in " + event.getParent() + " replaced with " + event.getNewChild());
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
    log("children of " + event.getParent() + " changed.");
  }

  // Probably doesn't matter, and we've never seen it anyways
  @Override
  public void childMoved(@NotNull PsiTreeChangeEvent event) {
    log("child " + event.getChild() + " moved from " + event.getOldParent() + " to " + event.getNewParent());
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
    log("property " + event.getPropertyName() + " changed in " + event.getElement());
  }
}
