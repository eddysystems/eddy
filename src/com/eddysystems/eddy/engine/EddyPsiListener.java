package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.EddyThread;
import com.intellij.psi.PsiTreeChangeEvent;
import com.intellij.psi.PsiTreeChangeListener;
import org.jetbrains.annotations.NotNull;

public class EddyPsiListener implements PsiTreeChangeListener {

  @Override
  public void beforeChildAddition(@NotNull PsiTreeChangeEvent event) {
    // for now, whenever the PSI changes, assume our current eddy process is out of date completely
    EddyThread.kill();
  }

  @Override
  public void childAdded(@NotNull PsiTreeChangeEvent event) {
  }

  @Override
  public void beforeChildRemoval(@NotNull PsiTreeChangeEvent event) {
    // for now, whenever the PSI changes, assume our current eddy process is out of date completely
    EddyThread.kill();
  }

  @Override
  public void childRemoved(@NotNull PsiTreeChangeEvent event) {
  }

  @Override
  public void beforeChildReplacement(@NotNull PsiTreeChangeEvent event) {
    // for now, whenever the PSI changes, assume our current eddy process is out of date completely
    EddyThread.kill();
  }

  @Override
  public void childReplaced(@NotNull PsiTreeChangeEvent event) {
  }

  @Override
  public void beforeChildrenChange(@NotNull PsiTreeChangeEvent event) {
    // for now, whenever the PSI changes, assume our current eddy process is out of date completely
    EddyThread.kill();
  }

  @Override
  public void childrenChanged(@NotNull PsiTreeChangeEvent event) {
  }

  @Override
  public void beforeChildMovement(@NotNull PsiTreeChangeEvent event) {
    // for now, whenever the PSI changes, assume our current eddy process is out of date completely
    EddyThread.kill();
  }

  @Override
  public void childMoved(@NotNull PsiTreeChangeEvent event) {
  }

  @Override
  public void beforePropertyChange(@NotNull PsiTreeChangeEvent event) {
    // for now, whenever the PSI changes, assume our current eddy process is out of date completely
    EddyThread.kill();
  }

  @Override
  public void propertyChanged(@NotNull PsiTreeChangeEvent event) {
  }
}
