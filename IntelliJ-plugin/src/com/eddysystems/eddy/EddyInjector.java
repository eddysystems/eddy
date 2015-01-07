package com.eddysystems.eddy;

import com.intellij.ide.highlighter.JavaFileType;
import com.intellij.openapi.fileEditor.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;

import static com.eddysystems.eddy.Utility.log;

public class EddyInjector implements FileEditorManagerListener {
  private final Project project;
  private final HashMap<FileEditor, EddyFileListener> injected = new HashMap<FileEditor, EddyFileListener>();

  public EddyInjector(Project project) {
    this.project = project;
  }

  @Override
  public void fileOpened(@NotNull FileEditorManager fileEditorManager, @NotNull VirtualFile virtualFile) {
    FileEditor[] editors = fileEditorManager.getAllEditors();
    for (FileEditor editor : editors) {
      inject(editor);
    }
  }

  private void inject(final FileEditor editor) {
    if (this.injected.containsKey(editor)) {
      log("not injecting into already injected editor");
      return;
    }

    if(!(editor instanceof TextEditor)) {
      this.injected.put(editor, null);
      log("not injecting into non-text editor");
      return;
    }

    // make sure we have a Psi aware editor here
    PsiFile psifile = PsiDocumentManager.getInstance(project).getPsiFile(((TextEditor) editor).getEditor().getDocument());
    if (psifile == null) {
      log("not injecting into non-psi editor");
      return;
    }

    // we can only deal with Java files
    if (!(psifile.getFileType() instanceof JavaFileType)) {
      log("not injecting into non-java editor");
      return;
    }

    // make a new eddy which will attach to the editor
    this.injected.put(editor, new EddyFileListener(project, (TextEditor)editor));
  }

  @Override
  public void fileClosed(@NotNull FileEditorManager fileEditorManager, @NotNull VirtualFile virtualFile) {
    FileEditor[] editors = fileEditorManager.getAllEditors(virtualFile);
    for (FileEditor editor : editors) {
      uninject(editor);
    }
  }

  private void uninject(FileEditor editor) {
    EddyFileListener eddyFileListener = injected.get(editor);
    if (eddyFileListener != null) {
      eddyFileListener.dispose();
    }
  }

  @Override
  public void selectionChanged(@NotNull FileEditorManagerEvent fileEditorManagerEvent) {
  }
}
