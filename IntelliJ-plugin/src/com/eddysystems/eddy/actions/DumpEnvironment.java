package com.eddysystems.eddy.actions;

import com.eddysystems.eddy.EddyFileListener;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;

public class DumpEnvironment extends AnAction {
  public void actionPerformed(AnActionEvent e) {
    Project project = e.getProject();
    Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
    EddyFileListener eddylistener = EddyFileListener.activeHintInstance();
    if (eddylistener == null)
      return;

    eddylistener.dumpEnvironment(System.getProperty("user.home") + "/dump.jenv");
  }
}
