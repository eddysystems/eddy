package com.eddysystems.eddy;

import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;

/**
 * Created by martin on 05.11.14.
 */
public class DumpEnvironmentAction extends AnAction {
  public void actionPerformed(AnActionEvent e) {
    Project project = e.getProject();
    Editor editor = FileEditorManager.getInstance(project).getSelectedTextEditor();
    EddyFileListener eddylistener = EddyFileListener.activeInstance();
    if (eddylistener == null)
      return;

    eddylistener.dumpEnvironment(System.getProperty("user.home") + "/dump.jenv");
  }
}
