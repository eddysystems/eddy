package com.eddysystems.eddy;

import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.StartupManager;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

public class EddyPlugin implements ProjectComponent {
  private Project project;
  private Logger logger = Logger.getInstance(getClass());
  private EddyInjector injector;

  public EddyPlugin(Project project) {
    logger.setLevel(Level.DEBUG);
    this.project = project;
    injector = new EddyInjector(project);

    // TODO: talk to server to send usage info
  }

  public void initComponent() {
    // register our injector
    project.getMessageBus().connect().subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, injector);
    // initialize the global environment
    StartupManager.getInstance(project).runWhenProjectIsInitialized(new Runnable() { @Override public void run() {
        DumbService.getInstance(project).runWhenSmart(new Runnable() { @Override public void run() {
          EnvironmentProcessor.initGlobalEnvironment(project);
        }});
      }});

    logger.debug("Eddy initialized");
  }

  public void disposeComponent() {
    // called before the apocalypse
  }

  @NotNull
  public String getComponentName() {
    return "EddyPlugin";
  }

  public void projectOpened() {
    // called when project is opened
  }

  public void projectClosed() {
    // called when project is being closed
  }
}
