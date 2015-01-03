package com.eddysystems.eddy;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.StartupManager;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiTreeChangeListener;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

import static com.eddysystems.eddy.EnvironmentProcessor.JavaEnvironment;

public class EddyPlugin implements ProjectComponent {
  private Project project;
  private Logger logger = Logger.getInstance(getClass());
  private EddyInjector injector;
  private EddyWidget widget = new EddyWidget(this);

  private static Map<Project,EddyPlugin> projectMap = new HashMap<Project, EddyPlugin>();
  public static EddyPlugin getInstance(Project project) {
    return projectMap.get(project);
  }

  private PsiTreeChangeListener psiListener = null;
  private JavaEnvironment env = null;
  public JavaEnvironment getEnv() { return env; }
  public boolean isInitialized() { return env != null; }

  public void initEnv() {

    System.out.println("init env");

    if (psiListener != null) {
      PsiManager.getInstance(project).removePsiTreeChangeListener(psiListener);
    }

    if (ApplicationManager.getApplication().isHeadlessEnvironment()) {
      // free env before allocating the new one
      env = null;
      env = EnvironmentProcessor.getEnvironment(project);
      psiListener = new EddyPsiListener(env);
      PsiManager.getInstance(project).addPsiTreeChangeListener(psiListener);
    } else {
      final StatusBar sbar = WindowManager.getInstance().getStatusBar(project);
      if (sbar != null) {
        sbar.setInfo("eddy is scanning libraries...");
        widget.moreBusy();
      }

      env = EnvironmentProcessor.getEnvironment(project);
      psiListener = new EddyPsiListener(env);
      PsiManager.getInstance(project).addPsiTreeChangeListener(psiListener);

      if (sbar != null) {
        sbar.setInfo("eddy scan done.");
        widget.lessBusy();
      }
    }
  }

  public EddyPlugin(Project project) {

    projectMap.put(project, this);

    logger.setLevel(Level.DEBUG);
    logger.info("available memory: total " + Runtime.getRuntime().totalMemory() + ", max " + Runtime.getRuntime().maxMemory() + ", free " + Runtime.getRuntime().freeMemory());

    this.project = project;
    injector = new EddyInjector(project);

    // TODO: talk to server to send usage info
  }

  public EddyWidget getWidget() {
    return widget;
  }

  public void initComponent() {

    // register our injector
    project.getMessageBus().connect().subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, injector);

    // initialize the global environment
    if (ApplicationManager.getApplication().isHeadlessEnvironment()) {
      /*
      StartupManager.getInstance(project).runWhenProjectIsInitialized(new Runnable() {
        @Override
        public void run() {
          initEnv();
        }
      });
      */
    } else {
      // TODO: maybe run with a ProgressManager function to show a dialog while this is going on
      StartupManager.getInstance(project).runWhenProjectIsInitialized(new Runnable() {
        @Override
        public void run() {
          ApplicationManager.getApplication().executeOnPooledThread(new Runnable() {
            @Override
            public void run() {
              DumbService.getInstance(project).repeatUntilPassesInSmartMode(new Runnable() {
                @Override
                public void run() {
                  ApplicationManager.getApplication().runReadAction(new Runnable() {
                    @Override
                    public void run() {
                      ApplicationManager.getApplication().invokeLater(new Runnable() {
                        @Override public void run() {
                          final StatusBar sbar = WindowManager.getInstance().getStatusBar(project);
                          if (sbar != null) sbar.addWidget(widget);
                        }
                      });
                      initEnv();
                    }
                  });
                }
              });
            }
          });
        }
      });
    }
  }

  public void disposeComponent() {
    System.out.println("disposing plugin.");

    assert ApplicationManager.getApplication().isDispatchThread();

    projectMap.remove(project);
    StatusBar sbar = WindowManager.getInstance().getStatusBar(project);
    if (sbar != null)
      sbar.removeWidget(widget.ID());

    if (psiListener != null) {
      PsiManager.getInstance(project).removePsiTreeChangeListener(psiListener);
    }
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
