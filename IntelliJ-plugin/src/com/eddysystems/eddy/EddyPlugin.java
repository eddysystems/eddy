package com.eddysystems.eddy;

import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.progress.PerformInBackgroundOption;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.progress.Task;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.startup.StartupManager;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiTreeChangeListener;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;

import static com.eddysystems.eddy.Utility.log;

public class EddyPlugin implements ProjectComponent {
  @NotNull final private Application app;
  private Project project;
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

  public void dropEnv() {
    env = null;
    Runtime.getRuntime().gc();
  }

  public void initEnv(@Nullable ProgressIndicator indicator) {

    if (indicator != null)
      indicator.setIndeterminate(true);

    log("start init environment");

    if (psiListener != null) {
      PsiManager.getInstance(project).removePsiTreeChangeListener(psiListener);
    }
    env = null;

    if (ApplicationManager.getApplication().isHeadlessEnvironment()) {
      // free env before allocating the new one
      env = EnvironmentProcessor.getEnvironment(project);
      psiListener = new EddyPsiListener(env);
      PsiManager.getInstance(project).addPsiTreeChangeListener(psiListener);
    } else {
      final StatusBar sbar = WindowManager.getInstance().getStatusBar(project);
      if (sbar != null) {
        sbar.setInfo("eddy is scanning libraries...");
        widget.moreBusy();
      }

      String err = "";
      try {
        env = EnvironmentProcessor.getEnvironment(project);
        psiListener = new EddyPsiListener(env);
        PsiManager.getInstance(project).addPsiTreeChangeListener(psiListener);
      } catch (EnvironmentProcessor.NoJDKError e) {
        env = null;
        err = e.getMessage();
        log(e.getMessage());
      }

      if (sbar != null) {
        if (err.isEmpty())
          sbar.setInfo("eddy scan done.");
        else
          sbar.setInfo("eddy scan aborted, " + err);
        widget.lessBusy();
      }
    }
  }

  public EddyPlugin(Project project) {
    assert !projectMap.containsKey(project);

    app = ApplicationManager.getApplication();
    this.project = project;

    log("available memory: total " + Runtime.getRuntime().totalMemory() + ", max " + Runtime.getRuntime().maxMemory() + ", free " + Runtime.getRuntime().freeMemory());

    projectMap.put(project, this);
    injector = new EddyInjector(project);

    // TODO: talk to server to send usage info
  }

  public EddyWidget getWidget() {
    return widget;
  }

  private void init_internal(final ProgressIndicator indicator) {
    DumbService.getInstance(project).repeatUntilPassesInSmartMode(new Runnable() {
      @Override
      public void run() {
        app.runReadAction(new Runnable() {
          @Override
          public void run() {
            app.invokeLater(new Runnable() {
              @Override
              public void run() {
                final StatusBar sbar = WindowManager.getInstance().getStatusBar(project);
                if (sbar != null) sbar.addWidget(widget);
              }
            });
            initEnv(indicator);
          }
        });
      }
    });
  }

  public void initComponent() {

    // register our injector
    project.getMessageBus().connect().subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, injector);

    // initialize the global environment
    if (!app.isHeadlessEnvironment()) {
      // TODO: maybe run with a ProgressManager function to show a dialog while this is going on
      StartupManager.getInstance(project).runWhenProjectIsInitialized(new Runnable() {
        @Override
        public void run() {
          ProgressManager.getInstance().run(new Task.Backgroundable(project, "Eddy scan", true, new PerformInBackgroundOption() {
            @Override
            public boolean shouldStartInBackground() {
              return true;
            }

            @Override
            public void processSentToBackground() {
            }
          }) {
            @Override
            public void run(@NotNull ProgressIndicator indicator) {
              init_internal(indicator);
            }
          });

          /*
          app.executeOnPooledThread(new Runnable() {
            @Override
            public void run() {
              init_internal();
            }
          });
          */
        }
      });
    }
  }

  public void disposeComponent() {
    log("disposing plugin.");

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
