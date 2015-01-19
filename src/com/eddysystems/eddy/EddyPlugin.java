package com.eddysystems.eddy;

import com.eddysystems.eddy.engine.EddyPsiListener;
import com.eddysystems.eddy.engine.JavaEnvironment;
import com.intellij.ide.util.PropertiesComponent;
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
import com.intellij.util.PathUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static com.eddysystems.eddy.engine.Utility.log;
import static utility.JavaUtils.popScope;
import static utility.JavaUtils.pushScope;

public class EddyPlugin implements ProjectComponent {
  @NotNull final private Application app;
  @NotNull final private EddyApplicationListener listener;
  private Project project;

  // Find our "install" key, or create it if necessary
  static private String _install = null;
  public static String installKey() {
    if (_install == null) {
      final PropertiesComponent props = PropertiesComponent.getInstance();
      final String name = "com.eddysystems.Props.install";
      _install = props.getValue(name);
      if (_install == null) {
        _install = tarski.Crypto.randomKey();
        props.setValue(name,_install);
      }
    }
    return _install;
  }

  static private Properties _properties = null;
  public static Properties getProperties() {
    if (_properties == null) {
      _properties = new Properties();

      try {
        String pathname = PathUtil.getJarPathForClass(EddyPlugin.class);
        File path = new File(pathname);
        if (path.isDirectory()) {
          log("looking for resources in directory: " + pathname);
          _properties.load(new FileInputStream(new File(path, "eddy.properties")));
        } else {
          _properties.load(_properties.getClass().getClassLoader().getResourceAsStream("eddy.properties"));
        }
      } catch (IOException e) {
        log("cannot read version information: " + e);
      }
    }
    return _properties;
  }

  public static String getVersion() {
    return getProperties().getProperty("version");
  }

  public static String getBuild() {
    return getProperties().getProperty("build");
  }

  public Project getProject() { return project; }
  private EddyInjector injector;
  private EddyWidget widget = new EddyWidget(this);

  private static Map<Project,EddyPlugin> projectMap = new HashMap<Project, EddyPlugin>();
  public static EddyPlugin getInstance(Project project) {
    return projectMap.get(project);
  }

  private PsiTreeChangeListener psiListener = null;
  private JavaEnvironment env = null;
  public JavaEnvironment getEnv() { return env; }

  private boolean initializing = false;
  private Object initLock = new Object();
  public boolean isInitialized() { return env != null && env.initialized(); }

  public void dropEnv() {
    env = null;
  }

  public void initEnv(@Nullable ProgressIndicator indicator) {
    pushScope("start init environment");
    try {
      if (indicator != null)
        indicator.setIndeterminate(true);

      if (psiListener != null) {
        PsiManager.getInstance(project).removePsiTreeChangeListener(psiListener);
      }
      env = null;

      if (ApplicationManager.getApplication().isHeadlessEnvironment()) {
        // free env before allocating the new one
        env = new JavaEnvironment(project);
        psiListener = new EddyPsiListener(env);
        PsiManager.getInstance(project).addPsiTreeChangeListener(psiListener);
        env.initialize();
      } else {
        final StatusBar sbar = WindowManager.getInstance().getStatusBar(project);
        String err = "";

        if (sbar != null) {
          widget.moreBusy();
        }

        try {
          // can't have changes between when we make the environment and when we register the psi listener
          app.runReadAction(new Runnable() { @Override public void run() {
            env = new JavaEnvironment(project);
            psiListener = new EddyPsiListener(env);
            PsiManager.getInstance(project).addPsiTreeChangeListener(psiListener);
          }});

          env.initialize();

          log("environment initialized");

        } catch (JavaEnvironment.NoJDKError e) {
          env = null;
          err = e.getMessage();
          log(e.getMessage());
        } finally {
          initializing = false;
          if (sbar != null) {
            widget.lessBusy();
            if (err.isEmpty())
              sbar.setInfo("eddy initialized.");
            else
              sbar.setInfo("eddy library scan aborted, " + err);
          }
        }
      }
    } finally { popScope(); }
  }

  public EddyPlugin(Project project) {
    assert !projectMap.containsKey(project);

    app = ApplicationManager.getApplication();
    this.project = project;

    log("available memory: total " + Runtime.getRuntime().totalMemory() + ", max " + Runtime.getRuntime().maxMemory() + ", free " + Runtime.getRuntime().freeMemory());

    projectMap.put(project, this);
    injector = new EddyInjector(project);
    listener = new EddyApplicationListener();
    app.addApplicationListener(listener);

    // TODO: talk to server to send usage info
  }

  public EddyWidget getWidget() {
    return widget;
  }

  // returns immediately
  public void requestInit() {
    if (isInitialized())
      return;

    synchronized (initLock) {
      if (initializing)
        return;
      initializing = true;
    }

    // schedule initialization if necessary
    final Runnable init = new Runnable() { @Override public void run() {
      ProgressManager.getInstance().run(new Task.Backgroundable(project, "Initializing eddy...", true, new PerformInBackgroundOption() {
        @Override public boolean shouldStartInBackground() { return true; }
        @Override public void processSentToBackground() { }
      }) {
        @Override
        public void run(@NotNull final ProgressIndicator indicator) {
          // either testing or in background
          assert (app.isHeadlessEnvironment() || !app.isDispatchThread());

          DumbService.getInstance(project).repeatUntilPassesInSmartMode(new Runnable() { @Override public void run() {
            initEnv(indicator);
          }});
        }
      });
    }};

    if (app.isDispatchThread()) {
      init.run();
    } else {
      app.invokeLater(init);
    }
  }

  public void initComponent() {
    log("eddy starting: installation " + installKey() + " version " + getVersion() + " build " + getBuild());

    // register our injector
    project.getMessageBus().connect().subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, injector);

    // initialize the global environment
    if (!app.isHeadlessEnvironment()) {
      StartupManager.getInstance(project).runWhenProjectIsInitialized(new Runnable() {
        @Override
        public void run() {
          widget.requestInstall();
          requestInit();
        }
      });
    }
  }

  public void disposeComponent() {
    log("disposing plugin.");
    assert app.isDispatchThread();
    projectMap.remove(project);
    final StatusBar sbar = WindowManager.getInstance().getStatusBar(project);

    if (sbar != null)
      sbar.removeWidget(widget.ID());

    if (psiListener != null)
      PsiManager.getInstance(project).removePsiTreeChangeListener(psiListener);

    app.removeApplicationListener(listener);
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
