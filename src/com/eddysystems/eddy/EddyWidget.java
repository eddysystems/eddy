package com.eddysystems.eddy;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.StatusBarWidget;
import com.intellij.openapi.wm.WindowManager;
import com.intellij.util.Consumer;
import com.intellij.util.PathUtil;
import com.intellij.util.ResourceUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.event.MouseEvent;
import java.io.File;
import java.net.URL;

import static com.eddysystems.eddy.engine.Utility.log;

class EddyWidget implements StatusBarWidget {

  static private Icon eddyIcon;
  static private Icon eddyIconGray;

  static public Icon getIcon() {
    if (eddyIcon == null) {
      makeIcons();
    }
    return eddyIcon;
  }
  static public Icon getIconGray() {
    if (eddyIconGray== null) {
      makeIcons();
    }
    return eddyIconGray;
  }

  static private void makeIcons() {
    String pathname = PathUtil.getJarPathForClass(EddyWidget.class);
    File path = new File(pathname);

    if (path.isDirectory()) {
      log("looking for resources in directory: " + pathname);
      eddyIcon = new ImageIcon(new File(path, "eddy-icon-16.png").getPath());
      eddyIconGray = new ImageIcon(new File(path, "eddy-icon-16-gray.png").getPath());
    } else {
      URL colorurl = ResourceUtil.getResource(EddyWidget.class, "", "eddy-icon-16.png");
      URL greyurl = ResourceUtil.getResource(EddyWidget.class, "", "eddy-icon-16-gray.png");
      eddyIcon = new ImageIcon(colorurl);
      eddyIconGray = new ImageIcon(greyurl);
    }
  }

  class EddyPresentation implements WidgetPresentation {
    private boolean _busy = false;
    public boolean busy() {
      return _busy;
    }
    public void setBusy(boolean b) {
      _busy = b;
    }

    @Nullable @Override
    public String getTooltipText() {
      if (busy()) {
        if (plugin.isInitialized())
          return "let me think about this...";
        else
          return "eddy is scanning libraries.";
      } else {
        if (plugin.isInitialized())
          return "eddy ready.";
        else
          return "eddy disabled.";
      }
    }

    @Nullable @Override public Consumer<MouseEvent> getClickConsumer() {
      return null;
    }
  }

  class FancyPresentation extends EddyPresentation implements IconPresentation {

    @NotNull
    @Override
    public Icon getIcon() {
      if (busy())
        return EddyWidget.getIcon();
      else
        return EddyWidget.getIconGray();
    }
  }

  private final EddyPlugin plugin;
  private StatusBar statusBar = null;
  int users = 0;
  EddyPresentation presentation = null;

  private EddyWidget() {
    this.plugin = null;
  }

  public EddyWidget(final @NotNull EddyPlugin plugin) {
    this.plugin = plugin;
    presentation = new FancyPresentation();
  }

  public void requestInstall() {

    // can't install -- problem during construction
    if (presentation == null) {
      log("presentation is null when requesting widget install.");
      return;
    }

    ApplicationManager.getApplication().invokeLater(new Runnable() { @Override public void run() {
      if (!installed()) {
        final StatusBar sbar = WindowManager.getInstance().getStatusBar(plugin.getProject());
        if (sbar != null)
          sbar.addWidget(EddyWidget.this);
      }
    }}, plugin.getProject().getDisposed());
  }

  public boolean installed() {
    return statusBar != null;
  }

  public void update() {
    if (!ApplicationManager.getApplication().isDispatchThread()) {
      ApplicationManager.getApplication().invokeLater(new Runnable() {
        @Override public void run() {
          if (statusBar != null) // we may not yet be installed
            statusBar.updateWidget(ID());
        }
      });
    } else if (statusBar != null) // we may not yet be installed
        statusBar.updateWidget(ID());
  }

  public void moreBusy() {
    users++;
    if (users == 1) {
      presentation.setBusy(true);
      update();
    }
  }

  public void lessBusy() {
    users--;
    if (users == 0) {
      presentation.setBusy(false);
      update();
    }
  }


  @NotNull
  @Override
  public String ID() {
    return "EddyWidget-" + plugin.hashCode();
  }

  @NotNull
  @Override
  public WidgetPresentation getPresentation(@NotNull PlatformType type) {
    return presentation;
  }

  @Override
  public void install(@NotNull StatusBar statusBar) {
    this.statusBar = statusBar;
  }

  @Override
  public void dispose() {}
}
