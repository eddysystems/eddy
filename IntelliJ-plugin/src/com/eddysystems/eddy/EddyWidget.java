package com.eddysystems.eddy;

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.wm.StatusBar;
import com.intellij.openapi.wm.StatusBarWidget;
import com.intellij.util.Consumer;
import com.intellij.util.PathUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.event.MouseEvent;
import java.io.File;

import static com.eddysystems.eddy.Utility.log;

public class EddyWidget implements StatusBarWidget {

  static private boolean icons_initialized = false;
  private Icon eddyIcon;
  private Icon eddyIconGray;

  class EddyWidgetPresentation implements IconPresentation {

    private boolean _busy = false;

    public boolean busy() {
      return _busy;
    }

    public void setBusy(boolean b) {
      _busy = b;
    }

    @Nullable
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

    @Nullable
    public Consumer<MouseEvent> getClickConsumer() { return null; }

    @NotNull
    @Override
    public Icon getIcon() {
      if (busy())
        return eddyIcon;
      else
        return eddyIconGray;
    }
  }

  private final @NotNull EddyPlugin plugin;
  private StatusBar statusBar = null;
  int users = 0;
  EddyWidgetPresentation presentation = new EddyWidgetPresentation();

  public EddyWidget(final @NotNull EddyPlugin plugin) {
    this.plugin = plugin;

    if (!icons_initialized) {
      String path = PathUtil.getJarPathForClass(this.getClass());
      log("looking for resources in: " + path);

      eddyIcon = new ImageIcon(new File(path, "eddy-icon-16.png").getPath());
      eddyIconGray = new ImageIcon(new File(path, "eddy-icon-16-gray.png").getPath());
      icons_initialized = true;
    }
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
