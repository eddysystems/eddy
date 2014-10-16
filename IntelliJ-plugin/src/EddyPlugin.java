import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileEditor.FileEditorManagerListener;
import com.intellij.openapi.project.Project;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

/**
 * Created by martin on 15.10.14.
 */
public class EddyPlugin implements ProjectComponent {
  private Project project;
  private Logger logger = Logger.getInstance(getClass());
  private EddyInjector injector;

  public EddyPlugin(Project project) {
    logger.setLevel(Level.DEBUG);
    this.project = project;
    injector = new EddyInjector();
  }

  public void initComponent() {
    project.getMessageBus().connect().subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, injector);
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
