import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileEditor.*;
import com.intellij.openapi.vfs.VirtualFile;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;

/**
 * Created by martin on 15.10.14.
 */
public class EddyInjector implements FileEditorManagerListener {
  private final Logger logger = Logger.getInstance(getClass());
  private final HashMap<FileEditor, Eddy> injected = new HashMap<FileEditor, Eddy>();

  public EddyInjector() {
    logger.setLevel(Level.DEBUG);
  }

  @Override
  public void fileOpened(@NotNull FileEditorManager fileEditorManager, @NotNull VirtualFile virtualFile) {
    FileEditor[] editors = fileEditorManager.getAllEditors();
    for (FileEditor editor : editors) {
      inject(editor);
    }
  }

  private void inject(FileEditor editor) {
    if (this.injected.containsKey(editor)) {
      logger.debug("not injecting into already injected editor");
      return;
    }

    this.injected.put(editor, null);

    if(!(editor instanceof TextEditor)) {
      logger.debug("not injecting into non-text editor");
      return;
    }

    // make a new eddy which will attach to the editor
    this.injected.put(editor, new Eddy((TextEditor)editor));
  }

  @Override
  public void fileClosed(@NotNull FileEditorManager fileEditorManager, @NotNull VirtualFile virtualFile) {
    FileEditor[] editors = fileEditorManager.getAllEditors(virtualFile);
    for (FileEditor editor : editors) {
      uninject(editor);
    }
  }

  private void uninject(FileEditor editor) {
    Eddy eddy = injected.get(editor);
    if (eddy != null) {
      eddy.dispose();
    }
  }

  @Override
  public void selectionChanged(@NotNull FileEditorManagerEvent fileEditorManagerEvent) {
    logger.debug("selection changed");

    // TODO: disable popup on old editor (if active), enable popup on new editor (if needed)
  }
}
