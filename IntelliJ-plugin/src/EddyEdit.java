import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;

import static java.lang.System.out;

/**
 * Created by martin on 15.10.14.
 */
public class EddyEdit extends AnAction {
    public void actionPerformed(AnActionEvent e) {
        // Show eddy window linked to current Java window. Create it if it doesn't exist. Read eddy comments if there
        // are any
        out.println("action!");
    }
}
