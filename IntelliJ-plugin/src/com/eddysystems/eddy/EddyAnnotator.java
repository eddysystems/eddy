package com.eddysystems.eddy;

import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

public class EddyAnnotator implements Annotator {
  @Override
  public void annotate(@NotNull PsiElement element, @NotNull AnnotationHolder holder) {
    Project project = element.getProject();
    PsiFile psifile = element.getContainingFile();
    FileEditor[] editors = FileEditorManager.getInstance(project).getAllEditors();

    // annotate anything which in any of the editors is under the cursor
    for (FileEditor ed: editors) {
      if (!(ed instanceof TextEditor))
        continue;

      Editor editor = ((TextEditor)ed).getEditor();
      Document document = editor.getDocument();

      // make sure we're editing the right file
      if (psifile != PsiDocumentManager.getInstance(project).getPsiFile(document))
        continue;

      // only annotate the current line, and bail if there's more than one cursor
      if (editor.getCaretModel().getCaretCount() != 1)
        return;
      int pos = editor.getCaretModel().getCurrentCaret().getOffset();

      // since we're called for each element, discard all elements that are not the one under the cursor
      PsiElement lineelem = psifile.getNode().getPsi().findElementAt(pos);
      if (element != lineelem)
        return;

      // this runs once. Make an annotation (which is invisible), which will then be used to actually run eddy every time
      // the cursor moves.

      // this code should only execute once per pass
      int line = document.getLineNumber(pos);
      int lineStart = document.getLineStartOffset(line);
      int lineEnd = document.getLineEndOffset(line);
      TextRange lineRange = new TextRange(lineStart, lineEnd);

      Annotation ann = holder.createErrorAnnotation(lineRange, null /* we don't want a message here */);
      ann.registerFix(new EddyIntention());
      // make invisible, except for maybe gutter icon. The annotation is irrelevant -- we only care about the fix that's attached
      ann.setHighlightType(ProblemHighlightType.INFORMATION);
      //ann.setGutterIconRenderer(...);
      ann.setNeedsUpdateOnTyping(true);
    }
  }
}
