package com.eddysystems.eddy.engine;

import com.eddysystems.eddy.LightDocument;
import com.intellij.formatting.*;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageFormatting;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.java.JavaLanguage;
import com.intellij.lang.java.parser.JavaParser;
import com.intellij.lang.java.parser.JavaParserUtil;
import com.intellij.openapi.editor.ex.DocumentEx;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.fileEditor.impl.FileDocumentManagerImpl;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.pom.java.LanguageLevel;
import com.intellij.psi.*;
import com.intellij.psi.codeStyle.*;
import com.intellij.psi.formatter.FormattingDocumentModelImpl;
import com.intellij.psi.formatter.java.AbstractJavaBlock;
import com.intellij.psi.impl.source.DummyHolder;
import com.intellij.psi.impl.source.JavaDummyElement;
import com.intellij.psi.impl.source.SourceTreeToPsiMap;
import com.intellij.psi.impl.source.codeStyle.PreFormatProcessor;
import com.intellij.psi.impl.source.codeStyle.PsiBasedFormatterModelWithShiftIndentInside;
import com.intellij.psi.impl.source.tree.TreeElement;
import com.intellij.psi.util.PsiUtil;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

import static com.eddysystems.eddy.engine.Utility.logError;

/**
 * A hack around the bugs in the atrocious formatting mess inside intellij
 */
public class Formatter {

  @NotNull final Project project;
  @NotNull final PsiElement place;

  @NotNull final JavaParserUtil.ParserWrapper CODE_BLOCK = new JavaParserUtil.ParserWrapper() {
    @Override
    public void parse(final PsiBuilder builder) {
      JavaParser.INSTANCE.getStatementParser().parseCodeBlockDeep(builder, true);
    }
  };

  @NotNull final CodeStyleManager csm;
  @NotNull final PsiElementFactory ef;
  @NotNull final PsiManager pm;
  @NotNull final CodeStyleSettings css;
  @NotNull final FormatterTagHandler tagHandler;

  @NotNull final LanguageLevel context_level;

  static private class VersionIncompatibilityCircumventer {
    static @NotNull CommonCodeStyleSettings.IndentOptions getIndentOptions(@NotNull final CodeStyleSettings css, @NotNull final PsiFile file, @NotNull final TextRange range) {
      try {
        // does css have a getIndentOptionsByFile?
        Method getter = css.getClass().getMethod("getIndentOptionsByFile",PsiFile.class,TextRange.class);
        return (CommonCodeStyleSettings.IndentOptions)getter.invoke(file, range);
      } catch (NoSuchMethodException e) {
        // old version of intellij, fall back to by file type below
      } catch (SecurityException e) {
        // wtf? fall back to old version handling, hope that works.
      } catch (IllegalAccessException e) {
        // really shouldn't happen, we just got that method using a call that respects access control
      } catch (IllegalArgumentException e) {
        // really shouldn't happen, we just got that method using a call that respects access control
      } catch (InvocationTargetException e) {
        // the method threw an exception, it really shouldn't.
      }

      // no? then use getIndentOptions(filetype)
      return css.getIndentOptions(file.getFileType());
    }
  }

  Formatter(@NotNull Project project, @NotNull final PsiElement place) {
    this.project = project;
    this.place = place;

    csm = CodeStyleManager.getInstance(project);
    ef = JavaPsiFacade.getElementFactory(project);
    pm = PsiManager.getInstance(project);
    css = CodeStyleSettingsManager.getSettings(project);
    tagHandler = new FormatterTagHandler(css);

    context_level = place.isValid() ? PsiUtil.getLanguageLevel(place) : LanguageLevel.HIGHEST;
  }

  private @NotNull TextRange preprocess(@NotNull final ASTNode node, @NotNull TextRange range) {
    // TODO: deleted a bunch of injection stuff which we don't respect right now, see CodeFormatterFacade.preprocess

    TextRange result = TextRange.create(range.getStartOffset(), range.getEndOffset());;

    if (!css.FORMATTER_TAGS_ENABLED) {
      for(PreFormatProcessor processor: Extensions.getExtensions(PreFormatProcessor.EP_NAME)) {
        result = processor.process(node, result);
      }
    }
    else {
      List<TextRange> enabledRanges = tagHandler.getEnabledRanges(node, result);
      int delta = 0;
      for (TextRange enabledRange : enabledRanges) {
        enabledRange = enabledRange.shiftRight(delta);
        for (PreFormatProcessor processor : Extensions.getExtensions(PreFormatProcessor.EP_NAME)) {
          TextRange processedRange = processor.process(node, enabledRange);
          delta += processedRange.getLength() - enabledRange.getLength();
        }
      }
      result = result.grown(delta);
    }

    return result;
  }

  private @NotNull PsiCodeBlock reformatCodeBlock(final @NotNull String blockText) {
    // make a fake document
    @NotNull final DocumentEx doc = new LightDocument(blockText);
    doc.setReadOnly(false);

    // inlining createCodeBlockFromText(blockText,place)
    final TreeElement treeElement = new JavaDummyElement(blockText, CODE_BLOCK, context_level, true);
    final DummyHolder holder = new DummyHolder(pm,treeElement,place);

    final PsiElement psiElement = SourceTreeToPsiMap.treeElementToPsi(holder.getTreeElement().getFirstChildNode());
    if (!(psiElement instanceof PsiCodeBlock)) {
      throw new IncorrectOperationException("Incorrect code block '" + blockText + '\'');
    }

    // make sure the associated document does not require locks by making the document ourselves
    assert psiElement.getContainingFile() == holder;
    doc.setModificationStamp(holder.getModificationStamp());
    FileDocumentManagerImpl.registerDocument(doc, holder.getViewProvider().getVirtualFile());

    // inline CodeStyleManager.reformat(block,true)
    assert psiElement.getLanguage() instanceof JavaLanguage;

    // inlining final ASTNode node = new CodeFormatterFacade(css, JavaLanguage.INSTANCE).processElement(treeElement);
    final int startOffset = treeElement.getStartOffset();
    final int endOffset = startOffset + treeElement.getTextLength();

    final FormattingModelBuilder builder = LanguageFormatting.INSTANCE.forContext(holder);
    assert builder != null;

    TextRange range = TextRange.create(startOffset, endOffset);
    range = preprocess(treeElement, range);

    // inlining final FormattingModel model = builder.createModel(elementToFormat, css);
    CommonCodeStyleSettings commonSettings = css.getCommonSettings(JavaLanguage.INSTANCE);
    JavaCodeStyleSettings customJavaSettings = css.getCustomSettings(JavaCodeStyleSettings.class);
    Block block = AbstractJavaBlock.createJavaBlock(treeElement, commonSettings, customJavaSettings);
    FormattingDocumentModelImpl fmodel = new FormattingDocumentModelImpl(doc, holder);
    final FormattingModel model = new PsiBasedFormatterModelWithShiftIndentInside(holder, block, fmodel);
    final CommonCodeStyleSettings.IndentOptions indentOptions = VersionIncompatibilityCircumventer.getIndentOptions(css,holder,range);
    if (holder.getTextLength() > 0) {
      try {
        FormatterEx.getInstanceEx().format(
          model, css, indentOptions, new FormatTextRanges(range, true)
        );

        // line wrapping only does anything if file is backed by real editor, so we don't do it here
        //wrapLongLinesIfNecessary(file, document, startOffset, endOffset);
      }
      catch (IncorrectOperationException e) {
        logError("reformat()", e);
      }
    }

    // we're assuming here that reformatting cannot change the top level element, which in the case of a code block, is pretty reasonable.
    return (PsiCodeBlock)psiElement;
  }

  public String reformat(final @NotNull String show) {
    final String blockText = '{' + show + "\n}";

    final PsiCodeBlock block = reformatCodeBlock(blockText);

    // strip whitespace at the beginning and end of the block
    PsiElement elem = block.getFirstBodyElement();
    // skip whitespace at the beginning of the block
    if (elem instanceof PsiWhiteSpace)
      elem = elem.getNextSibling();
    String result = "";
    while (elem != null && elem != block.getRBrace()) {
      if (elem instanceof PsiWhiteSpace && elem.getNextSibling() == block.getRBrace()) // don't believe IntelliJ, this is important!
        break;
      result += elem.getText();
      elem = elem.getNextSibling();
    }
    return result;
  }

}
