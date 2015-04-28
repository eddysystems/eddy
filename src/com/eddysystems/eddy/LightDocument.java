/* LightDocument: Oh, the humanity
 *
 * In IntelliJ 14 and below, there are a number of spurious assertions in DocumentImpl, in particular,
 * it is assumed that batch processing mode is only ever invoked from the dispatch thread. Because
 * IntelliJ's code formatting code uses the batch processing mode if there are enough changes to make,
 * Formatting can only be performed from the dispatch thread. Because this is unacceptable, this is a
 * minimal implementation of the Document interface, which we force the Formatter to use by polluting
 * the document cache.
 */

package com.eddysystems.eddy;

import com.intellij.openapi.Disposable;
import com.intellij.openapi.application.Application;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.RangeMarker;
import com.intellij.openapi.editor.ReadOnlyModificationException;
import com.intellij.openapi.editor.actionSystem.DocCommandGroupId;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.editor.ex.*;
import com.intellij.openapi.editor.ex.util.SegmentArrayWithData;
import com.intellij.openapi.editor.impl.event.DocumentEventImpl;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.util.*;
import com.intellij.openapi.util.text.LineTokenizer;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.reference.SoftReference;
import com.intellij.util.LocalTimeCounter;
import com.intellij.util.Processor;
import com.intellij.util.containers.ContainerUtil;
import com.intellij.util.text.CharArrayUtil;
import com.intellij.util.text.ImmutableText;
import com.intellij.util.text.MergingCharSequence;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.eddysystems.eddy.engine.Utility.log;

public class LightDocument extends UserDataHolderBase implements DocumentEx {

  static class MyLineIterator implements LineIterator {
    private int myLineIndex = 0;
    private final MyLineSet myLineSet;

    MyLineIterator(MyLineSet lineSet) {
      myLineSet = lineSet;
    }

    @Override
    public void start(int startOffset) {
      myLineIndex = myLineSet.findLineIndex(startOffset);
    }

    @Override
    public int getStart() {
      return myLineSet.getLineStart(myLineIndex);
    }

    @Override
    public int getEnd() {
      return myLineSet.getLineEnd(myLineIndex);
    }

    @Override
    public int getSeparatorLength() {
      return myLineSet.getSeparatorLength(myLineIndex);
    }

    @Override
    public int getLineNumber() {
      return myLineIndex;
    }

    @Override
    public void advance() {
      myLineIndex++;
    }

    @Override
    public boolean atEnd() {
      return myLineIndex >= myLineSet.getLineCount() || myLineIndex < 0;
    }

  }

  static class MyLineSet {
    private SegmentArrayWithData mySegments = new SegmentArrayWithData();
    private static final int MODIFIED_MASK = 0x4;
    private static final int SEPARATOR_MASK = 0x3;

    public int findLineIndex(int offset) {
      int lineIndex = mySegments.findSegmentIndex(offset);
      assert lineIndex >= 0;
      return lineIndex;
    }

    public LineIterator createIterator() {
      return new MyLineIterator(this);
    }

    public final int getLineStart(int index) {
      int lineStart = mySegments.getSegmentStart(index);
      assert lineStart >= 0;
      return lineStart;
    }

    public final int getLineEnd(int index) {
      return mySegments.getSegmentEnd(index);
    }

    final boolean isModified(int index) {
      return (mySegments.getSegmentData(index) & MODIFIED_MASK) != 0;
    }

    final void setModified(int index) {
      setSegmentModified(mySegments, index);
    }

    final int getSeparatorLength(int index) {
      return mySegments.getSegmentData(index) & SEPARATOR_MASK;
    }

    final int getLineCount() {
      return mySegments.getSegmentCount();
    }

    public void documentCreated(@NotNull Document document) {
      initSegments(document.getCharsSequence(), false);
    }

    public void changedUpdate(DocumentEvent e1) {
      DocumentEventImpl e = (DocumentEventImpl) e1;
      if (e.isOnlyOneLineChanged() && mySegments.getSegmentCount() > 0) {
        processOneLineChange(e);
      } else {
        if (mySegments.getSegmentCount() == 0 || e.getStartOldIndex() >= mySegments.getSegmentCount() ||
          e.getStartOldIndex() < 0) {
          initSegments(e.getDocument().getCharsSequence(), true);
          return;
        }

        final int optimizedLineShift = e.getOptimizedLineShift();

        if (optimizedLineShift != -1) {
          processOptimizedMultilineInsert(e, optimizedLineShift);
        } else {
          final int optimizedOldLineShift = e.getOptimizedOldLineShift();

          if (optimizedOldLineShift != -1) {
            processOptimizedMultilineDelete(e, optimizedOldLineShift);
          } else {
            processMultilineChange(e);
          }
        }
      }

      if (e.isWholeTextReplaced()) {
        clearModificationFlags();
      }
    }

    public static void setTestingMode(boolean testMode) {
      assert ApplicationManager.getApplication().isUnitTestMode();
      doTest = testMode;
    }

    private static boolean doTest = false;

    private void processOptimizedMultilineDelete(final DocumentEventImpl e, final int optimizedLineShift) {
      final int insertionPoint = e.getOffset();
      final int changedLineIndex = e.getStartOldIndex();
      final int lengthDiff = e.getOldLength();

      SegmentArrayWithData workingCopySegmentsForTesting = null;
      SegmentArrayWithData segments; //

      if (doTest) {
        segments = new SegmentArrayWithData();
        workingCopySegmentsForTesting = new SegmentArrayWithData();
        fillSegments(segments, workingCopySegmentsForTesting);
      } else {
        segments = mySegments;
      }

      final int oldSegmentStart = segments.getSegmentStart(changedLineIndex);
      final int lastChangedEnd = segments.getSegmentEnd(changedLineIndex + optimizedLineShift);
      final short lastChangedData = segments.getSegmentData(changedLineIndex + optimizedLineShift);
      final int newSegmentEnd = oldSegmentStart + (insertionPoint - oldSegmentStart) + (lastChangedEnd - insertionPoint - lengthDiff);

      segments.remove(changedLineIndex, changedLineIndex + optimizedLineShift);

      if (newSegmentEnd != 0) {
        segments.setElementAt(
          changedLineIndex,
          oldSegmentStart, newSegmentEnd,
          lastChangedData | MODIFIED_MASK
        );
      } else {
        segments.remove(changedLineIndex, changedLineIndex + 1);
      }

      // update data after lineIndex, shifting with optimizedLineShift
      final int segmentCount = segments.getSegmentCount();
      for(int i = changedLineIndex + 1; i < segmentCount; ++i) {
        segments.setElementAt(i, segments.getSegmentStart(i) - lengthDiff,
          segments.getSegmentEnd(i) - lengthDiff,
          segments.getSegmentData(i)
        );
      }

      if (doTest) {
        final SegmentArrayWithData data = mySegments;
        mySegments = segments;
        addEmptyLineAtEnd();

        doCheckResults(workingCopySegmentsForTesting, e, data, segments);
      } else {
        addEmptyLineAtEnd();
      }
    }

    private void processOptimizedMultilineInsert(final DocumentEventImpl e, final int optimizedLineShift) {
      final int insertionPoint = e.getOffset();
      final int changedLineIndex = e.getStartOldIndex();
      final int lengthDiff = e.getNewLength();
      final LineTokenizer tokenizer = new LineTokenizer(e.getNewFragment());

      SegmentArrayWithData workingCopySegmentsForTesting = null;
      SegmentArrayWithData segments; //

      if (doTest) {
        segments = new SegmentArrayWithData();
        workingCopySegmentsForTesting = new SegmentArrayWithData();
        fillSegments(segments, workingCopySegmentsForTesting);
      } else {
        segments = mySegments;
      }

      int i;

      // update data after lineIndex, shifting with optimizedLineShift
      for(i = segments.getSegmentCount() - 1; i > changedLineIndex; --i) {
        segments.setElementAt(i + optimizedLineShift, segments.getSegmentStart(i) + lengthDiff,
          segments.getSegmentEnd(i) + lengthDiff,
          segments.getSegmentData(i)
        );
      }

      final int oldSegmentEnd = segments.getSegmentEnd(changedLineIndex);
      final int oldSegmentStart = segments.getSegmentStart(changedLineIndex);
      final short oldSegmentData = segments.getSegmentData(changedLineIndex);

      final int newChangedLineEnd = insertionPoint + tokenizer.getLineSeparatorLength() + tokenizer.getOffset() + tokenizer.getLength();
      segments.setElementAt(
        changedLineIndex,
        oldSegmentStart, newChangedLineEnd,
        tokenizer.getLineSeparatorLength() | MODIFIED_MASK
      );

      tokenizer.advance();
      i = 1;
      int lastFragmentLength = 0;

      while(!tokenizer.atEnd()) {
        lastFragmentLength = tokenizer.getLineSeparatorLength() != 0 ? 0:tokenizer.getLength();
        segments.setElementAt(
          changedLineIndex + i,
          insertionPoint + tokenizer.getOffset(),
          insertionPoint + tokenizer.getOffset() + tokenizer.getLength() + tokenizer.getLineSeparatorLength(),
          tokenizer.getLineSeparatorLength() | MODIFIED_MASK
        );
        i++;
        tokenizer.advance();
      }

      segments.setElementAt(
        changedLineIndex + optimizedLineShift, insertionPoint + lengthDiff - lastFragmentLength,
        oldSegmentEnd + lengthDiff,
        oldSegmentData | MODIFIED_MASK
      );

      if (doTest) {
        final SegmentArrayWithData data = mySegments;
        mySegments = segments;
        addEmptyLineAtEnd();

        doCheckResults(workingCopySegmentsForTesting, e, data, segments);
      } else {
        addEmptyLineAtEnd();
      }
    }

    private void doCheckResults(final SegmentArrayWithData workingCopySegmentsForTesting, final DocumentEventImpl e,
                                final SegmentArrayWithData data,
                                final SegmentArrayWithData segments) {
      mySegments = workingCopySegmentsForTesting;
      processMultilineChange(e);
      mySegments = data;

      assert workingCopySegmentsForTesting.getSegmentCount() == segments.getSegmentCount();
      for(int i =0; i < segments.getSegmentCount();++i) {
        assert workingCopySegmentsForTesting.getSegmentStart(i) == segments.getSegmentStart(i);
        assert workingCopySegmentsForTesting.getSegmentEnd(i) == segments.getSegmentEnd(i);
        assert workingCopySegmentsForTesting.getSegmentData(i) == segments.getSegmentData(i);
      }

      processMultilineChange(e);
    }

    private void fillSegments(final SegmentArrayWithData segments, final SegmentArrayWithData workingCopySegmentsForTesting) {
      for(int i = mySegments.getSegmentCount() - 1; i >=0; --i) {
        segments.setElementAt(
          i,
          mySegments.getSegmentStart(i),
          mySegments.getSegmentEnd(i),
          mySegments.getSegmentData(i)
        );
        workingCopySegmentsForTesting.setElementAt(
          i,
          mySegments.getSegmentStart(i),
          mySegments.getSegmentEnd(i),
          mySegments.getSegmentData(i)
        );
      }
    }

    private void processMultilineChange(DocumentEventImpl e) {
      int offset = e.getOffset();
      CharSequence newString = e.getNewFragment();
      CharSequence chars = e.getDocument().getCharsSequence();

      int oldStartLine = e.getStartOldIndex();
      int offset1 = getLineStart(oldStartLine);
      if (offset1 != offset) {
        CharSequence prefix = chars.subSequence(offset1, offset);
        newString = new MergingCharSequence(prefix, newString);
      }

      int oldEndLine = findLineIndex(e.getOffset() + e.getOldLength());
      if (oldEndLine < 0) {
        oldEndLine = getLineCount() - 1;
      }
      int offset2 = getLineEnd(oldEndLine);
      if (offset2 != offset + e.getOldLength()) {
        final int start = offset + e.getNewLength();
        final int length = offset2 - offset - e.getOldLength();
        CharSequence postfix = chars.subSequence(start, start + length);
        newString = new MergingCharSequence(newString, postfix);
      }

      updateSegments(newString, oldStartLine, oldEndLine, offset1, e);
      // We add empty line at the end, if the last line ends by line separator.
      addEmptyLineAtEnd();
    }

    private void updateSegments(CharSequence newText, int oldStartLine, int oldEndLine, int offset1,
                                DocumentEventImpl e) {
      int count = 0;
      LineTokenizer lineTokenizer = new LineTokenizer(newText);
      for (int index = oldStartLine; index <= oldEndLine; index++) {
        if (!lineTokenizer.atEnd()) {
          setSegmentAt(mySegments, index, lineTokenizer, offset1, true);
          lineTokenizer.advance();
        } else {
          mySegments.remove(index, oldEndLine + 1);
          break;
        }
        count++;
      }
      if (!lineTokenizer.atEnd()) {
        SegmentArrayWithData insertSegments = new SegmentArrayWithData();
        int i = 0;
        while (!lineTokenizer.atEnd()) {
          setSegmentAt(insertSegments, i, lineTokenizer, offset1, true);
          lineTokenizer.advance();
          count++;
          i++;
        }
        mySegments.insert(insertSegments, oldEndLine + 1);
      }
      int shift = e.getNewLength() - e.getOldLength();
      mySegments.shiftSegments(oldStartLine + count, shift);
    }

    private void processOneLineChange(DocumentEventImpl e) {
      // Check, if the change on the end of text
      if (e.getOffset() >= mySegments.getSegmentEnd(mySegments.getSegmentCount() - 1)) {
        mySegments.changeSegmentLength(mySegments.getSegmentCount() - 1, e.getNewLength() - e.getOldLength());
        setSegmentModified(mySegments, mySegments.getSegmentCount() - 1);
      } else {
        mySegments.changeSegmentLength(e.getStartOldIndex(), e.getNewLength() - e.getOldLength());
        setSegmentModified(mySegments, e.getStartOldIndex());
      }
    }

    public void clearModificationFlags() {
      for (int i = 0; i < mySegments.getSegmentCount(); i++) {
        mySegments.setSegmentData(i, mySegments.getSegmentData(i) & ~MODIFIED_MASK);
      }
    }

    private static void setSegmentAt(SegmentArrayWithData segmentArrayWithData, int index, LineTokenizer lineTokenizer, int offsetShift, boolean isModified) {
      int offset = lineTokenizer.getOffset() + offsetShift;
      int length = lineTokenizer.getLength();
      int separatorLength = lineTokenizer.getLineSeparatorLength();
      int separatorAndModifiedFlag = separatorLength;
      if(isModified) {
        separatorAndModifiedFlag |= MODIFIED_MASK;
      }
      segmentArrayWithData.setElementAt(index, offset, offset + length + separatorLength, separatorAndModifiedFlag);
    }

    private static void setSegmentModified(SegmentArrayWithData segments, int i) {
      segments.setSegmentData(i, segments.getSegmentData(i)|MODIFIED_MASK);
    }

    private void initSegments(CharSequence text, boolean toSetModified) {
      mySegments.removeAll();
      LineTokenizer lineTokenizer = new LineTokenizer(text);
      int i = 0;
      while(!lineTokenizer.atEnd()) {
        setSegmentAt(mySegments, i, lineTokenizer, 0, toSetModified);
        i++;
        lineTokenizer.advance();
      }
      // We add empty line at the end, if the last line ends by line separator.
      addEmptyLineAtEnd();
    }

    // Add empty line at the end, if the last line ends by line separator.
    private void addEmptyLineAtEnd() {
      int segmentCount = mySegments.getSegmentCount();
      if(segmentCount > 0 && getSeparatorLength(segmentCount-1) > 0) {
        mySegments.setElementAt(segmentCount, mySegments.getSegmentEnd(segmentCount-1),  mySegments.getSegmentEnd(segmentCount-1), 0);
        setSegmentModified(mySegments, segmentCount);
      }
    }
  }

  private final Ref<DocumentListener[]> myCachedDocumentListeners = Ref.create(null);
  private final List<DocumentListener> myDocumentListeners = ContainerUtil.createLockFreeCopyOnWriteList();

  private final Object myLineSetLock = new String("line set lock");
  private volatile MyLineSet myLineSet;
  private volatile ImmutableText myText;
  private volatile SoftReference<String> myTextString;

  private boolean myIsReadOnly = false;
  private volatile boolean isStripTrailingSpacesEnabled = true;
  private volatile long myModificationStamp;
  private final PropertyChangeSupport myPropertyChangeSupport = new PropertyChangeSupport(this);

  private final List<EditReadOnlyListener> myReadOnlyListeners = ContainerUtil.createLockFreeCopyOnWriteList();

  private boolean myEventsHandling = false;
  private volatile boolean myDoingBulkUpdate = false;
  private boolean myChangeInProgress;
  private volatile int myBufferSize;
  private final CharSequence myMutableCharSequence = new CharSequence() {
    @Override
    public int length() {
      return myText.length();
    }

    @Override
    public char charAt(int index) {
      return myText.charAt(index);
    }

    @Override
    public CharSequence subSequence(int start, int end) {
      return myText.subSequence(start, end);
    }

    @NotNull
    @Override
    public String toString() {
      return doGetText();
    }
  };

  public LightDocument(@NotNull String text) {
    assertValidSeparators(text);
    setCyclicBufferSize(0);
    myModificationStamp = LocalTimeCounter.currentTime();
    myText = ImmutableText.valueOf(text);
  }

  private MyLineSet getLineSet() {
    MyLineSet lineSet = myLineSet;
    if (lineSet == null) {
      synchronized (myLineSetLock) {
        lineSet = myLineSet;
        if (lineSet == null) {
          lineSet = new MyLineSet();
          lineSet.documentCreated(this);
          myLineSet = lineSet;
        }
      }
    }
    return lineSet;
  }

  private void assertValidSeparators(@NotNull CharSequence s) {
    StringUtil.assertValidSeparators(s);
  }

  @Deprecated
  @Override
  @NotNull
  public char[] getChars() {
    return CharArrayUtil.fromSequence(myText);
  }

  @Override
  public void setStripTrailingSpacesEnabled(boolean isEnabled) {
    isStripTrailingSpacesEnabled = isEnabled;
  }

  @Override
  public void setReadOnly(boolean isReadOnly) {
    if (myIsReadOnly != isReadOnly) {
      myIsReadOnly = isReadOnly;
      myPropertyChangeSupport.firePropertyChange(Document.PROP_WRITABLE, !isReadOnly, isReadOnly);
    }
  }

  @Override
  public boolean isWritable() {
    return !myIsReadOnly;
  }

  @Override
  public boolean removeRangeMarker(@NotNull RangeMarkerEx rangeMarker) {
    return false;
  }

  @Override
  public void registerRangeMarker(@NotNull RangeMarkerEx rangeMarker,
                                  int start,
                                  int end,
                                  boolean greedyToLeft,
                                  boolean greedyToRight,
                                  int layer) {
    throw new NotImplementedException();
  }

  @Override
  @NotNull
  public RangeMarker createGuardedBlock(int startOffset, int endOffset) {
    throw new NotImplementedException();
  }

  @Override
  public void removeGuardedBlock(@NotNull RangeMarker block) {
  }

  @Override
  @NotNull
  public List<RangeMarker> getGuardedBlocks() {
    return Collections.emptyList();
  }

  @Override
  public RangeMarker getOffsetGuard(int offset) {
    return null;
  }

  @Override
  public RangeMarker getRangeGuard(int start, int end) {
    return null;
  }

  @Override
  public void startGuardedBlockChecking() {}

  @Override
  public void stopGuardedBlockChecking() {}

  @Override
  @NotNull
  public RangeMarker createRangeMarker(int startOffset, int endOffset) {
    throw new NotImplementedException();
  }

  @Override
  @NotNull
  public RangeMarker createRangeMarker(int startOffset, int endOffset, boolean surviveOnExternalChange) {
    throw new NotImplementedException();
  }

  @Override
  public long getModificationStamp() {
    return myModificationStamp;
  }

  @Override
  public void setModificationStamp(long modificationStamp) {
    myModificationStamp = modificationStamp;
  }

  @Override
  public void replaceText(@NotNull CharSequence chars, long newModificationStamp) {
    replaceString(0, getTextLength(), chars, newModificationStamp, true);
    clearLineModificationFlags();
  }

  @Override
  public int getListenersCount() {
    return myDocumentListeners.size();
  }

  @Override
  public void insertString(int offset, @NotNull CharSequence s) {
    if (offset < 0) throw new IndexOutOfBoundsException("Wrong offset: " + offset);
    if (offset > getTextLength()) {
      throw new IndexOutOfBoundsException(
        "Wrong offset: " + offset + "; documentLength: " + getTextLength() + "; " + s.subSequence(Math.max(0, s.length() - 20), s.length())
      );
    }
    assertValidSeparators(s);

    if (!isWritable()) throw new ReadOnlyModificationException(this);
    if (s.length() == 0) return;

    updateText(myText.insert(offset, ImmutableText.valueOf(s)), offset, null, s, false, LocalTimeCounter.currentTime());
    trimToSize();
  }

  private void trimToSize() {
    if (myBufferSize != 0 && getTextLength() > myBufferSize) {
      deleteString(0, getTextLength() - myBufferSize);
    }
  }

  @Override
  public void deleteString(int startOffset, int endOffset) {
    assertBounds(startOffset, endOffset);

    if (!isWritable()) throw new ReadOnlyModificationException(this);
    if (startOffset == endOffset) return;

    CharSequence sToDelete = myText.subSequence(startOffset, endOffset);

    updateText(myText.delete(startOffset, endOffset), startOffset, sToDelete, null, false, LocalTimeCounter.currentTime());
  }

  @Override
  public void moveText(int srcStart, int srcEnd, int dstOffset) {
    assertBounds(srcStart, srcEnd);
    if (dstOffset == srcEnd) return;
    ProperTextRange srcRange = new ProperTextRange(srcStart, srcEnd);
    assert !srcRange.containsOffset(dstOffset) : "Can't perform text move from range [" +srcStart+ "; " + srcEnd+ ") to offset "+dstOffset;

    String replacement = getCharsSequence().subSequence(srcStart, srcEnd).toString();

    insertString(dstOffset, replacement);
    int shift = 0;
    if (dstOffset < srcStart) {
      shift = srcEnd - srcStart;
    }
    fireMoveText(srcStart + shift, srcEnd + shift, dstOffset);

    deleteString(srcStart + shift, srcEnd + shift);
  }

  private void fireMoveText(int start, int end, int newBase) {
    for (DocumentListener listener : getCachedListeners()) {
      if (listener instanceof PrioritizedInternalDocumentListener) {
        ((PrioritizedInternalDocumentListener)listener).moveTextHappened(start, end, newBase);
      }
    }
  }

  @Override
  public void replaceString(int startOffset, int endOffset, @NotNull CharSequence s) {
    replaceString(startOffset, endOffset, s, LocalTimeCounter.currentTime(), startOffset == 0 && endOffset == getTextLength());
  }

  private void replaceString(int startOffset, int endOffset, @NotNull CharSequence s, final long newModificationStamp, boolean wholeTextReplaced) {
    assertBounds(startOffset, endOffset);
    assertValidSeparators(s);

    if (!isWritable()) {
      throw new ReadOnlyModificationException(this);
    }

    final int newStringLength = s.length();
    final CharSequence chars = getCharsSequence();
    int newStartInString = 0;
    int newEndInString = newStringLength;
    while (newStartInString < newStringLength &&
           startOffset < endOffset &&
           s.charAt(newStartInString) == chars.charAt(startOffset)) {
      startOffset++;
      newStartInString++;
    }

    while (endOffset > startOffset &&
           newEndInString > newStartInString &&
           s.charAt(newEndInString - 1) == chars.charAt(endOffset - 1)) {
      newEndInString--;
      endOffset--;
    }

    CharSequence changedPart = s.subSequence(newStartInString, newEndInString);
    CharSequence sToDelete = myText.subSequence(startOffset, endOffset);

    ImmutableText newText;
    if (wholeTextReplaced && s instanceof ImmutableText) {
      newText = (ImmutableText)s;
    }
    else {
      newText = myText.delete(startOffset, endOffset).insert(startOffset, changedPart);
    }
    updateText(newText, startOffset, sToDelete, changedPart, wholeTextReplaced, newModificationStamp);
    trimToSize();
  }

  private void assertBounds(final int startOffset, final int endOffset) {
    if (startOffset < 0 || startOffset > getTextLength()) {
      throw new IndexOutOfBoundsException("Wrong startOffset: " + startOffset + "; documentLength: " + getTextLength());
    }
    if (endOffset < 0 || endOffset > getTextLength()) {
      throw new IndexOutOfBoundsException("Wrong endOffset: " + endOffset + "; documentLength: " + getTextLength());
    }
    if (endOffset < startOffset) {
      throw new IllegalArgumentException(
        "endOffset < startOffset: " + endOffset + " < " + startOffset + "; documentLength: " + getTextLength());
    }
  }

  /**
   * All document change actions follows the algorithm below:
   * <pre>
   * <ol>
   *   <li>
   *     All {@link #addDocumentListener(com.intellij.openapi.editor.event.DocumentListener) registered listeners} are notified
   *     {@link com.intellij.openapi.editor.event.DocumentListener#beforeDocumentChange(com.intellij.openapi.editor.event.DocumentEvent) before the change};
   *   </li>
   *   <li>The change is performed </li>
   *   <li>
   *     All {@link #addDocumentListener(com.intellij.openapi.editor.event.DocumentListener) registered listeners} are notified
   *     {@link com.intellij.openapi.editor.event.DocumentListener#documentChanged(com.intellij.openapi.editor.event.DocumentEvent) after the change};
   *   </li>
   * </ol>
   * </pre>
   * <p/>
   * There is a possible case that <code>'before change'</code> notification produces new change. We have a problem then - imagine
   * that initial change was <code>'replace particular range at document end'</code> and <code>'nested change'</code> was to
   * <code>'remove text at document end'</code>. That means that when initial change will be actually performed, the document may be
   * not long enough to contain target range.
   * <p/>
   * Current method allows to check if document change is a <code>'nested call'</code>.
   *
   * @throws IllegalStateException if this method is called during a <code>'nested document modification'</code>
   */
  private void assertNotNestedModification() throws IllegalStateException {
    if (myChangeInProgress) {
      throw new IllegalStateException("Detected nested request for document modification from 'before change' callback!");
    }
  }

  @Override
  public void suppressGuardedExceptions() {}

  @Override
  public void unSuppressGuardedExceptions() {}

  @Override
  public boolean isInEventsHandling() {
    return myEventsHandling;
  }

  @Override
  public void clearLineModificationFlags() {
    getLineSet().clearModificationFlags();
  }

  private void updateText(@NotNull ImmutableText newText,
                          int offset,
                          @Nullable CharSequence oldString,
                          @Nullable CharSequence newString,
                          boolean wholeTextReplaced,
                          long newModificationStamp) {
    assertNotNestedModification();
    myChangeInProgress = true;
    final DocumentEvent event;
    try {
      event = doBeforeChangedUpdate(offset, oldString, newString, wholeTextReplaced);
    }
    finally {
      myChangeInProgress = false;
    }
    myTextString = null;
    myText = newText;
    changedUpdate(event, newModificationStamp);
  }

  @NotNull
  private DocumentEvent doBeforeChangedUpdate(int offset, CharSequence oldString, CharSequence newString, boolean wholeTextReplaced) {
    Application app = ApplicationManager.getApplication();
    if (app != null) {
      FileDocumentManager manager = FileDocumentManager.getInstance();
      if (manager != null) {
        VirtualFile file = manager.getFile(this);
        if (file != null && !file.isValid())
          log("File of this document has been deleted.");
      }
    }

    getLineSet(); // initialize line set to track changed lines

    DocumentEvent event = new DocumentEventImpl(this, offset, oldString, newString, myModificationStamp, wholeTextReplaced);

    if (!ShutDownTracker.isShutdownHookRunning()) {
      DocumentListener[] listeners = getCachedListeners();
      for (int i = listeners.length - 1; i >= 0; i--) {
        try {
          listeners[i].beforeDocumentChange(event);
        }
        catch (Throwable e) {
          log(e);
        }
      }
    }

    myEventsHandling = true;
    return event;
  }

  private void changedUpdate(@NotNull DocumentEvent event, long newModificationStamp) {
    try {
      getLineSet().changedUpdate(event);
      setModificationStamp(newModificationStamp);

      if (!ShutDownTracker.isShutdownHookRunning()) {
        DocumentListener[] listeners = getCachedListeners();
        for (DocumentListener listener : listeners) {
          try {
            listener.documentChanged(event);
          }
          catch (Throwable e) {
            log(e);
          }
        }
      }
    }
    finally {
      myEventsHandling = false;
    }
  }

  @NotNull
  @Override
  public String getText() {
    return ApplicationManager.getApplication().runReadAction(new Computable<String>() {
      @Override
      public String compute() {
        return doGetText();
      }
    });
  }

  @NotNull
  private String doGetText() {
    String s = SoftReference.dereference(myTextString);
    if (s == null) {
      myTextString = new SoftReference<String>(s = myText.toString());
    }
    return s;
  }

  @NotNull
  @Override
  public String getText(@NotNull final TextRange range) {
    return ApplicationManager.getApplication().runReadAction(new Computable<String>() {
      @Override
      public String compute() {
        return myText.subSequence(range.getStartOffset(), range.getEndOffset()).toString();
      }
    });
  }

  @Override
  public int getTextLength() {
    return myText.length();
  }

  @Override
  @NotNull
  public CharSequence getCharsSequence() {
    return myMutableCharSequence;
  }

  @NotNull
  @Override
  public CharSequence getImmutableCharSequence() {
    return myText;
  }


  @Override
  public void addDocumentListener(@NotNull DocumentListener listener) {
    myCachedDocumentListeners.set(null);

    if (myDocumentListeners.contains(listener)) {
      log("Already registered: " + listener);
    }
    boolean added = myDocumentListeners.add(listener);
    assert added : listener;
  }

  @Override
  public void addDocumentListener(@NotNull final DocumentListener listener, @NotNull Disposable parentDisposable) {
    addDocumentListener(listener);
    Disposer.register(parentDisposable, new DocumentListenerDisposable(listener, myCachedDocumentListeners, myDocumentListeners));
  }

  private static class DocumentListenerDisposable implements Disposable {
    private final DocumentListener myListener;
    private final Ref<DocumentListener[]> myCachedDocumentListenersRef;
    private final List<DocumentListener> myDocumentListeners;

    public DocumentListenerDisposable(@NotNull DocumentListener listener,
                                      @NotNull Ref<DocumentListener[]> cachedDocumentListenersRef,
                                      @NotNull List<DocumentListener> documentListeners) {
      myListener = listener;
      myCachedDocumentListenersRef = cachedDocumentListenersRef;
      myDocumentListeners = documentListeners;
    }

    @Override
    public void dispose() {
      doRemoveDocumentListener(myListener, myCachedDocumentListenersRef, myDocumentListeners);
    }
  }

  @Override
  public void removeDocumentListener(@NotNull DocumentListener listener) {
    doRemoveDocumentListener(listener, myCachedDocumentListeners, myDocumentListeners);
  }

  private static void doRemoveDocumentListener(@NotNull DocumentListener listener,
                                               @NotNull Ref<DocumentListener[]> cachedDocumentListenersRef,
                                               @NotNull List<DocumentListener> documentListeners) {
    cachedDocumentListenersRef.set(null);
    boolean success = documentListeners.remove(listener);
    if (!success) {
      log("Can't remove document listener (" + listener + "). Registered listeners: " + documentListeners);
    }
  }

  @Override
  public int getLineNumber(final int offset) {
    return getLineSet().findLineIndex(offset);
  }

  @Override
  @NotNull
  public LineIterator createLineIterator() {
    return getLineSet().createIterator();
  }

  @Override
  public final int getLineStartOffset(final int line) {
    if (line == 0) return 0; // otherwise it crashed for zero-length document
    return getLineSet().getLineStart(line);
  }

  @Override
  public final int getLineEndOffset(int line) {
    if (getTextLength() == 0 && line == 0) return 0;
    int result = getLineSet().getLineEnd(line) - getLineSeparatorLength(line);
    assert result >= 0;
    return result;
  }

  @Override
  public final int getLineSeparatorLength(int line) {
    int separatorLength = getLineSet().getSeparatorLength(line);
    assert separatorLength >= 0;
    return separatorLength;
  }

  @Override
  public final int getLineCount() {
    int lineCount = getLineSet().getLineCount();
    assert lineCount >= 0;
    return lineCount;
  }

  @NotNull
  private DocumentListener[] getCachedListeners() {
    DocumentListener[] cachedListeners = myCachedDocumentListeners.get();
    if (cachedListeners == null) {
      DocumentListener[] listeners = myDocumentListeners.toArray(new DocumentListener[myDocumentListeners.size()]);
      Arrays.sort(listeners, PrioritizedDocumentListener.COMPARATOR);
      cachedListeners = listeners;
      myCachedDocumentListeners.set(cachedListeners);
    }

    return cachedListeners;
  }

  @Override
  public void fireReadOnlyModificationAttempt() {
    for (EditReadOnlyListener listener : myReadOnlyListeners) {
      listener.readOnlyModificationAttempt(this);
    }
  }

  @Override
  public void addEditReadOnlyListener(@NotNull EditReadOnlyListener listener) {
    myReadOnlyListeners.add(listener);
  }

  @Override
  public void removeEditReadOnlyListener(@NotNull EditReadOnlyListener listener) {
    myReadOnlyListeners.remove(listener);
  }


  @Override
  public void addPropertyChangeListener(@NotNull PropertyChangeListener listener) {
    myPropertyChangeSupport.addPropertyChangeListener(listener);
  }

  @Override
  public void removePropertyChangeListener(@NotNull PropertyChangeListener listener) {
    myPropertyChangeSupport.removePropertyChangeListener(listener);
  }

  @Override
  public void setCyclicBufferSize(int bufferSize) {
    assert bufferSize >= 0 : bufferSize;
    myBufferSize = bufferSize;
  }

  @Override
  public void setText(@NotNull final CharSequence text) {
    Runnable runnable = new Runnable() {
      @Override
      public void run() {
        replaceString(0, getTextLength(), text, LocalTimeCounter.currentTime(), true);
      }
    };
    if (CommandProcessor.getInstance().isUndoTransparentActionInProgress()) {
      runnable.run();
    }
    else {
      CommandProcessor.getInstance().executeCommand(null, runnable, "", DocCommandGroupId.noneGroupId(this));
    }

    clearLineModificationFlags();
  }

  @Override
  @NotNull
  public RangeMarker createRangeMarker(@NotNull final TextRange textRange) {
    return createRangeMarker(textRange.getStartOffset(), textRange.getEndOffset());
  }

  @Override
  public final boolean isInBulkUpdate() {
    return myDoingBulkUpdate;
  }

  @Override
  public final void setInBulkUpdate(boolean value) {
    myDoingBulkUpdate = value;
  }

  @Override
  public boolean processRangeMarkers(@NotNull Processor<RangeMarker> processor) {
    throw new NotImplementedException();
  }

  @Override
  public boolean processRangeMarkersOverlappingWith(int start, int end, @NotNull Processor<RangeMarker> processor) {
    throw new NotImplementedException();
  }

  @Override
  public String toString() {
    return "DocumentImpl[" + FileDocumentManager.getInstance().getFile(this) + "]";
  }

}
