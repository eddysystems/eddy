import com.intellij.codeInsight.hint.HintManager;
import com.intellij.codeInsight.hint.HintManagerImpl;
import com.intellij.codeInsight.hint.HintUtil;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.event.CaretEvent;
import com.intellij.openapi.editor.event.CaretListener;
import com.intellij.openapi.editor.event.DocumentEvent;
import com.intellij.openapi.editor.event.DocumentListener;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.TextEditor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.tree.LeafElement;
import com.intellij.psi.impl.source.tree.RecursiveTreeElementVisitor;
import com.intellij.psi.impl.source.tree.TreeElement;
import com.intellij.psi.scope.BaseScopeProcessor;
import com.intellij.psi.scope.ElementClassHint;
import com.intellij.psi.scope.JavaScopeProcessorEvent;
import com.intellij.psi.scope.util.PsiScopesUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.ui.LightweightHint;
import com.intellij.util.SmartList;
import org.apache.log4j.Level;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.HashSet;
import java.util.List;

/**
 * Created by martin on 15.10.14.
 */
public class Eddy implements CaretListener, DocumentListener {
  private final @NotNull Project project;
  private final @NotNull Editor editor;
  private final @NotNull Document document;
  private final @NotNull PsiFile psifile;
  private final @NotNull Logger logger = Logger.getInstance(getClass());

  public Eddy(Project project, TextEditor editor, @NotNull PsiFile psifile) {
    logger.setLevel(Level.DEBUG);

    this.project = project;
    this.editor = editor.getEditor();
    this.document = this.editor.getDocument();
    this.psifile = psifile;

    VirtualFile file = FileDocumentManager.getInstance().getFile(this.document);
    if (file != null)
      logger.debug("making eddy for editor for file " + file.getPresentableName());
    else
      logger.debug("making eddy for editor for file 'null'");

    // moving the caret around
    this.editor.getCaretModel().addCaretListener(this);

    // subscribe to document events
    this.editor.getDocument().addDocumentListener(this);
  }

  public void dispose() {
    editor.getCaretModel().removeCaretListener(this);
    editor.getDocument().removeDocumentListener(this);
  }

  protected boolean enabled() {
    // only with a single caret can we deal...
    return editor.getCaretModel().getCaretCount() == 1;
  }

  /**
   * Shows a hint under the cursor with what eddy understood
   * @param line what eddy understood the current line meant
   */
  protected void showHint(String line) {
    JComponent hintcomponent = HintUtil.createInformationLabel("eddy says: " + line);
    LightweightHint hint = new LightweightHint(hintcomponent);
    HintManagerImpl hm = ((HintManagerImpl) HintManager.getInstance());
    hm.showEditorHint(hint, editor, HintManager.UNDER, HintManager.HIDE_BY_ANY_KEY | HintManager.HIDE_BY_TEXT_CHANGE | HintManager.HIDE_BY_SCROLLING, 0, false);
  }

  /**
   * An action performing one of the edits that eddy suggested
   */
  /*
  protected class EddyAcceptAction extends AnAction {
    EddyAcceptAction(String line) {
      super(line);
    };
    @Override
    public void actionPerformed(AnActionEvent actionEvent) {
      logger.info("eddy thingy accepted, event: " + actionEvent.toString());
    }
  }
*/

  class EnvironmentProcessor extends BaseScopeProcessor implements ElementClassHint {

    // things that are in scope (not all these are accessible! things may be private, or not static while we are)
    private final List<PsiClass> classes = new SmartList<PsiClass>();
    private final List<PsiVariable> variables = new SmartList<PsiVariable>();
    private final List<PsiMethod> methods = new SmartList<PsiMethod>();
    private final List<PsiPackage> packages = new SmartList<PsiPackage>();
    private final List<PsiEnumConstant> enums = new SmartList<PsiEnumConstant>();
    private final List<PsiNameIdentifierOwner> elements = new SmartList<PsiNameIdentifierOwner>();

    // the expanded symbol dictionary
    private List<EnvironmentInfo> environment = null;

    private final PsiElement place;

    // used during walking
    private boolean inStaticScope = false;
    private PsiElement currentFileContext;
    private boolean honorPrivate;

    EnvironmentProcessor(PsiElement place, boolean honorPrivate) {
      this.place = place;
      this.honorPrivate = honorPrivate;
      PsiScopesUtil.treeWalkUp(this, place, null);
    }

    /**
     * Expand the environment by enumerating all things that require qualification by something that's in scope. For
     * instance, If a class A is in scope, but place is not somewhere in A or a subclass of A, then a field x of A is
     * not in the environment. Calling expand fills a list of names, each associated with a list of ways to get to
     * qualify that name such that it is in scope. For instance, calling expand will result in an entry x: [A] to denote
     * that writing A.x would be legal.
     */
    class EnvironmentInfo {
      public PsiElement element;

      // a list of all paths to get to this element (there may be more than one)
      // each path starts with a symbol that's in scope. Concatenating the path with '.' and appending the element should
      // yield a name that correctly qualifies the element from the current place.
      // For shadowed elements, the path is just large enough to overcome the shadowing
      public List<PsiElement[]> paths = new SmartList<PsiElement[]>();

      EnvironmentInfo(PsiElement element) {
        this.element = element;
      }

      float weight(String query) {
        // TODO: take into account how well the name fits the query and how deep the paths are + other heuristics
        return 0;
      }
    }

    public void expand() {
      // all elements in the environment are added with a zero length path
      environment = new SmartList<EnvironmentInfo>();

      HashSet<String> shadowed = new HashSet<String>();

      for (final PsiNameIdentifierOwner elem : getElements()) {
        // add all non-shadowed items into the environment
        PsiElement ident = elem.getNameIdentifier();

        // for example, an anonymous class -- we can't do much with that, ignore it
        if (ident == null)
          continue;

        if (shadowed.contains(ident.getText())) {
          // check whether we can unshadow this by using either this. or super. or some part of the fully qualified name
          // TODO
        }

        // TODO
      }

      List<PsiElement> queue = new SmartList<PsiElement>();
      queue.addAll(this.getElements());

      while (!queue.isEmpty()) {
        int idx = queue.size()-1;
        PsiElement element = queue.get(idx);
        queue.remove(idx);

        // find all sub-names of this name
        // TODO...
      }

      // fill environment
      // TODO
    }

    /**
     * Check if a thing (member or class) is private or protected and therefore not accessible
     * @param element The thing to check whether it's inaccessible because it may be private or protected
     * @param containingClass The class containing the thing
     * @return whether the element is private or protected and not accessible because of it
     */
    private boolean isInaccessible(PsiModifierListOwner element, PsiClass containingClass) {
      if (element.hasModifierProperty(PsiModifier.PRIVATE)) {
        // if the member is private we can only see it if place is contained in a class in which member is declared.
        PsiClass containingPlaceClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
        while (containingPlaceClass != null) {
          if (containingClass == containingPlaceClass) {
            break;
          }
          containingPlaceClass = PsiTreeUtil.getParentOfType(containingPlaceClass, PsiClass.class);
        }
        if (containingPlaceClass == null) {
          return true;
        }
      }

      if (element.hasModifierProperty(PsiModifier.PROTECTED)) {
        // if the member is protected we can only see it if place is contained in a subclass of the containingClass
        PsiClass containingPlaceClass = PsiTreeUtil.getParentOfType(place, PsiClass.class, false);
        while (containingPlaceClass != null) {
          if (containingPlaceClass == containingClass)
            break;
          containingPlaceClass = containingPlaceClass.getSuperClass();
        }
        if (containingPlaceClass == null) {
          return true;
        }
      }

      return false;
    }

    /**
     * Check if a variable is visible in our place (not private member in a superclass)
     */
    public boolean isVisible(PsiVariable var) {
      if (var instanceof PsiMember) {
        PsiMember member = (PsiMember)var;
        return !isInaccessible(member, member.getContainingClass());
      } else
        return true; // parameters and local variables are always visible
    }

    /**
     * Check if a member (field or method) is visible in our place (not private in a superclass)
     */
    public boolean isVisible(PsiMember member) {
      return !isInaccessible(member, member.getContainingClass());
    }

    /**
     * Check if a class is visible in our place (not private)
     */
    public boolean isVisible(PsiClass clazz) {
      return !isInaccessible(clazz, clazz.getContainingClass());
    }

    public List<PsiClass> getClasses() {
      return classes;
    }

    public List<PsiVariable> getVariables() {
      return variables;
    }

    public List<PsiEnumConstant> getEnums() {
      return enums;
    }

    public List<PsiMethod> getMethods() {
      return methods;
    }

    public List<PsiPackage> getPackages() {
      return packages;
    }

    public List<PsiNameIdentifierOwner> getElements() {
      return elements;
    }

    @Override
    public boolean shouldProcess(DeclarationKind kind) {
      return kind == DeclarationKind.CLASS ||
             kind == DeclarationKind.FIELD ||
             kind == DeclarationKind.METHOD ||
             kind == DeclarationKind.VARIABLE ||
             kind == DeclarationKind.PACKAGE ||
             kind == DeclarationKind.ENUM_CONST;
    }

    @Override
    public boolean execute(@NotNull PsiElement element, ResolveState state) {

      // if we are in static scope, a class member has to be declared static for us to see it
      if (element instanceof PsiField || element instanceof PsiMethod) {
        if (inStaticScope && !((PsiMember)element).hasModifierProperty(PsiModifier.STATIC))
          return true;
      }

      if (honorPrivate) {
        PsiClass containing = null;
        if (element instanceof PsiMember)
          containing = ((PsiMember)element).getContainingClass();

        if (containing != null && isInaccessible((PsiModifierListOwner)element, containing)) {
          return true;
        }
      }

      if (element instanceof PsiClass) {
        elements.add((PsiNameIdentifierOwner) element);
        classes.add((PsiClass)element);
      } else if (element instanceof PsiEnumConstant) {
        elements.add((PsiNameIdentifierOwner) element);
        enums.add((PsiEnumConstant)element);
      } else if (element instanceof PsiVariable) {
        elements.add((PsiNameIdentifierOwner) element);
        variables.add((PsiVariable)element);
      } else if (element instanceof PsiMethod) {
        elements.add((PsiNameIdentifierOwner) element);
        methods.add((PsiMethod)element);
      } else if (element instanceof PsiPackage) {
        elements.add((PsiNameIdentifierOwner) element);
        packages.add((PsiPackage)element);
      }
      return true;
    }

    @Override
    public final void handleEvent(@NotNull Event event, Object associated){
      if (event == JavaScopeProcessorEvent.START_STATIC)
        inStaticScope = true;
      else if (event == JavaScopeProcessorEvent.SET_CURRENT_FILE_CONTEXT)
        currentFileContext = (PsiElement)associated;
    }
  }

  protected Token psiToTok(TreeElement elem) {
    IElementType type = elem.getElementType();
    if (type == JavaTokenType.IDENTIFIER)
        return new Ident(elem.getText());

    if (type == JavaTokenType.C_STYLE_COMMENT)
      return new EOLComment(((PsiComment)elem).getText());
    if (type == JavaTokenType.END_OF_LINE_COMMENT)
      return new CComment(((PsiComment)elem).getText());

    if (type == JavaTokenType.INTEGER_LITERAL)
      return new IntLitTok((Integer) ((PsiLiteral) elem).getValue());
    if (type == JavaTokenType.LONG_LITERAL)
      return new LongLitTok((Long) ((PsiLiteral) elem).getValue());
    if (type == JavaTokenType.FLOAT_LITERAL)
      return new FloatLitTok((Float) ((PsiLiteral) elem).getValue());
    if (type == JavaTokenType.DOUBLE_LITERAL)
      return new DoubleLitTok((Double) ((PsiLiteral) elem).getValue());
    if (type == JavaTokenType.CHARACTER_LITERAL)
      return new CharLitTok((Character) ((PsiLiteral) elem).getValue());
    if (type == JavaTokenType.STRING_LITERAL)
      return new StringLitTok((String) ((PsiLiteral) elem).getValue());
    if (type == JavaTokenType.TRUE_KEYWORD)
      return new BooleanLitTok(true);
    if (type == JavaTokenType.FALSE_KEYWORD)
      return new BooleanLitTok(false);
    if (type == JavaTokenType.NULL_KEYWORD)
      return new NullLitTok();

    if (type == JavaTokenType.ABSTRACT_KEYWORD)
      return new AbstractTok();
    if (type == JavaTokenType.ASSERT_KEYWORD)
      return new AssertTok();
    if (type == JavaTokenType.BOOLEAN_KEYWORD)
      return new BooleanTok();
    if (type == JavaTokenType.BREAK_KEYWORD)
      return new BreakTok();
    if (type == JavaTokenType.BYTE_KEYWORD)
      return new ByteTok();
    if (type == JavaTokenType.CASE_KEYWORD)
      return new CaseTok();
    if (type == JavaTokenType.CATCH_KEYWORD)
      return new CatchTok();
    if (type == JavaTokenType.CHAR_KEYWORD)
      return new CharTok();
    if (type == JavaTokenType.CLASS_KEYWORD)
      return new ClassTok();
    if (type == JavaTokenType.CONST_KEYWORD)
      return new ConstTok();
    if (type == JavaTokenType.CONTINUE_KEYWORD)
      return new ContinueTok();
    if (type == JavaTokenType.DEFAULT_KEYWORD)
      return new DefaultTok();
    if (type == JavaTokenType.DO_KEYWORD)
      return new DoTok();
    if (type == JavaTokenType.DOUBLE_KEYWORD)
      return new DoubleTok();
    if (type == JavaTokenType.ELSE_KEYWORD)
      return new ElseTok();
    if (type == JavaTokenType.ENUM_KEYWORD)
      return new EnumTok();
    if (type == JavaTokenType.EXTENDS_KEYWORD)
      return new ExtendsTok();
    if (type == JavaTokenType.FINAL_KEYWORD)
      return new FinalTok();
    if (type == JavaTokenType.FINALLY_KEYWORD)
      return new FinallyTok();
    if (type == JavaTokenType.FLOAT_KEYWORD)
      return new FloatTok();
    if (type == JavaTokenType.FOR_KEYWORD)
      return new ForTok();
    if (type == JavaTokenType.GOTO_KEYWORD)
      return new GotoTok();
    if (type == JavaTokenType.IF_KEYWORD)
      return new IfTok();
    if (type == JavaTokenType.IMPLEMENTS_KEYWORD)
      return new ImplementsTok();
    if (type == JavaTokenType.IMPORT_KEYWORD)
      return new ImportTok();
    if (type == JavaTokenType.INSTANCEOF_KEYWORD)
      return new InstanceofTok();
    if (type == JavaTokenType.INT_KEYWORD)
      return new IntTok();
    if (type == JavaTokenType.INTERFACE_KEYWORD)
      return new InterfaceTok();
    if (type == JavaTokenType.LONG_KEYWORD)
      return new LongTok();
    if (type == JavaTokenType.NATIVE_KEYWORD)
      return new NativeTok();
    if (type == JavaTokenType.NEW_KEYWORD)
      return new NewTok();
    if (type == JavaTokenType.PACKAGE_KEYWORD)
      return new PackageTok();
    if (type == JavaTokenType.PRIVATE_KEYWORD)
      return new PrivateTok();
    if (type == JavaTokenType.PUBLIC_KEYWORD)
      return new PublicTok();
    if (type == JavaTokenType.SHORT_KEYWORD)
      return new ShortTok();
    if (type == JavaTokenType.SUPER_KEYWORD)
      return new SuperTok();
    if (type == JavaTokenType.SWITCH_KEYWORD)
      return new SwitchTok();
    if (type == JavaTokenType.SYNCHRONIZED_KEYWORD)
      return new SynchronizedTok();
    if (type == JavaTokenType.THIS_KEYWORD)
      return new ThisTok();
    if (type == JavaTokenType.THROW_KEYWORD)
      return new ThrowTok();
    if (type == JavaTokenType.PROTECTED_KEYWORD)
      return new ProtectedTok();
    if (type == JavaTokenType.TRANSIENT_KEYWORD)
      return new TransientTok();
    if (type == JavaTokenType.RETURN_KEYWORD)
      return new ReturnTok();
    if (type == JavaTokenType.VOID_KEYWORD)
      return new VoidTok();
    if (type == JavaTokenType.STATIC_KEYWORD)
      return new StaticTok();
    if (type == JavaTokenType.STRICTFP_KEYWORD)
      return new StrictfpTok();
    if (type == JavaTokenType.WHILE_KEYWORD)
      return new WhileTok();
    if (type == JavaTokenType.TRY_KEYWORD)
      return new TryTok();
    if (type == JavaTokenType.VOLATILE_KEYWORD)
      return new VolatileTok();
    if (type == JavaTokenType.THROWS_KEYWORD)
      return new ThrowsTok();

    if (type == JavaTokenType.LPARENTH)
      return new LParenTok();
    if (type == JavaTokenType.RPARENTH)
      return new RParenTok();
    if (type == JavaTokenType.LBRACE)
      return new LCurlyTok();
    if (type == JavaTokenType.RBRACE)
      return new RCurlyTok();
    if (type == JavaTokenType.LBRACKET)
      return new LBrackTok();
    if (type == JavaTokenType.RBRACKET)
      return new RBrackTok();
    if (type == JavaTokenType.SEMICOLON)
      return new SemiTok();
    if (type == JavaTokenType.COMMA)
      return new CommaTok();
    if (type == JavaTokenType.DOT)
      return new DotTok();
    if (type == JavaTokenType.ELLIPSIS)
      return new EllipsisTok();
    if (type == JavaTokenType.AT)
      return new AtTok();

    if (type == JavaTokenType.EQ)
      return new EqTok();
    if (type == JavaTokenType.GT)
      return new GtTok();
    if (type == JavaTokenType.LT)
      return new LtTok();
    if (type == JavaTokenType.EXCL)
      return new NotTok();
    if (type == JavaTokenType.TILDE)
      return new CompTok();
    if (type == JavaTokenType.QUEST)
      return new QuestionTok();
    if (type == JavaTokenType.COLON)
      return new ColonTok();
    if (type == JavaTokenType.PLUS)
      return new PlusTok();
    if (type == JavaTokenType.MINUS)
      return new MinusTok();
    if (type == JavaTokenType.ASTERISK)
      return new MulTok();
    if (type == JavaTokenType.DIV)
      return new DivTok();
    if (type == JavaTokenType.AND)
      return new AndTok();
    if (type == JavaTokenType.OR)
      return new OrTok();
    if (type == JavaTokenType.XOR)
      return new XorTok();
    if (type == JavaTokenType.PERC)
      return new ModTok();

    if (type == JavaTokenType.EQEQ)
      return new EqEqTok();
    if (type == JavaTokenType.LE)
      return new LeTok();
    if (type == JavaTokenType.GE)
      return new GeTok();
    if (type == JavaTokenType.NE)
      return new NeTok();
    if (type == JavaTokenType.ANDAND)
      return new AndAndTok();
    if (type == JavaTokenType.OROR)
      return new OrOrTok();
    if (type == JavaTokenType.PLUSPLUS)
      return new PlusPlusTok();
    if (type == JavaTokenType.MINUSMINUS)
      return new MinusMinusTok();
    if (type == JavaTokenType.LTLT)
      return new LShiftTok();
    if (type == JavaTokenType.GTGT)
      return new RShiftTok();
    if (type == JavaTokenType.GTGTGT)
      return new UnsignedRShiftTok();
    if (type == JavaTokenType.PLUSEQ)
      return new PlusEqTok();
    if (type == JavaTokenType.MINUSEQ)
      return new MinusEqTok();
    if (type == JavaTokenType.ASTERISKEQ)
      return new MulEqTok();
    if (type == JavaTokenType.DIVEQ)
      return new DivEqTok();
    if (type == JavaTokenType.ANDEQ)
      return new AndEqTok();
    if (type == JavaTokenType.OREQ)
      return new OrEqTok();
    if (type == JavaTokenType.XOREQ)
      return new XorEqTok();
    if (type == JavaTokenType.PERCEQ)
      return new ModEqTok();
    if (type == JavaTokenType.LTLTEQ)
      return new LShiftEqTok();
    if (type == JavaTokenType.GTGTEQ)
      return new RShiftEqTok();
    if (type == JavaTokenType.GTGTGTEQ)
      return new UnsignedRShiftEqTok();

    if (type == JavaTokenType.DOUBLE_COLON)
      return new ColonColonTok();
    if (type == JavaTokenType.ARROW)
      return new ArrowTok();

    if (type == TokenType.WHITE_SPACE)
      return new WhiteSpace();

    logger.warn("unknown token type: " + elem.getElementType() + " in element " + elem);
    return null;
  }

  protected void processLineAt(int lnum, int column) {
    int offset = document.getLineStartOffset(lnum) + column;
    final TextRange lrange = TextRange.create(document.getLineStartOffset(lnum), document.getLineEndOffset(lnum));
    String line = document.getText(lrange);

    logger.debug("processing at " + lnum + "/" + column);
    logger.debug("  current line: " + line);

    // whitespace is counted toward the next token/statement, so start at the beginning of the line
    PsiElement elem = psifile.findElementAt(document.getLineStartOffset(lnum));

    // if we hit whitespace, advance until we find something substantial, or leave the line
    if (elem instanceof PsiWhiteSpace) {
      elem = elem.getNextSibling();
      logger.debug("  found whitespace, next token " + elem);
      if (!lrange.intersects(elem.getTextRange())) {
        logger.debug("out of line range");
        elem = null;
      }
    }

    if (elem != null) {
      // parse beginning of the line to the end of the line
      ASTNode node = elem.getNode();

      // walk up the tree until the line is fully contained
      while (node != null && lrange.contains(node.getTextRange())) {
        logger.debug("  PSI node: " + node.getPsi() + ", contained in this line: " + lrange.contains(node.getTextRange()));
        node = node.getTreeParent();
      }

      // then walk the node subtree and output all tokens contained in any statement overlapping with the line
      // now, node is the AST node we want to interpret.
      if (node == null) {
        logger.warn("cannot find a node to look at.");
        return;
      }

      // get token stream for this node
      assert node instanceof TreeElement;

      final List<Token> tokens = new SmartList<Token>();

      ((TreeElement) node).acceptTree(new RecursiveTreeElementVisitor() {
        @Override
        protected boolean visitNode(TreeElement element) {
          // if the element is not overlapping, don't output any of it
          if (!lrange.intersects(element.getTextRange())) {
            return false;
          }

          if (element instanceof LeafElement) {
            logger.debug("    node: " + element);
            tokens.add(psiToTok(element));
          }

          return true;
        }
      });

      AST.parse(tokens);
    }

    /*
    PsiElement elem = psifile.findElementAt(offset);
    if (elem != null) {
      ASTNode node = elem.getNode();
      logger.debug("AST node: " + node.toString() + ", contained in this line: " + lrange.contains(node.getTextRange()));
      node = node.getTreeParent();
      while (node != null) {
        logger.debug("  parent: " + node.toString() + ", contained in this line: " + lrange.contains(node.getTextRange()));
        node = node.getTreeParent();
      }

      EnvironmentProcessor env = new EnvironmentProcessor(elem, true);

      logger.debug("variables in scope: ");
      for (final PsiVariable var : env.getVariables()) {
        if (var instanceof PsiLocalVariable)
          logger.debug("  local: " + var.getName() + ": " + var.getType().getCanonicalText());
        else if (var instanceof PsiParameter)
          logger.debug("  param: " + var.getName() + ": " + var.getType().getCanonicalText());
        else if (var instanceof PsiField) {
          logger.debug("  field: " + ((PsiField)var).getContainingClass().getQualifiedName() + "." + var.getName() + ": " + var.getType().getCanonicalText());
        } else {
          logger.debug("  other: " + var.getName() + ": " + var.getType().getCanonicalText() + " kind " + var.getClass().getSimpleName());
        }
      }
      logger.debug("methods in scope:");
      for (final PsiMethod m : env.getMethods()) {
        if (m.isConstructor())
          logger.debug("  " + m.getTypeParameterList().getText() + " " + m.getModifierList().getText().replace('\n', ' ') + " " + m.getContainingClass().getQualifiedName() + "." + m.getName() + m.getParameterList().getText());
        else
          logger.debug("  " + m.getTypeParameterList().getText() + " " + m.getModifierList().getText().replace('\n', ' ') + " " + m.getReturnType().getCanonicalText() + " " + m.getContainingClass().getQualifiedName() + "." + m.getName() + m.getParameterList().getText());
      }
      logger.debug("enums in scope:");
      for (final PsiEnumConstant en : env.getEnums()) {
        logger.debug("  " + en.getName() + ": " + en.getType());
      }
      logger.debug("classes in scope (excluding java.lang.*): ");
      for (final PsiClass cls : env.getClasses()) {
        if (cls.getQualifiedName() != null && cls.getQualifiedName().startsWith("java.lang"))
          continue;
        logger.debug("  " + cls.getQualifiedName());
      }
      logger.debug("packages in scope (excluding java.lang.*):");
      for (final PsiPackage pkg : env.getPackages()) {
        if (pkg.getQualifiedName().startsWith("java.lang"))
          continue;
        logger.debug("  " + pkg.getQualifiedName());
      }

    } else
      logger.debug("PSI element not available.");
    */

    showHint(line);

    /*
    // this shows a popup, but it's way too intrusive. We need something like the hint, but have the options inside the
    // hint, only accessible by hotkeys (maybe ^arrows, ^enter, ^numbers)
    DataContext context = DataManager.getInstance().getDataContext(editor.getComponent());
    DefaultActionGroup actions = new DefaultActionGroup();
    actions.add(new EddyAcceptAction(line));
    actions.add(new EddyAcceptAction("noooooo!"));
    ListPopup popup = JBPopupFactory.getInstance().createActionGroupPopup(null, actions, context, JBPopupFactory.ActionSelectionAid.NUMBERING, false, null, 4);
    popup.showInBestPositionFor(context);
    */
  }

  @Override
  public void caretPositionChanged(CaretEvent e) {
    if (!enabled())
      return;

    processLineAt(e.getNewPosition().line, e.getNewPosition().column);
  }

  @Override
  public void caretAdded(CaretEvent e) {
  }

  @Override
  public void caretRemoved(CaretEvent e) {
  }

  @Override
  public void beforeDocumentChange(DocumentEvent event) {
    //logger.debug("before document change");
  }

  @Override
  public void documentChanged(DocumentEvent event) {
    //logger.debug("document changed");
  }
}
