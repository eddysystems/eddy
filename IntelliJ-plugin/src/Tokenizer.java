import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.JavaTokenType;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiLiteral;
import com.intellij.psi.TokenType;
import com.intellij.psi.impl.source.tree.TreeElement;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

import tarski.Tokens.*;

/**
 * Created by martin on 23.10.14.
 */
public class Tokenizer {
  private final static @NotNull Logger logger = Logger.getInstance("Tokenizer");

  public static Token psiToTok(TreeElement elem) {
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
}
