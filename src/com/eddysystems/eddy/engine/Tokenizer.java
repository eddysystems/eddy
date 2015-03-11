package com.eddysystems.eddy.engine;

import com.intellij.openapi.util.TextRange;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.tree.ChildRole;
import com.intellij.psi.impl.source.tree.CompositeElement;
import utility.Locations;
import utility.Locations.Loc;
import com.intellij.psi.impl.source.tree.TreeElement;
import com.intellij.psi.tree.IElementType;
import tarski.Tokens.*;

class Tokenizer {

  public static Loc<Token> locTok(int lo, int hi, Token tok) {
    return Locations.locatedHelper(tok,Locations.buildHelper(lo,hi));
  }

  public static Loc<Token> psiToTok(final TreeElement elem) {
    final int lo = elem.getTextOffset(),
              hi = lo+elem.getTextLength();
    return locTok(lo, hi, token(elem));
  }

  public static <A> TextRange range(final Loc<A> x) {
    return new TextRange(x.rawLo(),x.rawHi());
  }

  public static boolean isSpace(final TreeElement elem) {
    return elem.getElementType() == JavaTokenType.WHITE_SPACE;
  }

  // Uninterpreted statements and code blocks
  private static final class PsiStmtTok extends StmtTok {
    final PsiElement psi;

    PsiStmtTok(final PsiCodeBlock block) {
      this.psi = block;
    }
    PsiStmtTok(final PsiStatement stmt) {
      this.psi = stmt;
    }

    public boolean isBlock() {
      return psi instanceof PsiCodeBlock;
    }

    private final static String sentinel = initSentinel();
    private static String initSentinel() {
      final String s = "_PsiStmtTokSentinelEllipsis_();";
      ShowFlags$.MODULE$.registerSentinel(s,"...");
      return s;
    }

    public String show(final ShowFlags f) {
      return f.abbreviate() ? f.valid() ? isBlock() ? "{ "+sentinel+" }" : sentinel
                                        : isBlock() ? "{ ... }" : "..."
                            : psi.getText();
    }

    @Override public String toString() {
      return "PsiStmtTok(...)";
    }
  }

  // Uninterpreted anonymous class body (stores whole anon class PsiElement, but only represents the body
  public static final class AtomicAnonBodyTok extends AnonBodyTok {
    final PsiAnonymousClass anon;
    final String text;

    AtomicAnonBodyTok(final PsiAnonymousClass anon) {
      this(anon, (TreeElement)((CompositeElement)anon.getNode()).findChildByRole(ChildRole.LBRACE));
    }

    AtomicAnonBodyTok(final PsiAnonymousClass anon, final TreeElement lbrace) {
      this.anon = anon;
      this.text = anon.getText().substring(lbrace.getStartOffsetInParent());
    }

    public boolean equals(Object o) {
      return o instanceof AtomicAnonBodyTok && ((AtomicAnonBodyTok)o).anon == anon;
    }

    public String show(final ShowFlags f) {
      return f.abbreviate() ? "{ ... }" : text;
    }
  }

  public static Token token(final TreeElement elem) {
    final IElementType type = elem.getElementType();

    if (type == JavaTokenType.IDENTIFIER) return new IdentTok(elem.getText());

    if (type == JavaTokenType.C_STYLE_COMMENT)     return new CCommentTok(elem.getText());
    if (type == JavaTokenType.END_OF_LINE_COMMENT) return new EOLCommentTok(elem.getText());

    if (type == JavaTokenType.INTEGER_LITERAL)   return new IntLitTok(elem.getText());
    if (type == JavaTokenType.LONG_LITERAL)      return new LongLitTok(elem.getText());
    if (type == JavaTokenType.FLOAT_LITERAL)     return new FloatLitTok(elem.getText());
    if (type == JavaTokenType.DOUBLE_LITERAL)    return new DoubleLitTok(elem.getText());
    if (type == JavaTokenType.CHARACTER_LITERAL) return new CharLitTok(elem.getText());
    if (type == JavaTokenType.STRING_LITERAL)    return new StringLitTok(elem.getText());
    if (type == JavaTokenType.TRUE_KEYWORD)      return new BoolLitTok(true);
    if (type == JavaTokenType.FALSE_KEYWORD)     return new BoolLitTok(false);
    if (type == JavaTokenType.NULL_KEYWORD)      return NullTok$.MODULE$;

    if (type == JavaTokenType.ABSTRACT_KEYWORD)     return AbstractTok$.MODULE$;
    if (type == JavaTokenType.ASSERT_KEYWORD)       return AssertTok$.MODULE$;
    if (type == JavaTokenType.BOOLEAN_KEYWORD)      return BooleanTok$.MODULE$;
    if (type == JavaTokenType.BREAK_KEYWORD)        return BreakTok$.MODULE$;
    if (type == JavaTokenType.BYTE_KEYWORD)         return ByteTok$.MODULE$;
    if (type == JavaTokenType.CASE_KEYWORD)         return CaseTok$.MODULE$;
    if (type == JavaTokenType.CATCH_KEYWORD)        return CatchTok$.MODULE$;
    if (type == JavaTokenType.CHAR_KEYWORD)         return CharTok$.MODULE$;
    if (type == JavaTokenType.CLASS_KEYWORD)        return ClassTok$.MODULE$;
    if (type == JavaTokenType.CONST_KEYWORD)        return ConstTok$.MODULE$;
    if (type == JavaTokenType.CONTINUE_KEYWORD)     return ContinueTok$.MODULE$;
    if (type == JavaTokenType.DEFAULT_KEYWORD)      return DefaultTok$.MODULE$;
    if (type == JavaTokenType.DO_KEYWORD)           return DoTok$.MODULE$;
    if (type == JavaTokenType.DOUBLE_KEYWORD)       return DoubleTok$.MODULE$;
    if (type == JavaTokenType.ELSE_KEYWORD)         return ElseTok$.MODULE$;
    if (type == JavaTokenType.ENUM_KEYWORD)         return EnumTok$.MODULE$;
    if (type == JavaTokenType.EXTENDS_KEYWORD)      return ExtendsTok$.MODULE$;
    if (type == JavaTokenType.FINAL_KEYWORD)        return FinalTok$.MODULE$;
    if (type == JavaTokenType.FINALLY_KEYWORD)      return FinallyTok$.MODULE$;
    if (type == JavaTokenType.FLOAT_KEYWORD)        return FloatTok$.MODULE$;
    if (type == JavaTokenType.FOR_KEYWORD)          return ForTok$.MODULE$;
    if (type == JavaTokenType.GOTO_KEYWORD)         return GotoTok$.MODULE$;
    if (type == JavaTokenType.IF_KEYWORD)           return IfTok$.MODULE$;
    if (type == JavaTokenType.IMPLEMENTS_KEYWORD)   return ImplementsTok$.MODULE$;
    if (type == JavaTokenType.IMPORT_KEYWORD)       return ImportTok$.MODULE$;
    if (type == JavaTokenType.INSTANCEOF_KEYWORD)   return InstanceofTok$.MODULE$;
    if (type == JavaTokenType.INT_KEYWORD)          return IntTok$.MODULE$;
    if (type == JavaTokenType.INTERFACE_KEYWORD)    return InterfaceTok$.MODULE$;
    if (type == JavaTokenType.LONG_KEYWORD)         return LongTok$.MODULE$;
    if (type == JavaTokenType.NATIVE_KEYWORD)       return NativeTok$.MODULE$;
    if (type == JavaTokenType.NEW_KEYWORD)          return NewTok$.MODULE$;
    if (type == JavaTokenType.PACKAGE_KEYWORD)      return PackageTok$.MODULE$;
    if (type == JavaTokenType.PRIVATE_KEYWORD)      return PrivateTok$.MODULE$;
    if (type == JavaTokenType.PUBLIC_KEYWORD)       return PublicTok$.MODULE$;
    if (type == JavaTokenType.SHORT_KEYWORD)        return ShortTok$.MODULE$;
    if (type == JavaTokenType.SUPER_KEYWORD)        return SuperTok$.MODULE$;
    if (type == JavaTokenType.SWITCH_KEYWORD)       return SwitchTok$.MODULE$;
    if (type == JavaTokenType.SYNCHRONIZED_KEYWORD) return SynchronizedTok$.MODULE$;
    if (type == JavaTokenType.THIS_KEYWORD)         return ThisTok$.MODULE$;
    if (type == JavaTokenType.THROW_KEYWORD)        return ThrowTok$.MODULE$;
    if (type == JavaTokenType.PROTECTED_KEYWORD)    return ProtectedTok$.MODULE$;
    if (type == JavaTokenType.TRANSIENT_KEYWORD)    return TransientTok$.MODULE$;
    if (type == JavaTokenType.RETURN_KEYWORD)       return ReturnTok$.MODULE$;
    if (type == JavaTokenType.VOID_KEYWORD)         return VoidTok$.MODULE$;
    if (type == JavaTokenType.STATIC_KEYWORD)       return StaticTok$.MODULE$;
    if (type == JavaTokenType.STRICTFP_KEYWORD)     return StrictfpTok$.MODULE$;
    if (type == JavaTokenType.WHILE_KEYWORD)        return WhileTok$.MODULE$;
    if (type == JavaTokenType.TRY_KEYWORD)          return TryTok$.MODULE$;
    if (type == JavaTokenType.VOLATILE_KEYWORD)     return VolatileTok$.MODULE$;
    if (type == JavaTokenType.THROWS_KEYWORD)       return ThrowsTok$.MODULE$;

    if (type == JavaTokenType.LPARENTH)  return LParenTok$.MODULE$;
    if (type == JavaTokenType.RPARENTH)  return RParenTok$.MODULE$;
    if (type == JavaTokenType.LBRACE)    return LCurlyTok$.MODULE$;
    if (type == JavaTokenType.RBRACE)    return RCurlyTok$.MODULE$;
    if (type == JavaTokenType.LBRACKET)  return LBrackTok$.MODULE$;
    if (type == JavaTokenType.RBRACKET)  return RBrackTok$.MODULE$;
    if (type == JavaTokenType.SEMICOLON) return SemiTok$.MODULE$;
    if (type == JavaTokenType.COMMA)     return CommaTok$.MODULE$;
    if (type == JavaTokenType.DOT)       return DotTok$.MODULE$;
    if (type == JavaTokenType.ELLIPSIS)  return EllipsisTok$.MODULE$;
    if (type == JavaTokenType.AT)        return AtTok$.MODULE$;

    if (type == JavaTokenType.EQ)       return EqTok$.MODULE$;
    if (type == JavaTokenType.GT)       return GtTok$.MODULE$;
    if (type == JavaTokenType.LT)       return LtTok$.MODULE$;
    if (type == JavaTokenType.EXCL)     return NotTok$.MODULE$;
    if (type == JavaTokenType.TILDE)    return CompTok$.MODULE$;
    if (type == JavaTokenType.QUEST)    return QuestionTok$.MODULE$;
    if (type == JavaTokenType.COLON)    return ColonTok$.MODULE$;
    if (type == JavaTokenType.PLUS)     return PlusTok$.MODULE$;
    if (type == JavaTokenType.MINUS)    return MinusTok$.MODULE$;
    if (type == JavaTokenType.ASTERISK) return MulTok$.MODULE$;
    if (type == JavaTokenType.DIV)      return DivTok$.MODULE$;
    if (type == JavaTokenType.AND)      return AndTok$.MODULE$;
    if (type == JavaTokenType.OR)       return OrTok$.MODULE$;
    if (type == JavaTokenType.XOR)      return XorTok$.MODULE$;
    if (type == JavaTokenType.PERC)     return ModTok$.MODULE$;

    if (type == JavaTokenType.EQEQ)       return EqEqTok$.MODULE$;
    if (type == JavaTokenType.LE)         return LeTok$.MODULE$;
    if (type == JavaTokenType.GE)         return GeTok$.MODULE$;
    if (type == JavaTokenType.NE)         return NeTok$.MODULE$;
    if (type == JavaTokenType.ANDAND)     return AndAndTok$.MODULE$;
    if (type == JavaTokenType.OROR)       return OrOrTok$.MODULE$;
    if (type == JavaTokenType.PLUSPLUS)   return PlusPlusTok$.MODULE$;
    if (type == JavaTokenType.MINUSMINUS) return MinusMinusTok$.MODULE$;
    if (type == JavaTokenType.LTLT)       return LShiftTok$.MODULE$;
    if (type == JavaTokenType.GTGT)       return RShiftTok$.MODULE$;
    if (type == JavaTokenType.GTGTGT)     return UnsignedRShiftTok$.MODULE$;
    if (type == JavaTokenType.PLUSEQ)     return PlusEqTok$.MODULE$;
    if (type == JavaTokenType.MINUSEQ)    return MinusEqTok$.MODULE$;
    if (type == JavaTokenType.ASTERISKEQ) return MulEqTok$.MODULE$;
    if (type == JavaTokenType.DIVEQ)      return DivEqTok$.MODULE$;
    if (type == JavaTokenType.ANDEQ)      return AndEqTok$.MODULE$;
    if (type == JavaTokenType.OREQ)       return OrEqTok$.MODULE$;
    if (type == JavaTokenType.XOREQ)      return XorEqTok$.MODULE$;
    if (type == JavaTokenType.PERCEQ)     return ModEqTok$.MODULE$;
    if (type == JavaTokenType.LTLTEQ)     return LShiftEqTok$.MODULE$;
    if (type == JavaTokenType.GTGTEQ)     return RShiftEqTok$.MODULE$;
    if (type == JavaTokenType.GTGTGTEQ)   return UnsignedRShiftEqTok$.MODULE$;

    if (type == JavaTokenType.DOUBLE_COLON) return ColonColonTok$.MODULE$;
    if (type == JavaTokenType.ARROW)        return ArrowTok$.MODULE$;

    if (type == TokenType.WHITE_SPACE)   return new WhitespaceTok(elem.getText());
    if (type == TokenType.BAD_CHARACTER) return new IllegalTok(elem.getText());

    final PsiElement psi = elem.getPsi();
    if (psi instanceof PsiStatement) return new PsiStmtTok((PsiStatement)psi);
    if (psi instanceof PsiCodeBlock) return new PsiStmtTok((PsiCodeBlock)psi);

    throw new RuntimeException("unknown token: type " + type + ", text " + elem.getText());
  }
}
