package tarski

import tarski.Arounds._
import tarski.Mods._
import tarski.Operators.{AssignOp, BinaryOp, UnaryOp}
import tarski.Tokens.{IdentTok, StmtTok}
import utility.Locations._

object AST {
  type Name = String

  sealed abstract class Bound
  case object Extends extends Bound
  case object Super extends Bound
  case class WildBound(b: Bound, br: SRange, t: AExp)

  type Block = List[AStmt]
  type ADims = List[SGroup]
  type ADimExps = List[Grouped[Option[AExp]]]
  case class AVarDecl(x: Name, xr: SRange, n: ADims, i: Option[(SRange,AExp)]) extends HasRange {
    def r = i match { case None => xr; case Some((_,e)) => xr union e.r }
  }

  sealed abstract class AStmt extends HasRange
  case class SemiAStmt(s: AStmt, sr: SRange) extends AStmt {
    def r = s.r union sr
  }
  case class EmptyAStmt(r: SRange) extends AStmt
  case class HoleAStmt(r: SRange) extends AStmt
  case class VarAStmt(m: Mods, t: Option[AExp], v: KList[AVarDecl]) extends AStmt {
    def r = v.list.last.r unionR m unionR t
  }
  case class BlockAStmt(b: Block, a: SGroup) extends AStmt {
    def r = a.lr
  }
  case class TokAStmt(b: StmtTok, r: SRange) extends AStmt
  case class ExpAStmt(e: AExp) extends AStmt {
    def r = e.r
  }
  case class AssertAStmt(ar: SRange, cond: AExp, msg: Option[(SRange,AExp)]) extends AStmt {
    def r = msg match { case None => ar; case Some((_,e)) => ar union e.r }
  }
  case class BreakAStmt(br: SRange, label: Option[Loc[Name]]) extends AStmt {
    def r = br unionR label
  }
  case class ContinueAStmt(cr: SRange, label: Option[Loc[Name]]) extends AStmt {
    def r = cr unionR label
  }
  case class ReturnAStmt(rr: SRange, e: Option[AExp]) extends AStmt {
    def r = rr unionR e
  }
  case class ThrowAStmt(tr: SRange, e: AExp) extends AStmt {
    def r = tr union e.r
  }
  case class SyncAStmt(sr: SRange, e: AExp, a: Around, s: AStmt) extends AStmt {
    def r = sr union s.r
  }
  case class TryAStmt(tr: SRange, s: AStmt, cs: List[(CatchInfo,AStmt)],fs: Option[AStmt]) extends AStmt {
    def r = tr union fs.getOrElse(cs.lastOption.getOrElse((Nil,s))._2).r
  }
  case class PreIf(f: SRange => AStmt) { // For use in the parser.  TODO: Remove once the generator handles function types
    def apply(ir: SRange) = f(ir)
  }
  case class IfAStmt(ir: SRange, cond: AExp, a: Around, x: AStmt) extends AStmt {
    def r = ir union x.r
  }
  case class IfElseAStmt(ir: SRange, cond: AExp, a: Around, x: AStmt, er: SRange, y: AStmt) extends AStmt {
    def r = ir union y.r
  }
  case class WhileAStmt(wr: SRange, flip: Boolean, cond: AExp, a: Around, s: AStmt) extends AStmt {
    def r = wr union s.r
  }
  case class DoAStmt(dr: SRange, s: AStmt, wr: SRange, flip: Boolean, cond: AExp, a: Around) extends AStmt {
    def r = dr union a.r
  }
  case class ForAStmt(fr: SRange, i: ForInfo, a: Around, s: AStmt) extends AStmt {
    def r = fr union s.r
  }

  case class CatchInfo(cr: SRange, t: Option[AExp], i: Option[Name], ir: SRange, a: Around, colon: Boolean) extends HasRange {
    def r = cr union a.r
  }

  sealed abstract class ForInfo extends HasRange
  case class For(i: CommaList[AStmt], sr0: SRange, cond: Option[AExp], sr1: SRange, u: CommaList[AExp]) extends ForInfo {
    def r = sr0 union sr1 unionR i.list unionR u.list
  }
  case class Foreach(m: Mods, t: Option[AExp], v: Name, vr: SRange, n: ADims, cr: SRange, e: AExp) extends ForInfo {
    def r = vr unionR m unionR t union e.r
  }

  sealed abstract class AExp extends HasRange
  case class NameAExp(name: Name, r: SRange) extends AExp
  case class ParenAExp(e: AExp, a: YesAround) extends AExp {
    def r = a.a.r
  }
  case class FieldAExp(e: AExp, dot: SRange, t: Option[Grouped[KList[AExp]]], f: Name, fr: SRange) extends AExp {
    def r = e.r union fr
  }
  case class MethodRefAExp(e: AExp, ccr: SRange, t: Option[Grouped[KList[AExp]]], f: Name, fr: SRange) extends AExp {
    def r = e.r union fr
  }
  case class NewRefAExp(e: AExp, cc: SRange, t: Option[Grouped[KList[AExp]]], newr: SRange) extends AExp {
    def r = e.r union newr
  }
  case class TypeApplyAExp(e: AExp, t: KList[AExp], tr: SGroup, after: Boolean) extends AExp { // after ? A<T> : <T>A
    def r = e.r union tr.lr
  }
  case class ApplyAExp(e: AExp, xs: KList[AExp], l: Around) extends AExp {
    def r = e.r union l.r
  }
  case class NewAExp(newr: SRange, t: Option[Grouped[KList[AExp]]], e: AExp, ns: ADimExps = Nil) extends AExp {
    def r = newr union e.r unionR ns
  }
  case class WildAExp(qr: SRange, b: Option[WildBound]) extends AExp {
    def r = b match { case None => qr; case Some(WildBound(_,_,t)) => qr union t.r }
  }
  case class UnaryAExp(op: UnaryOp, opr: SRange, e: AExp) extends AExp {
    def r = opr union e.r
  }
  case class BinaryAExp(op: BinaryOp, opr: SRange, e0: AExp, e1: AExp) extends AExp {
    def r = e0.r union e1.r
  }
  case class CastAExp(t: AExp, a: YesAround, e: AExp) extends AExp {
    def r = a.a.l union e.r
  }
  case class CondAExp(cond: AExp, qr: SRange, t: AExp, cr: SRange, f: AExp) extends AExp {
    def r = cond.r union f.r
  }
  case class AssignAExp(op: Option[AssignOp], opr: SRange, left: AExp, right: AExp) extends AExp {
    def r = left.r union right.r
  }
  case class ArrayAExp(e: KList[AExp], a: Around) extends AExp {
    def r = a.r
  }
  case class InstanceofAExp(e: AExp, ir: SRange, t: AExp) extends AExp {
    def r = e.r union t.r
  }

  sealed abstract class ALit extends AExp
  case class IntALit(v: String, r: SRange) extends ALit
  case class LongALit(v: String, r: SRange) extends ALit
  case class FloatALit(v: String, r: SRange) extends ALit
  case class DoubleALit(v: String, r: SRange) extends ALit
  case class CharALit(v: String, r: SRange) extends ALit
  case class StringALit(v: String, r: SRange) extends ALit
}
