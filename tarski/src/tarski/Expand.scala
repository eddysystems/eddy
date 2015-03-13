package tarski

import tarski.AST._
import tarski.Arounds._
import tarski.Denotations._
import tarski.Scores._
import utility.Locations._

import scala.language.implicitConversions

object Expand {
  type Expand[A] = A => Scored[A]

  @inline def expand[A](x: A)(implicit e: Expand[A]) = e(x)

  implicit def expandPair[A,B](ab: (A,B))(implicit a: Expand[A], b: Expand[B]): Scored[(A,B)] =
    product(a(ab._1),b(ab._2))

  implicit def expandOption[A](xs: Option[A])(implicit e: Expand[A]): Scored[Option[A]] =
    product(xs map e)

  implicit def expandList[A](xs: List[A])(implicit e: Expand[A]): Scored[List[A]] =
    product(xs map e)

  implicit def expandCommaList[A](xs: CommaList[A])(implicit e: Expand[A]): Scored[CommaList[A]] = xs match {
    case EmptyList => known(xs)
    case SingleList(x) => e(x) map (SingleList(_))
    case CommaList2(l,sep) => product(l map e) map (CommaList2(_,sep))
  }

  implicit def expandKList[A](xs: KList[A])(implicit e: Expand[A]): Scored[KList[A]] = xs match {
    case EmptyList => known(xs)
    case SingleList(x) => e(x) map (SingleList(_))
    case CommaList2(l,sep) => product(l map e) map (CommaList2(_,sep))
    case AndList2(l,sep) => product(l map e) map (AndList2(_,sep))
    case JuxtList2(l) => product(l map e) map (JuxtList2(_))
  }

  implicit def expandSRange(r: SRange): Scored[SRange] = known(r)

  implicit def expandGrouped[A](x: Grouped[A])(implicit e: Expand[A]): Scored[Grouped[A]] =
    expand(x.x) map (Grouped(_,x.a))

  implicit def expandAStmt(s: AStmt): Scored[AStmt] = s match {
    case ScoredAStmt(s,_) => s flatMap (expand(_))
    case SemiAStmt(s,sr) => expand(s) map (SemiAStmt(_,sr))
    case ParenAStmt(x,a) => expand(x) map (ParenAStmt(_,a))
    case VarAStmt(m,t,vs) => productWith(expand(t),expand(vs))(VarAStmt(m,_,_))
    case BlockAStmt(b,a) => product(b map (expand(_))) map (BlockAStmt(_,a))
    case ExpAStmt(e) => expand(e) map ExpAStmt
    case AssertAStmt(ar,c,m) => productWith(expand(c),expand(m))(AssertAStmt(ar,_,_))
    case ReturnAStmt(rr,e) => expand(e) map (ReturnAStmt(rr,_))
    case ThrowAStmt(tr,e) => expand(e) map (ThrowAStmt(tr,_))
    case SyncAStmt(sr,e,a,s) => productWith(expand(e),expand(s))(SyncAStmt(sr,_,a,_))
    case TryAStmt(tr,s,cs,f) => productWith(expand(s),expand(cs),expand(f))(TryAStmt(tr,_,_,_))
    case IfAStmt(ir,c,a,x) => productWith(expand(c),expand(x))(IfAStmt(ir,_,a,_))
    case IfElseAStmt(ir,c,a,x,er,y) => productWith(expand(c),expand(x),expand(y))(IfElseAStmt(ir,_,a,_,er,_))
    case WhileAStmt(wr,flip,c,a,s) => productWith(expand(c),expand(s))(WhileAStmt(wr,flip,_,a,_))
    case DoAStmt(dr,s,wr,flip,c,a) => productWith(expand(s),expand(c))(DoAStmt(dr,_,wr,flip,_,a))
    case ForAStmt(fr,i,a,s) => productWith(expand(i),expand(s))(ForAStmt(fr,_,a,_))
    case _:EmptyAStmt|_:HoleAStmt|_:TokAStmt|_:BreakAStmt|_:ContinueAStmt => known(s)
  }
  implicit def expandAVarDecl(d: AVarDecl): Scored[AVarDecl] = d match {
    case AVarDecl(x,xr,n,i) => expand(i) map (AVarDecl(x,xr,n,_))
  }
  implicit def expandCatchInfo(c: CatchInfo): Scored[CatchInfo] = c match {
    case CatchInfo(cr,m,t,i,a,c) => expand(t) map (CatchInfo(cr,m,_,i,a,c))
  }
  implicit def expandForInfo(f: ForInfo): Scored[ForInfo] = f match {
    case For(i,sr0,c,sr1,u) => productWith(expand(i),expand(c),expand(u))(For(_,sr0,_,sr1,_))
    case Foreach(m,t,v,vr,n,cr,e) => productWith(expand(t),expand(e))(Foreach(m,_,v,vr,n,cr,_))
  }

  implicit def expandAExp(e: AExp): Scored[AExp] = e match {
    case ScoredAExp(s,_) => s flatMap (expand(_))
    case ParenAExp(e,a) => expand(e) map (ParenAExp(_,a))
    case FieldAExp(e,dot,t,f,fr) => productWith(expand(e),expand(t))(FieldAExp(_,dot,_,f,fr))
    case MethodRefAExp(e,ccr,t,f,fr) => productWith(expand(e),expand(t))(MethodRefAExp(_,ccr,_,f,fr))
    case NewRefAExp(e,cc,t,newr) => productWith(expand(e),expand(t))(NewRefAExp(_,cc,_,newr))
    case TypeApplyAExp(e,t,tr,after) => productWith(expand(e),expand(t))(TypeApplyAExp(_,_,tr,after))
    case ApplyAExp(e,xs,l) => productWith(expand(e),expand(xs))(ApplyAExp(_,_,l))
    case NewAExp(qe,newr,t,e,ns) => productWith(expand(qe),expand(t),expand(e))(NewAExp(_,newr,_,_,ns))
    case UnaryAExp(op,opr,e) => expand(e) map (UnaryAExp(op,opr,_))
    case BinaryAExp(op,opr,e0,e1) => productWith(expand(e0),expand(e1))(BinaryAExp(op,opr,_,_))
    case CastAExp(t,a,e) => productWith(expand(t),expand(e))(CastAExp(_,a,_))
    case CondAExp(c,qr,x,cr,y) => productWith(expand(c),expand(x),expand(y))(CondAExp(_,qr,_,cr,_))
    case AssignAExp(op,opr,x,y) => productWith(expand(x),expand(y))(AssignAExp(op,opr,_,_))
    case ArrayAExp(e,a) => expand(e) map (ArrayAExp(_,a))
    case InstanceofAExp(e,ir,t) => productWith(expand(e),expand(t))(InstanceofAExp(_,ir,_))
    case _:NameAExp|_:WildAExp|_:ALit => known(e)
  }

  implicit def expandCallable(c: Callable): Scored[Callable] = c match {
    case TypeApply(f,ts,a,hide) => expandCallable(f) map (f => TypeApply(f.asInstanceOf[NotTypeApply],ts,a,hide))
    case MethodDen(x,t,f,fr) => expand(x) map (MethodDen(_,t,f,fr))
    case _:ForwardDen => known(c)
    case NewDen(nr,x,f,fr,ts) => expand(x) map (NewDen(nr,_,f,fr,ts))
    case NewArrayDen(nr,t,tr,ns,ds) => expand(ns) map (NewArrayDen(nr,t,tr,_,ds))
  }

  implicit def expandStmt(s: Stmt): Scored[Stmt] = s match {
    case SemiStmt(x,sr) => expand(x) map (SemiStmt(_,sr))
    case _:EmptyStmt|_:HoleStmt|_:BreakStmt|_:ContinueStmt|_:TokStmt => known(s)
    case VarStmt(m,t,tr,vs,env) => expand(vs) map (VarStmt(m,t,tr,_,env))
    case ExpStmt(e,env) => expandExp(e) map (e => ExpStmt(e.asInstanceOf[StmtExp],env))
    case BlockStmt(b,a,env) => expand(b) map (BlockStmt(_,a,env))
    case MultipleStmt(b) => expand(b) map MultipleStmt
    case AssertStmt(ar,c,m,env) => productWith(expand(c),expand(m))(AssertStmt(ar,_,_,env))
    case ReturnStmt(rr,e,env) => expand(e) map (ReturnStmt(rr,_,env))
    case ThrowStmt(tr,e,env) => expand(e) map (ThrowStmt(tr,_,env))
    case IfStmt(ir,c,a,x) => productWith(expand(c),expand(x))(IfStmt(ir,_,a,_))
    case IfElseStmt(ir,c,a,x,er,y) => productWith(expand(c),expand(x),expand(y))(IfElseStmt(ir,_,a,_,er,_))
    case WhileStmt(wr,c,a,x) => productWith(expand(c),expand(x))(WhileStmt(wr,_,a,_))
    case DoStmt(dr,x,wr,c,a) => productWith(expand(x),expand(c))(DoStmt(dr,_,wr,_,a))
    case ForStmt(fr,i,c,sr,u,a,x) => productWith(expand(i),expand(c),expand(u),expand(x))(ForStmt(fr,_,_,sr,_,a,_))
    case ForeachStmt(fr,m,t,tr,v,vr,e,a,x,env) => productWith(expand(e),expand(x))(ForeachStmt(fr,m,t,tr,v,vr,_,a,_,env))
    case SyncStmt(sr,e,a,x) => productWith(expand(e),expand(x))(SyncStmt(sr,_,a,_))
    case TryStmt(tr,x,cs,f) => productWith(expand(x),expand(cs),expand(f))(TryStmt(tr,_,_,_))
  }
  implicit def expandVarDecl(d: VarDecl): Scored[VarDecl] = d match {
    case VarDecl(x,xr,d,i,env) => expand(i) map (VarDecl(x,xr,d,_,env))
  }
  implicit def expandForInit(i: ForInit): Scored[ForInit] = i match {
    case VarStmt(m,t,tr,vs,env) => expand(vs) map (VarStmt(m,t,tr,_,env))
    case ForExps(xs,sr,env) => expand(xs) map (ForExps(_,sr,env))
  }
  implicit def expandCatchBlock(c: CatchBlock): Scored[CatchBlock] = c match {
    case CatchBlock(m,tr,v,vr,a,s) => expand(s) map (CatchBlock(m,tr,v,vr,a,_))
  }
  def expandStmts(ss: List[Stmt]): Scored[List[Stmt]] = expandList(ss)(expandStmt)

  implicit def expandExp(e: Exp): Scored[Exp] = e match {
    case WhateverExp(_,_,s) => s flatMap expandExp
    case _:Lit|_:LocalExp|_:ThisOrSuperExp => known(e)
    case FieldExp(x,f,fr) => expand(x) map (FieldExp(_,f,fr))
    case CastExp(ty,a,x,g) => expand(x) map (CastExp(ty,a,_,g))
    case ImpExp(op,opr,x) => expand(x) map (ImpExp(op,opr,_))
    case NonImpExp(op,opr,x) => expand(x) map (NonImpExp(op,opr,_))
    case BinaryExp(op,opr,x,y) => productWith(expand(x),expand(y))(BinaryExp(op,opr,_,_))
    case InstanceofExp(x,ir,t,tr) => expand(x) map (InstanceofExp(_,ir,t,tr))
    case AssignExp(op,opr,x,y) => productWith(expand(x),expand(y))(AssignExp(op,opr,_,_))
    case ParenExp(x,a) => expand(x) map (ParenExp(_,a))
    case ApplyExp(f,xs,a,auto) => productWith(expandCallable(f),expand(xs))((f,xs) => ApplyExp(f.asInstanceOf[NormalCallable],xs,a,auto))
    case IndexExp(x,i,a) => productWith(expand(x),expand(i))(IndexExp(_,_,a))
    case CondExp(c,qr,x,cr,y,ty) => productWith(expand(c),expand(x),expand(y))(CondExp(_,qr,_,cr,_,ty))
    case ArrayExp(nr,t,tr,xs,a) => expand(xs) map (ArrayExp(nr,t,tr,_,a))
    case EmptyArrayExp(nr,t,tr,is) => expand(is) map (EmptyArrayExp(nr,t,tr,_))
    case AnonClassExp(c,as,ar,b) => productWith(expandCallable(c),expand(as))( (c,args) => AnonClassExp(c,args,ar,b))
  }
}
