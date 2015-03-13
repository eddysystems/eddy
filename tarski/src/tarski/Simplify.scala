package tarski

import tarski.Denotations._
import tarski.Environment.shadowedInSubtype
import tarski.Items._
import tarski.Types._
import utility.Locations._
import utility.Utility.impossible

object Simplify {
  def simplify(c: Callable): Callable = c match {
    case TypeApply(f,ts,a,hide) => TypeApply(simplify(f).asInstanceOf[NotTypeApply],ts,a,hide)
    case MethodDen(x,t,f,fr) => MethodDen(x map (simplify(_,superValid=true)),t,f,fr)
    case _:ForwardDen => c
    case NewDen(nr,x,f,fr,ts) => NewDen(nr,x map (simplify(_)),f,fr,ts)
    case NewArrayDen(nr,t,tr,ns,ds) => NewArrayDen(nr,t,tr,ns map simplify,ds)
  }

  def simplify(s: Stmt): Stmt = s match {
    case SemiStmt(x,sr) => SemiStmt(simplify(x),sr)
    case _:EmptyStmt|_:HoleStmt|_:BreakStmt|_:ContinueStmt|_:TokStmt => s
    case VarStmt(m,t,tr,vs,env) => VarStmt(m,t,tr,vs map simplify,env)
    case ExpStmt(e,env) => ExpStmt(simplify(e).asInstanceOf[StmtExp],env)
    case BlockStmt(b,a,env) => BlockStmt(b map simplify,a,env)
    case MultipleStmt(b) => MultipleStmt(b map simplify)
    case AssertStmt(ar,c,m,env) => AssertStmt(ar,simplify(c),m map simplify,env)
    case ReturnStmt(rr,e,env) => ReturnStmt(rr,e map (e => simplifyReturn(simplify(e))),env)
    case ThrowStmt(tr,e,env) => ThrowStmt(tr,simplify(e),env)
    case IfStmt(ir,c,a,x) => IfStmt(ir,simplify(c),a,simplify(x))
    case IfElseStmt(ir,c,a,x,er,y) => IfElseStmt(ir,simplify(c),a,simplify(x),er,simplify(y))
    case WhileStmt(wr,c,a,x) => WhileStmt(wr,simplify(c),a,simplify(x))
    case DoStmt(dr,x,wr,c,a) => DoStmt(dr,simplify(x),wr,simplify(c),a)
    case ForStmt(fr,i,c,sr,u,a,x) => ForStmt(fr,simplify(i),c map (simplify(_)),sr,u map (simplify(_)),a,simplify(x))
    case ForeachStmt(fr,m,t,tr,v,vr,e,a,x,env) => ForeachStmt(fr,m,t,tr,v,vr,simplify(e),a,simplify(x),env)
    case SyncStmt(sr,e,a,x) => SyncStmt(sr,simplify(e),a,simplify(x))
    case TryStmt(tr,x,cs,f) => TryStmt(tr,simplify(x),cs map simplify,f map simplifyFinally)
  }
  def simplify(d: VarDecl): VarDecl = d match {
    case VarDecl(x,xr,d,i,env) => VarDecl(x,xr,d,i map simplify,env)
  }
  def simplify(i: ForInit): ForInit = i match {
    case VarStmt(m,t,tr,vs,env) => VarStmt(m,t,tr,vs map simplify,env)
    case ForExps(xs,sr,env) => ForExps(xs map (simplify(_)),sr,env)
  }
  def simplify(c: CatchBlock): CatchBlock = c match {
    case CatchBlock(m,tr,v,vr,a,s) => CatchBlock(m,tr,v,vr,a,simplify(s))
  }
  def simplifyFinally(re: (SRange,Stmt)): (SRange,Stmt) = (re._1,simplify(re._2))

  def simplify(e: Exp, superValid: Boolean=false): Exp = e match {
    case WhateverExp(_,_,s) => impossible
    case ThisOrSuperExp(i:SuperItem,r) if !superValid => CastExp(i.ty,SGroup.approx(r),ThisOrSuperExp(i.down,r),gen=true)
    case _:Lit|_:LocalExp|_:ThisOrSuperExp => e
    case FieldExp(x,f,fr) => FieldExp(x map (x => simplifyFieldExp(simplify(x,superValid=true),f)),f,fr)
    case CastExp(ty,a,x,g) => CastExp(ty,a,simplify(x),g)
    case ImpExp(op,opr,x) => ImpExp(op,opr,simplify(x))
    case NonImpExp(op,opr,x) => NonImpExp(op,opr,simplify(x))
    case BinaryExp(op,opr,x,y) => BinaryExp(op,opr,simplify(x),simplify(y))
    case InstanceofExp(x,ir,t,tr) => InstanceofExp(simplify(x),ir,t,tr)
    case AssignExp(op,opr,x,y) => AssignExp(op,opr,simplify(x),simplify(y))
    case ParenExp(x,a) => ParenExp(simplify(x),a)
    case ApplyExp(f,xs,a,auto) => ApplyExp(simplify(f).asInstanceOf[NormalCallable],xs map (simplify(_)),a,auto)
    case IndexExp(x,i,a) => IndexExp(simplify(x),simplify(i),a)
    case CondExp(c,qr,x,cr,y,ty) => CondExp(simplify(c),qr,simplify(x),cr,simplify(y),ty)
    case ArrayExp(nr,t,tr,xs,a) => ArrayExp(nr,t,tr,xs map (simplify(_)),a)
    case EmptyArrayExp(nr,t,tr,is) => EmptyArrayExp(nr,t,tr,is map simplify)
    case AnonClassExp(c,as,ar,b) => AnonClassExp(simplify(c),as map (simplify(_)),ar,b)
  }
  def simplify(re: (SRange,Exp)): (SRange,Exp) = (re._1,simplify(re._2))
  def simplify(e: Grouped[Exp]): Grouped[Exp] = Grouped(simplify(e.x),e.a)

  // Remove unnecessary casts inside FieldExps
  def simplifyFieldExp(e: Exp, f: FieldItem): Exp = e match {
    case CastExp(ty,_,x,true) if isSubitem(x.item,ty.item) && !shadowedInSubtype(f,x.item.asInstanceOf[RefTypeItem]) => simplifyFieldExp(x,f)
    case CastExp(ty,_,ThisOrSuperExp(i:ThisItem,r),true) if ty.item == i.parent.base.item => ThisOrSuperExp(i.up,r)
    case _ => e
  }

  // Remove unnecessary casts inside return e
  def simplifyReturn(e: Exp): Exp = e match {
    case CastExp(ty,_,x,true) if isSubitem(x.item,ty.item) => simplifyReturn(x)
    case _ => e
  }
}
