package connectorFamily.connector

object Eval {
  /** Evaluate (simplify) a connector type (not a family). */
  def evaluate(t:CType): (Interface,Interface) = t match {
    case IPair(l,r) => (evaluate(l),evaluate(r))
    case CPair(l,r) => 
      val (l1,l2) = evaluate(l)
      val (r1,r2) = evaluate(r)
    	(evaluate(l1++r1), evaluate(l2++r2))
  }
  
  /** Evaluate (simplify) an interface. */
  def evaluate(i:Interface) : Interface = i.get match {
  	case Nil => i
    case INat(n)::rest => Interface(INat(n)) ++ evaluate(Interface(rest))
    case IDual(lit)::rest =>
    	val ev = evaluate(Interface(lit))    	
    	ev.inv ++ evaluate(Interface(rest)) 
    case IIndBool(iTrue, _ , VTrue) ::rest => evaluate(iTrue ++Interface(rest))
    case IIndBool(_, iFalse, VFalse)::rest => evaluate(iFalse++Interface(rest))
    case IIndNat(iZero, _, _, _, VZero)::rest => evaluate(iZero++Interface(rest))
    case IIndNat(iZero, vv, iv, iSucc, VSucc(n))::rest =>
    	val subs  = Substitution(vv->n)
    	val isubs = ISubst(iv->IIndNat(iZero,vv,iv,iSucc,n))
//    	println("Substituting "+PP(iv)+" by "+PP(IIndNat(iZero,vv,iv,iSucc,n))+" in "+PP(iSucc)+"\n got: "+PP(isubs(iSucc)))
//    	println("Used substitution "+isubs)
    	evaluate(isubs(subs(iSucc))++Interface(rest))
    case (v:IVar)::rest => v // NOT sure...
    case _ => i // NOT sure...
    //throw new TypeException("Could not evaluate interface "+PP(i))
  }
}