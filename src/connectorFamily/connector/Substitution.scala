package connectorFamily.connector

/** a substitution from vars (VVar or CVar) to values (Val or Conn) */
class Substitution {
  val vals: Map[VVar,Val] = Map()
  val cons: Map[CVar,Conn] = Map()
  
  def -(v:VVar) = Substitution(vals-v,cons)

  def -(v:CVar) = Substitution(vals,cons-v)  
  
  /** Joins the maps with the substitutions */
  def ++(other:Substitution) =
    Substitution(vals++other.vals, cons++other.cons)
  
  def apply(v:Val) : Val = v match {
    case VSucc(n) => VSucc(apply(n)) 
    case v: VVar => if (vals contains v) vals(v) else v
    case _ => v
  }
  
  def apply(c:Conn) : Conn = c match {
  	case Seq(c1,c2) => Seq(apply(c1),apply(c2))
  	case Par(c1,c2) => Par(apply(c1),apply(c2))
  	case Lambda(v:CVar,  t:CType, c:Conn) => Lambda(v,t, (this-v).apply(c))
  	case LambdaV(v:VVar, t:VType, c:Conn) => LambdaV(v,t, (this-v).apply(c))
  	case App(c1:Conn,c2:Conn) => App(apply(c1),apply(c2))
  	case AppV(c1:Conn,c2:Val) => AppV(apply(c1),apply(c2))
  	case IndBool(vt:VVar, t:CType, ct:Conn, cf:Conn, bool:Val) =>
  	  IndBool(vt,(this-vt).apply(t),apply(ct),apply(cf),apply(bool))
  	case IndNat(vt:VVar, t:CType, c0:Conn, vs:CVar, cs:Conn, nat:Val) =>
  	  IndNat(vt,(this-vt).apply(t), apply(c0), vs, (this-vt-vs).apply(cs),apply(nat))
  	case _:CPrim => c
  	case v:CVar => if (cons contains v) cons(v) else v
  }
  
  def apply(i:Interface): Interface =
    Interface(i.get.map(apply(_)))
  
  def apply(lit:ILit) : ILit = lit match {
	case _ => lit
//	case IIndBool(ifTrue:Interface, ifFalse:Interface, b:Val) 
//	case IIndNat(ifZero:Interface, pred:VVar, predT: IVar, ifSucc:Interface, n:Val)
//	case INat(n:Int)
//	case IDual(i:ILit)
//	case v:IVar
  }

  def apply(t:CType): CType = t match {
  	case IPair(left,right) => IPair(apply(left),apply(right)) 
  	case CPair(left,right) => CPair(apply(left),apply(right))
  }
	
  def apply(t:FType): FType = t match {
  	case Prod(v,tpar,t)  => Prod(v,tpar,(this - v).apply(t))
	case ProdV(v,tpar,t) => ProdV(v,tpar,(this - v).apply(t))
	case c:CType => apply(c)
  }
} 


object Substitution {
//  def apply(v1v2:(CVar,Conn)) = new Substitution { override val cons = Map(v1v2) }
//  def apply(v1v2:(VVar,Val)) = new Substitution { override val vals = Map(v1v2) }
	
  // avoiding type lost by erasure...
  def apply(pair:(_,_)) = pair._1 match {
    case v1:CVar => pair._2 match {
      case v2:Conn => new Substitution { override val cons = Map(v1->v2) }
    }
    case v1:VVar => pair._2 match {
      case v2:Val => new Substitution { override val vals = Map(v1->v2) }
    }
  }
  
  def apply(vs:Map[VVar,Val], cs:Map[CVar,Conn]) =
  	new Substitution { override val vals = vs; override val cons = cs }
}


/** Substitution for constraints - interface vars so far.
 * Order is important! Start with the left first, then try the later (after substitution).  
 */
class ISubst {
//  val vars = Map[IVar,ILit]()
	val vars = List[(IVar,ILit)]()
	val bound = Set[IVar]()
  
  
  def -(v:IVar) = ISubst(vars,bound+v)

  /** Joins the maps with the substitutions */
  def ++(other:ISubst) =
  	ISubst(other.vars ++ vars, bound ++ other.bound) // ORDER of vars is important
  

  /**
   * Replace a variable by its assigned interface.
   * If the variable is found, continue to apply the substitution to the new interface,
   * but ignoring the substitutions already past.
   */  	
  private def replace(v:IVar): ILit =
  	if (bound contains v) v else replace(v,vars)
  	
  private def replace(v:IVar,vars2:List[(IVar,ILit)]): ILit = vars2 match {
  	case Nil => v
  	case (iv,il)::rest =>
  		if (v==iv)
  			ISubst(rest,bound).apply(il) // chose il and continue substitution with the rest of the substitutions
			else replace(v,rest) 
  }
  	  
	/** apply a substitution of interface variables. */
  def apply(l:ILit): ILit = l match {
    case INat(n) => l
    case IDual(lit) => IDual(apply(lit))
    case IIndBool(iTrue, iFalse, b) =>
      IIndBool(apply(iTrue),apply(iFalse),b)
    case IIndNat(iZero, vvar, ivar, iSucc, n) =>
      IIndNat(apply(iZero),vvar,ivar,(this-ivar)(iSucc),n)
    case v:IVar => replace(v)   	
//    	if (vars contains v) vars(v) else v
  }
  
  /** apply a substitution of interface variables. */
  def apply(i:Interface): Interface = Interface( i.get.map(apply(_)) )
  
  /** apply a substitution of interface variables. */
  def apply(c:CType): CType = c match {
    case IPair(l,r) => IPair(apply(l),apply(r))
    case CPair(l,r) => CPair(apply(l),apply(r))
  }
  
  /** apply a substitution of interface variables. */
  def apply(c:Const) : Const = c match {
    case CEq(t1:CType,t2:CType)         => CEq(apply(t1),apply(t2)) 
    case IEq(t1:Interface,t2:Interface) => IEq(apply(t1),apply(t2))
    case VEq(t1:VType,t2:VType)         => c
    case LEq(t1:ILit,t2:ILit)           => LEq(apply(t1),apply(t2))
  }
  
  def apply(t:FType) : FType = t match {
  	case c:CType => apply(c)
  	case Prod(v:CVar,tpar:CType,t:FType) => Prod(v,apply(tpar),apply(t))
  	case ProdV(v:VVar,tpar:VType,t:FType) => ProdV(v,tpar,apply(t))
  }
}

object ISubst {
  def apply(vl:(IVar,ILit)) = new ISubst { override val vars = List(vl) }
  def apply(vs:List[(IVar,ILit)], bd:Set[IVar]) =
  	new ISubst {override val vars = vs; override val bound = bd }
}