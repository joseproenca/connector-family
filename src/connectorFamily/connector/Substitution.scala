package connectorFamily.connector

/** a substitution from vars (VVar or CVar) to values (Val or Conn) */
class Substitution {
  private val vals: Map[VVar,Val] = Map()
  private val cons: Map[CVar,Conn] = Map()
  
  def -(v:VVar) = {
    val vs = vals
    val cs = cons
    new Substitution{ val vals = vs-v; val cons = cs }
  }
  def -(v:CVar) = {
    val vs = vals
    val cs = cons
    new Substitution{ val vals = vs; val cons = cs-v }
  }
  
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
    new Interface{ val interf = i.get.map(apply(_)) }
  
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
//  def apply(vs:(VVar,Val))  = new Substitution { val vals = Map(vs) }
//  def apply(vs:(CVar,Conn)) = new Substitution { val cons = Map(vs) }
  
  // avoiding type lost by erasure...
  def apply(pair:(_,_)) = pair._1 match {
    case v1:CVar => pair._2 match {
      case v2:Conn => new Substitution { val cons = Map[CVar,Conn](v1->v2) }
    }
    case v1:VVar => pair._2 match {
      case v2:Val => new Substitution { val vars = Map[VVar,Val](v1->v2) }
    }
  }
  
}