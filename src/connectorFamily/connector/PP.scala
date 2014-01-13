package connectorFamily.connector

/**
 * Pretty printing of connectors and related concepts.
 * Also includes a pretty printing of the result of the type checker.
 */
object PP {
  def apply(c:Conn): String = c match {
    case Seq(c1,c2) => apply(c1)+" ; "+apply(c2)
    case Par(c1,c2) => apply(c1)+" * "+apply(c2)
    case App(c1,c2) => apply(c1)+" "+apply(c2)
    case AppV(c1,c2) => apply(c1)+" "+c2
    case Lambda(v,t,c) => "\\"+apply(v)+":"+apply(t)+" . "+apply(c)
    case LambdaV(v,t,c) => "\\"+apply(v)+":"+apply(t)+" . "+apply(c)
    case IndBool(vt:VVar, t:CType, ct:Conn, cf:Conn, bool:Val) =>
      "if "+apply(bool)+"["+apply(vt)+"."+apply(t)+" then "+apply(ct)+" else "+apply(cf)
    case IndNat(vt:VVar, t:CType, c0:Conn, vs:CVar, cs:Conn, nat:Val) =>
      "IndN("+apply(vt)+"."+apply(t)+","+apply(c0)+","+apply(vt)+"."+apply(vs)+"."+apply(cs)+","+apply(nat)+")"
    case CPrim(t,n) => n//+":"+apply(t)
    case v:CVar => v.name
  }
  
  def apply(t:FType): String = t match {
    case Prod(v:CVar,tpar:CType,t:FType) => "Pi["+apply(v)+":"+tpar+"] "+apply(t)
    case ProdV(v:VVar,tpar:VType,t:FType) => "Pi["+apply(v)+":"+tpar+"] "+apply(t)
    case IPair(left:Interface,right:Interface) => "("+apply(left)+","+apply(right)+")"
    case CPair(left:CType, right:CType) => apply(left) +" * "+apply(right)
  }
  
  def apply(i:Interface) : String = i.get match {
    case Nil => "ø"
    case List(lit) => apply(lit)
    case lst:List[ILit] => lst.map(apply(_)).mkString("[",",","]")
  }
  
  def apply(i:ILit): String = i match {
    case INat(n) => n.toString
    case IDual(i) => "-"+apply(i)
    case IIndBool(a,b,c) => "IndB("+apply(a)+","+apply(b)+","+apply(c)+")"
    case IIndNat(a,b,c,d,e) => "IndN("+apply(a)+","+apply(b)+"."+apply(c)+"."+apply(d)+","+apply(e)+")"
    case v:IVar => v.name
  }
  
  def apply(c:Const) : String = c match {
    case CEq(t1:CType,t2:CType) => PP(t1)+" == "+PP(t2)
	case IEq(t1:Interface,t2:Interface) => PP(t1)+" == "+PP(t2)
	case VEq(t1:VType,t2:VType) => PP(t1)+" == "+PP(t2)
	case LEq(t1:ILit,t2:ILit) => PP(t1)+" == "+PP(t2)
  }
  
  def apply(t:VType) : String = t match {
    case VBool => "Bool"
    case VNat  => "Nat"
  }
 
  def apply(v:Val): String = v match {
  	case VZero => "0"
  	case VSucc(v:Val) => apply(v)+"+1"
  	case VTrue => "true"
  	case VFalse => "false"
  	case v:VVar => v.name
  }
  
  
    /** Calculates the type and returns a string with its type and intermediate results.
     * If type checking fails an exception is raised. 
     */
  def typeAndPrint(c:Conn): String = {
	val (t,cnst,subs,newtyp) = TypeCheck(c)
	PP(c)+"\n : "+PP(t)+
		  "\n"+cnst.map(PP(_)).mkString(" | ","\n | ","")+
		  (if (!subs.toString.isEmpty) "\n"+subs else "")+
		  "\n : "+PP(newtyp)
  }

  
  /** Calculates the type and returns a string with its type and intermediate results.
   *  If type checking fails, it returns a string with the error message and intermediate results.  
   */
  def typeAndPrintWithErrors(c:Conn): String = {
    var typ_consts: (FType,List[Const]) = null
    var subs: ISubst = null
    try{
		typ_consts = TypeCheck.infer(TypeEnv(),c)
		subs = TypeCheck.unify(typ_consts._2)
		val newtype = subs(typ_consts._1)
		PP(c)+"\n : "+PP(typ_consts._1)+
		  "\n"+typ_consts._2.map(PP(_)).mkString(" | ","\n | ","")+
		  (if (!subs.toString.isEmpty) "\n"+subs else "")+
		  "\n : "+PP(newtype)
    } catch {
      case TypeException(s:String) =>
        "Failed to typecheck "+PP(c)+".\n"+s+
        (if (typ_consts != null)  
          "\n : "+PP(typ_consts._1)+
		  "\n"+typ_consts._2.map(PP(_)).mkString(" | ","\n | ","")
		 else "") +
		(if (subs!=null && !subs.toString.isEmpty) "\n"+subs else "")
      case e: Exception => throw e

    }
  }
}


