package connectorFamily.connector

object TypeCheck {
  def apply(c:Conn) = {
  	val (typ,consts) = infer(TypeEnv(),c)
  	val subs = unify(consts)
  	val newtype = subs(typ)
  	(newtype,consts)
  }
  
  def infer(env:TypeEnv, c:Conn) : (FType,List[Const]) = c match {
    case Par(c1,c2) =>
      val (t1,ct1) = infer(env,c1)
      val (t2,ct2) = infer(env,c2)
      (t1,t2) match {
        case (tt1:CType,tt2:CType) => (CPair(tt1,tt2), ct1 ++ ct2)
        case _ => throw new TypeException("Tense product only defined for connectors - "+(t1,t2)) 
      }       
    case Seq(c1,c2) =>
      val (t1,ct1) = infer(env,c1)
      val (t2,ct2) = infer(env,c2)
      (t1,t2) match {
        case (tt1:CType,tt2:CType) =>
        	val (x1a,x1b) = (new IVar("x1a"),new IVar("x1b"))
        	val (x2a,x2b) = (new IVar("x2a"),new IVar("x2b"))
        	(IPair(x1a,x2b), ct1 ++ ct2 ++
    		  List(tt1 === IPair(x1a,x1b), tt2 === IPair(x2a,x2b), x1b === x2a))
        case _ => throw new TypeException("Sequence only defined for connectors - "+(t1,t2))
      }
    case Lambda(v,tpar,c) =>
      val (t,ct) = infer(env + (v->tpar), c)
      (Prod(v,tpar,t),ct)
    case LambdaV(v,tpar,c) =>
      val (t,ct) = infer(env + (v->tpar), c)
      (ProdV(v,tpar,t),ct)
    case App(c1,c2) =>
      val (t1,ct1) = infer(env,c1)
      val (t2,ct2) = infer(env,c2) // NOTE: Maybe t2 must be a FType. If it is a nat - next case.
      (t1,t2) match {
        // NOTE: application only of connector parameters (no family nor value parameters)
        case (Prod(vv,ttp2:CType,tt2) , ttt2: CType) => (tt2, ct1 ++ ct2 ++ List(ttt2 === ttp2))
        case _ => throw new TypeException("Application expected a connector as parameter - "+(t1,t2)) 
      }       
    case AppV(c,v) =>
      val (t1,ct1) = infer(env,c)
      val vt2 = infer(env,v) 
      t1 match {
        // NOTE: application only of connector parameters (no family nor value parameters)
        case ProdV(vv,ttp1:VType,tt1) => (tt1, ct1 ++ List(ttp1 === vt2))
        case _ => throw new TypeException("Application expected a value as parameter - "+(t1,vt2)) 
      }       
    case IndBool(vt, t, ct, cf, bool) =>
      if (!isFamily(env + (vt->VBool),t))
        throw new TypeException("Type must be a family (not a connector) - "+t)
      val vb = infer(env,bool)
      val (ttrue,ctrue) = infer(env,ct)
      val (tfalse,cfalse) = infer(env,cf)
      (ttrue,tfalse,vb) match {
      	case (ttrue2:CType,tfalse2:CType,VBool) =>
      	  val c1 = ttrue2  === Substitution(vt -> VTrue)(t)
      	  val c2 = tfalse2 === Substitution(vt -> VFalse)(t)
      	  (Substitution(vt -> bool)(t) , ctrue ++ cfalse ++ List(c1,c2) )
      	case _ => throw new TypeException("IndBool expected 2 connectors and a boolean. Instead "+ttrue+"/"+tfalse+"/"+vb+".")
      }
    //case class IndNat(vt:VVar, t:CType, c0:Conn, vs:CVar, cs:Conn, nat:Val) extends Conn
    case IndNat(vt, t, c0, vs, cs, nat) =>
      if (!isFamily(env + (vt->VNat),t))
        throw new TypeException("Type must be a family (not a connector) - "+t)
      val vn = infer(env,nat)
      val (t0,cnst0) = infer(env,c0)
      val (ts,cnsts) = infer(env + (vt->VNat) + (vs->t),cs)
      (t0,ts,vn) match {
        // NOTE: terms for 0 and succ cannot be families...
      	case (t02:CType,ts2:CType,VNat) =>
      	  val c1 = t02 === Substitution(vt -> VZero)(t)
      	  val c2 = ts2 === Substitution(vt -> VSucc(vt))(t)
      	  (Substitution(vt -> nat)(t) , cnst0 ++ cnsts ++ List(c1,c2) )
      	case _ => throw new TypeException("IndNat expected 2 connectors and a nat. Instead "+t0+"/"+ts+"/"+vn+".")
      }
    case CPrim(t,name) => (t,List())
    case v:CVar =>
      if (env contains v)
        (env(v),List())
      else
        throw new TypeException("Variable not in type environment - "+v)
  }
  
  def infer(env:TypeEnv, v:Val) : VType = v match {
  	case VZero    => VNat
  	case VSucc(n) => infer(env,n) match {
  	  case VNat => VNat
  	  case t => throw new TypeException("Expected VNat, got "+n+" : "+t)
  	}
  	case VTrue => VBool
  	case VFalse => VBool
  	case vv : VVar => // it is a VVar(name:String)
  	  if (env contains vv) env(vv)
  	  else throw new TypeException("Untyped variable "+vv)  		
  }
  
  def isFamily(env:TypeEnv, t:FType) : Boolean = t match {
  	case _:Prod => true 
  	case _:ProdV => true
  	case _ => false
  } 
  
  
  /** Unify constraints.
   *  If it compares connector types: EVALUATE (yielding a pair of evaluated interfaces).
   *  If it compares interfaces: split into interface literals
   *  If it compares literals: follow algorithm
   *  If it compares value types: check equality
   *  If it compares values (not yet): updated algorithm
   */
  def unify(cs: List[Const]): ISubst = cs match {
  	case Nil => new ISubst
  	case CEq(t1,t2)::rest =>
			val (i1,i2) = evaluate(t1)
			val (i3,i4) = evaluate(t2)
			unify(IEq(i1,i3)::IEq(i2,i4)::rest)
		case IEq(i1,i2)::rest =>
			val l1 = i1.get
			val l2 = i2.get
			if (l1.size != l2.size)
				throw new TypeException("Failed to unify interfaces "+i1+" - "+i2)
			unify((l1,l2).zipped.map(LEq(_,_)) ++ rest)
		//now the algorithm for literals
		case LEq(lit1,lit2)::rest if lit1 == lit2 => unify(rest)
		case LEq(x:IVar,lit2)::rest /*if lit2 contains x*/ => 
			val subst = ISubst(x->lit2)
			unify(rest.map(subst(_))) ++ ISubst(x->lit2) // ORDER is important.				
    case LEq(lit2,x:IVar)::rest /*if lit2 contains x*/ => 
      val subst = ISubst(x->lit2)
      unify(rest.map(subst(_))) ++ ISubst(x->lit2) // ORDER is important. 
    case LEq(IIndBool(t1,f1,b1),IIndBool(t2,f2,b2))::rest if b1==b2 => // LATER: unify values!
       unify(IEq(t1,t2)::IEq(f1,f2)::rest)
    case LEq(IIndNat(iZ1,vv1,iv1,iS1,n1),IIndNat(iZ2,vv2,iv2,iS2,n2))::rest if n1==n2 => // LATER: unify values!
       unify(IEq(iZ1,iZ2)::IEq(iS1,ISubst(iv2->iv1)(Substitution(vv2->vv1)(iS2)))::rest)
    case x::_ => throw new TypeException("Failed to unify constraints "+x)
  }
  
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
    	val subs = Substitution(vv->n)++
    	           Substitution(iv->IIndNat(iZero,vv,iv,iSucc,n))
    	evaluate(subs(iSucc)++Interface(rest))
    //case (v:Var)::rest =>
    case _ => throw new TypeException("Could not evaluate interface "+i)
  }
}

/**
 * Constraint collected during type checking
 */
sealed abstract class Const
case class CEq(t1:CType,t2:CType) extends Const
case class IEq(t1:Interface,t2:Interface) extends Const
case class VEq(t1:VType,t2:VType) extends Const
case class LEq(t1:ILit,t2:ILit) extends Const


/**
 * Maps variables to connectors, values, or interfaces.
 */
class TypeEnv (cvars:Map[CVar,CType], vvars: Map[VVar,VType], ivars: Map[IVar,Interface]) {
//class TypeEnv {
//  val cvars: scala.collection.immutable.Map[CVar,CType] = scala.collection.immutable.Map() 
//  val vvars: scala.collection.immutable.Map[VVar,VType] = scala.collection.immutable.Map() 
//  val ivars: scala.collection.immutable.Map[IVar,Interface] = scala.collection.immutable.Map()
  
//  def update(v:CVar,e:CType) = cvars.update(v,e)
//  def update(v:VVar,e:VType) = vvars.update(v,e)
//  def update(v:IVar,e:Interface) = ivars.update(v,e)
  
  def +(pair:(_,_)) = pair._1 match {
    case v1:CVar => pair._2 match {
    case v2:CType => new TypeEnv(cvars+(v1->v2),vvars,ivars) 
    }
    case v1:VVar => pair._2 match {
    case v2:VType => new TypeEnv(cvars,vvars+(v1->v2),ivars)    	
    }
    case v1:IVar => pair._2 match {
  	case v2:Interface => new TypeEnv(cvars,vvars,ivars+(v1->v2))
    }
  }
      
//  def +(CVar,CType)) = new TypeEnv{ cvars = this.cvars + ve; vvars = this.vvars; ivars = this.ivars }
//  def +(ve:(VVar,VType)) = new TypeEnv{ cvars = this.cvars; vvars = this.vvars + ve; ivars = this.ivars }
//  def +(ve:(IVar,Interface)) = new TypeEnv{ cvars = this.cvars; vvars = this.vvars; ivars = this.ivars + ve }
  
  def apply(v:CVar) = cvars(v) 
  def apply(v:VVar) = vvars(v) 
  def apply(v:IVar) = ivars(v)
  
  def contains(v:CVar) = cvars.contains(v)
  def contains(v:VVar) = vvars.contains(v)
  def contains(v:IVar) = ivars.contains(v)
}

object TypeEnv {
	def apply(): TypeEnv = new TypeEnv(Map(),Map(),Map())
//	def apply(cv:Map[CVar,CType], vv: Map[VVar,VType], iv: Map[IVar,Interface]) =
//		new TypeEnv{ override val cvars = cv; override val vvars = vv; override val ivars = iv }
}


class TypeException(msg:String) extends Exception(msg)