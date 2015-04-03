package connectorFamily.connector

object TypeCheck {
  /**
   * Type check with debug information:
   */
  def apply(c:Conn) = {
  	val (typ,consts) = infer(TypeEnv(),c)
//  	println("GOT TYPE+CONST\n"+PP(typ)+"\n - "+consts.map(PP(_)).mkString("\n - "))
  	val subs = unify(consts)
//  	println("GOT SUBS\n"+subs)
  	val newtype = subs(typ)
  	(typ,consts,subs,newtype)
  }
  
  /** Infer the type and type-constraints, without unification. */
  def infer(env:TypeEnv, c:Conn) : (FType,List[Const]) = 
    infer(env,c,new ErrorStack)
  
  private def infer(env:TypeEnv, c:Conn, err:ErrorStack) : (FType,List[Const]) = {
    err += PP(c)
    c match {
  
    case Par(c1,c2) =>
      val (t1,ct1) = infer(env,c1,err)
      val (t2,ct2) = infer(env,c2,err)
      (t1,t2) match {
        case (tt1:CType,tt2:CType) => (CPair(tt1,tt2), ct1 ++ ct2)
        case (tt1:FType,tt2:FType) =>
          (fpar(tt1,tt2),ct1++ct2)
//          val x = new CTVar("X-fpar")
//          (x, ct1 ++ ct2 ++ List(x === fpar(tt1,tt2)) // need to add fpar, const+unification of FTypes
        case _ => err.inference.error(s"Tense product undefined for given types - ${PP(t1)} * ${PP(t2)}") 
      }       
    case Seq(c1,c2) =>
      val (t1,ct1) = infer(env,c1,err)
      val (t2,ct2) = infer(env,c2,err)
      (t1,t2) match {
        case (tt1:CType,tt2:CType) =>
        	val (x1a,x1b) = (new IVar("x1a"),new IVar("x1b"))
        	val (x2a,x2b) = (new IVar("x2a"),new IVar("x2b"))
        	(IPair(x1a,x2b), ct1 ++ ct2 ++
    		  List(tt1 === IPair(x1a,x1b), tt2 === IPair(x2a,x2b), x1b === x2a))
        case (tt1:FType,tt2:FType) =>
//          println(s"!!! ${PP(right(tt1))} == ${PP(left(tt2))}")
    	  (fseq(tt1,tt2),ct1++ct2++List(right(tt1) === left(tt2)))
        case _ => err.inference.error(s"Sequence undifined for given types - ${PP(t1)} ; ${PP(t2)}")
      }
    case Lambda(v,tpar,c) =>
      val (t,ct) = infer(env + (v->tpar), c,err)
      (Prod(v,tpar,t),ct)
    case LambdaV(v,tpar,c) =>
      val (t,ct) = infer(env + (v->tpar), c,err)
      (ProdV(v,tpar,t),ct)
    case App(c1,c2) =>
      val (t1,ct1) = infer(env,c1,err)
      val (t2,ct2) = infer(env,c2,err) // NOTE: Maybe t2 must be a FType. If it is a nat - next case.
      (t1,t2) match {
        // NOTE: application only of connector parameters (no family nor value parameters)
        case (Prod(vv,ttp2:CType,tt2) , ttt2: CType) => (tt2, ct1 ++ ct2 ++ List(ttt2 === ttp2))
        case _ => err.inference.error(s"Application expected a connector as parameter - ${PP(t1)} == ${PP(t2)}") 
      }       
    case AppV(c,v) =>
      val (t1,ct1) = infer(env,c,err)
      val vt2 = infer(env,v,err) 
      t1 match {
        // NOTE: application only of connector parameters (no family nor value parameters)
        case ProdV(vv,ttp1:VType,tt1) => (Substitution(vv->v)(tt1), ct1 ++ List(ttp1 === vt2))
        case _ => err.inference.error(s"Application expected a value as parameter - ${PP(t1)} == ${PP(vt2)}") 
      }       
    case IndBool(vt, t, ct, cf, bool) =>
      if (!isFamily(env + (vt->VBool),t))
        err.inference.error("Type must be a family (not a connector) - "+t)
      val vb = infer(env,bool,err)
      val (ttrue,ctrue) = infer(env,ct,err)
      val (tfalse,cfalse) = infer(env,cf,err)
      (ttrue,tfalse,vb) match {
      	case (ttrue2:CType,tfalse2:CType,VBool) =>
      	  val c1 = ttrue2  === Substitution(vt -> VTrue)(t)
      	  val c2 = tfalse2 === Substitution(vt -> VFalse)(t)
      	  (Substitution(vt -> bool)(t) , ctrue ++ cfalse ++ List(c1,c2) )
      	case _ => err.inference.error("IndBool expected 2 connectors and a boolean. Instead "+
      	    PP(ttrue)+"/"+PP(tfalse)+"/"+PP(vb)+".")
      }
    //case class IndNat(vt:VVar, t:CType, c0:Conn, vs:CVar, cs:Conn, nat:Val) extends Conn
    case IndNat(vt, t, c0, vs, cs, nat) =>
//      if (!isFamily(env + (vt->VNat),t))
//        throw new TypeException("Type must be a family (not a connector) - "+t)
      val vn = infer(env,nat,err)
      val (t0,cnst0) = infer(env,c0,err)
      val (ts,cnsts) = infer(env + (vt->VNat) + (vs->t),cs,err)
      (t0,ts,vn) match {
        // NOTE: terms for 0 and succ cannot be families...
      	case (t02:CType,ts2:CType,VNat) =>
//      	  println("replacing "+PP(vt)+" by zero and succ(.).")
      	  val c1 = t02 === Substitution(vt -> VZero)(t)
      	  val c2 = ts2 === Substitution(vt -> VSucc(vt))(t)
//      	  println("See result:\n"+PP(c1)+"\n"+PP(c2))
      	  (Substitution(vt -> nat)(t) , cnst0 ++ cnsts ++ List(c1,c2) )
      	case (t02,_:CType,VNat) => err.inference.error(s"IndNat expected a connector instead of ${PP(c0)}:${PP(t0)}.")
      	case (_,ts2,VNat)       => err.inference.error(s"IndNat expected a connector instead of ${PP(cs)}:${PP(ts2)}.")
      	case (_,_,_)            => err.inference.error(s"IndNat expected a Nat instead of ${PP(nat)}:${PP(vn)}.")
      }
    case CPrim(t,name) => (t,List())
    case v:CVar =>
      if (env contains v)
        (env(v),List())
      else
        err.inference.error("Variable not in type environment - "+v)
  }}
  
  private def infer(env:TypeEnv, v:Val, err:ErrorStack) : VType = {
    err += PP(v)
    v match {
      
  	case VZero    => VNat
  	case VSucc(n) => infer(env,n,err) match {
  	  case VNat => VNat
  	  case t => err.inference.error("Expected VNat, got "+n+" : "+t)
  	}
  	case VTrue => VBool
  	case VFalse => VBool
  	case vv : VVar => // it is a VVar(name:String)
  	  if (env contains vv) env(vv)
  	  else err.inference.error("Untyped variable "+vv)  		
  }}
  
  private def isFamily(env:TypeEnv, t:FType) : Boolean = t match {
  	case _:Prod => true 
  	case _:ProdV => true
  	case _ => false
  } 
  
  private def fpar(t1:FType,t2:FType): FType = (t1,t2) match {
    case (Prod(a,b,c),_) => Prod(a,b,fpar(c,t2)) 
    case (ProdV(a,b,c),_) => ProdV(a,b,fpar(c,t2))
    case (_,Prod(a,b,c)) => Prod(a,b,fpar(c,t1))
    case (_,ProdV(a,b,c)) => ProdV(a,b,fpar(c,t1))
    case (a:CType,b:CType) => CPair(a,b)
  }
  private def fseq(t1:FType,t2:FType): FType = (t1,t2) match {
    case (Prod(a,b,c),_) => Prod(a,b,fseq(c,t2)) 
    case (ProdV(a,b,c),_) => ProdV(a,b,fseq(c,t2))
    case (_,Prod(a,b,c)) => Prod(a,b,fseq(c,t1))
    case (_,ProdV(a,b,c)) => ProdV(a,b,fseq(c,t1))
    case (a:CType,b:CType) => IPair(right(a),left(b))
  }
  private def left(t:FType): Interface = t match {
    case IPair(l,_)   => l
    case CPair(l,r)   => left(l) ++ left(r)
    case Prod(_,_,t)  => left(t)
    case ProdV(_,_,t) => left(t)
  }
  private def right(t:FType): Interface = t match {
    case IPair(_,r)   => r
    case CPair(l,r)   => right(l) ++ right(r)
    case Prod(_,_,t)  => right(t)
    case ProdV(_,_,t) => right(t)
  }
  
  /** Unify constraints */
  def unify(cs: List[Const]): ISubst = unify(cs,new ErrorStack)
  
  /** Unify constraints.
   *  If it compares connector types: EVALUATE (yielding a pair of evaluated interfaces).
   *  If it compares interfaces: split into interface literals
   *  If it compares literals: follow algorithm
   *  If it compares value types: check equality
   *  If it compares values (not yet): updated algorithm
   */
  private def unify(cs: List[Const], err: ErrorStack): ISubst = {
    if (!cs.isEmpty) err += PP(cs.head)
    cs match {
 
  	case Nil => new ISubst
  	//-- For connector types --
  	case CEq(t1,t2)::rest =>
		val (i1,i2) = Eval.evaluate(t1)
//		println("--- evaluated t1 - "+(PP(i1),PP(i2)))
		val (i3,i4) = Eval.evaluate(t2)
//		println("--- evaluated t2 - "+(PP(i3),PP(i4)))
		unify(IEq(i1,i3)::IEq(i2,i4)::rest,err)
//  	case FEq(t1:CType,t2:CType)::rest => unify(CEq(t1,t2)::rest,err)
//  	case FEq(t1:CType,t2:CType)::rest => err.unification.error("not yet...")
  	  // val p1 = Eval.evaluate(t1)
  	  // val p2 = Eval.evaluate(t2)
  	  
	//-- For interfaces --
	case IEq(i1,i2)::rest =>
//		println("--- "+PP(i1)+" <--> "+PP(i2))
		val l1 = i1.get
		val l2 = i2.get
		if (l1.size != l2.size) { // PROBLEM: ad-hoc solution (how to unify [x1 x2] with [1 -2 2]?)
		  if (l1.size == 1 && l1.head.isInstanceOf[IVar]) {
		      val subst = ISubst(l1.head.asInstanceOf[IVar]->i2)
		      unify(rest.map(subst(_)),err) ++ subst // ORDER is important.		
		  }
		  else if (l2.size == 1 && l2.head.isInstanceOf[IVar]) {
		      val subst = ISubst(l2.head.asInstanceOf[IVar]->i1)
		      unify(rest.map(subst(_)),err) ++ subst // ORDER is important.		
		  }
		  else
			  err.unification.error("Failed to unify interfaces.")
		}
		else
		  unify((l1,l2).zipped.map(LEq(_,_)) ++ rest,err)
	//-- For literals --
	case LEq(lit1,lit2)::rest if lit1 == lit2 => unify(rest,err)
	case LEq(x:IVar,lit2)::rest /*if lit2 contains x*/ => 
		val subst = ISubst(x->lit2)
		unify(rest.map(subst(_)),err) ++ subst // ORDER is important.				
    case LEq(lit2,x:IVar)::rest /*if lit2 contains x*/ => 
      val subst = ISubst(x->lit2)
      unify(rest.map(subst(_)),err) ++ subst // ORDER is important. 
    case LEq(IIndBool(t1,f1,b1),IIndBool(t2,f2,b2))::rest if b1==b2 => // LATER: unify values!
       unify(IEq(t1,t2)::IEq(f1,f2)::rest,err)
    case LEq(IIndNat(iZ1,vv1,iv1,iS1,n1),IIndNat(iZ2,vv2,iv2,iS2,n2))::rest if n1==n2 => // LATER: unify values!
       unify(IEq(iZ1,iZ2)::
         IEq(iS1,ISubst(iv2->iv1)(Substitution(vv2->vv1)(iS2)))::
         IEq(iS2,ISubst(iv1->iv2)(Substitution(vv1->vv2)(iS1)))::rest
            ,err)
    case LEq(IIndNat(iZ1,vv1,iv1,iS1,n1),IIndNat(iZ2,vv2,iv2,iS2,n2))::rest => // UNIFYING values
       unify(IEq(iZ1,iZ2)::
             IEq(iS1,ISubst(iv2->iv1)(Substitution(vv2->vv1)(iS2)))::
             IEq(iS2,ISubst(iv1->iv2)(Substitution(vv1->vv2)(iS1)))::
             VLEq(n1,n2)::rest
            ,err)
	//-- For value types --
	case VEq(vt1,vt2)::rest if vt1 == vt2 => unify(rest,err)
    //-- For values --
	case VLEq(t1,t2)::rest if t1 == t2 => unify(rest,err)
	case VLEq(x:VVar,t2)::rest =>
	  val subst = ISubst(x->t2)
	  unify(rest.map(subst(_)),err) ++ subst // ORDER is important.
	case VLEq(t1,x:VVar)::rest =>
	  val subst = ISubst(x->t1)
	  unify(rest.map(subst(_)),err) ++ subst // ORDER is important.
	case VLEq(VSucc(v1),VSucc(v2))::rest =>
	  unify(VLEq(v1,v2)::rest,err)	
	//-- Fail otherwise --
    case x::_ => err.unification.error("Failed to unify constraints") //+PP(x))
  }}
}

/**
 * Constraint collected during type checking
 */
sealed abstract class Const
case class CEq(t1:CType,t2:CType) extends Const
//case class FEq(t1:FType,t2:FType) extends Const
case class IEq(t1:Interface,t2:Interface) extends Const
case class LEq(t1:ILit,t2:ILit) extends Const
case class VEq(t1:VType,t2:VType) extends Const
case class VLEq(t1:Val,t2:Val) extends Const


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


class ErrorStack {
  var stack: List[String] = Nil
  def clear = stack = Nil
  def +=(s:String) = stack ::= s //{println(s"updating stack with $s -- new: ${stack.mkString("/")}"); stack ::= s}
  def error(msg:String) = throw TypeException(msg+"\n"+stack.mkString("\n"))
  def error = throw TypeException(stack.mkString("\n"))
  def unification = {stack = stack.map("unifying: "+_); this}
  def inference   = {stack = stack.map("infering: "+_); this}
}

case class TypeException(msg:String) extends Exception(msg)