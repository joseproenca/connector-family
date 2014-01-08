package connectorFamily.connector


/**
 * Creates an interface (left or right) of a connector.
 * For example, 1 x 2* x 1* is defined as Interface(1,-2,-1).
 * Supports: dual, parallel, no induction yet.
 */
class Interface {
//	// only normalised interfaces (2,-1,4 ok, 2,1,-3 not ok)
//  private var interf: List[Int] = List()

  private var interf: List[ILit] = List()
  
  /**
   * Gets a list representing the interface
   */
  def get = interf
  
  /**
   * Calculates this x other. Assumes both are normalised. 
   */
  def ++(other:Interface): Interface = (interf,other.get) match { 
    case (Nil,x) => new Interface{interf = x}
    case (x,Nil) => new Interface{interf = x}
    case (i, x::xs) =>
      val mrg = i.last ++ x
      if (mrg.isDefined)
        new Interface{interf = i.init ::: List(mrg.get) ::: xs}
      else
        new Interface{interf = i::: other.get}
//      if (i.last>0 == x>0)
//        new Interface{interf = i.init ::: List(i.last + x) ::: xs}
//      else
//        new Interface{interf = i::: other.get}
  } 
  
  /**
   * Inverts an interface.
   */
  def inv: Interface = {
  	val i = interf
  	new Interface{
  		interf =  //i.map(_*(-1))
  		  for (lit <- i)
  		    yield lit match {
  		    	case IDual(id) => id
  		    	case _ => IDual(lit)
  		  	}
  	}
  }

  
  override def toString(): String = interf.mkString("[",",","]")
  override def equals(other: Any) = other match {
    case that: Interface => that.get == interf
    case _ => false
  }
  override def hashCode = interf.hashCode

  def ===(i:Interface) = IEq(this,i) 
}

/**
 * Used to create an interface, after normalising the list.
 * E.g., [0,1,2,-4,3,2] becomes [3,-4,5] (which means 3 x 4* x 5 in the paper).
 */
object Interface {
	def apply(a:ILit*): Interface = apply(a.toList)
	
	def apply(a:Iterable[ILit]): Interface = {
		if (a.isEmpty) new Interface
		else {
			var res = new Interface
			for (lit <- a) {
				if (lit != INat(0))
					res ++= new Interface { interf = List(lit)}
			}
			res
		}
	}
	
	implicit def ilit2interface(lit:ILit) : Interface =  new Interface { interf = List(lit)}
}



sealed abstract class ILit {
  /** Tries to combine 2 constant interface literals */
  def ++(other:ILit) : Option[ILit] = None
  def ===(i:ILit) = IEq(Interface.ilit2interface(this),Interface.ilit2interface(i)) 

}
case class INat(n:Int) extends ILit // POSITIVE!
{   override def toString: String = n.toString
	override def ++(other:ILit) = other match {
		case INat(i2) => Some(INat(n+i2))
		case _ => None
	}
}
case class IDual(i:ILit) extends ILit
{   override def toString: String = i.toString+"*"
	override def ++(other:ILit) = other match {
		case IDual(i2) => (i++i2).map(IDual(_))
		case _ => None
	}
}
case class IIndBool(ifTrue:Interface, ifFalse:Interface, b:Val) extends ILit
	{ override def toString: String = ifTrue+" <"+b+"> "+ifFalse }
case class IIndNat(ifZero:Interface, pred:VVar, predT: IVar, ifSucc:Interface, n:Val) extends ILit
	{ override def toString: String = "Ind("+ifZero+","+pred+"."+predT+"."+ifSucc+","+n+")" }
     class IVar(val name:String) extends ILit
	{ override def toString: String = name }	






