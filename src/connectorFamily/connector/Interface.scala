package connectorFamily.connector


/**
 * Creates an interface (left or right) of a connector.
 * For example, 1 x 2* x 1* is defined as Interface(1,-2,-1).
 * Supports: dual, parallel, no induction yet.
 */
class Interface {
//	// only normalised interfaces (2,-1,4 ok, 2,1,-3 not ok)
//  private var interf: List[Int] = List()

  private var interf: List[InterfaceLit] = List()
  
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
  		    	case InterfaceDual(id) => id
  		    	case _ => InterfaceDual(lit)
  		  	}
  	}
  }

  
  override def toString(): String = interf.mkString("[",",","]")
  override def equals(other: Any) = other match {
    case that: Interface => that.get == interf
    case _ => false
  }
  override def hashCode = interf.hashCode
}

/**
 * Used to create an interface, after normalising the list.
 * E.g., [0,1,2,-4,3,2] becomes [3,-4,5] (which means 3 x 4* x 5 in the paper).
 */
object Interface {
	def apply(a:InterfaceLit*): Interface = apply(a.toList)
	
	def apply(a:Iterable[InterfaceLit]): Interface = {
		if (a.isEmpty) new Interface
		else {
			var res = new Interface
			for (x <- a) {
				if (x!=0)
					res ++= new Interface { interf = List(x)}
			}
			res
		}
	}
}




class InterfaceLit {
  /** Tries to combine 2 constant interface literals */
  def ++(other:InterfaceLit) : Option[InterfaceLit] = None
}
case class InterfaceInt(i:Int) extends InterfaceLit // POSITIVE!
{   override def toString: String = i.toString
	override def ++(other:InterfaceLit) = other match {
		case InterfaceInt(i2) => Some(InterfaceInt(i+i2))
		case _ => None
	}
}
case class InterfaceDual(i:InterfaceLit) extends InterfaceLit
{   override def toString: String = i.toString+"*"
	override def ++(other:InterfaceLit) = other match {
		case InterfaceDual(i2) => (i++i2).map(InterfaceDual(_))
		case _ => None
	}
}
case class InterfaceIndBool(ifTrue:Interface, ifFalse:Interface, b:BoolTerm) extends InterfaceLit
	{ override def toString: String = ifTrue+" <"+b+"> "+ifFalse }
case class InterfaceIndNat(ifZero:Interface, ifSucc:Interface,
		pred:NatVar, predT: InterfaceVar, n:NatTerm) extends InterfaceLit
	{ override def toString: String = "Ind("+ifZero+","+pred+"."+predT+"."+ifSucc+","+n+")" }

class BoolTerm
class BoolVar(name:String) extends BoolTerm
	{ override def toString: String = name }	
object TrueTerm extends BoolTerm
	{ override def toString: String = "True" }	
object FalseTerm extends BoolTerm
	{ override def toString: String = "False" }	

class NatTerm
class NatVar(name:String) extends NatTerm
	{ override def toString: String = name }	
class NatVal(n:Int) extends NatTerm
	{ override def toString: String = n.toString }	

class InterfaceVar(name:String) extends InterfaceLit
	{ override def toString: String = name }	
