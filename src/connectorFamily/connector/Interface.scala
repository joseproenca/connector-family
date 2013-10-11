package connectorFamily.connector


/**
 * Creates an interface (left or right) of a connector.
 * For example, 1 x 2* is defined as Interface(1,-2). 
 */
class Interface {
	// only normalised interfaces (2,-1,4 ok, 2,1,-3 not ok)
  private var interf: List[Int] = List()
  
  /**
   * Gets a list representing the interface
   */
  def get = interf
  
  /**
   * Calculates this x other, and updates the current interface
   */
  def ++=(other:Interface) = (interf,other.get) match {
  	case (Nil,x) => interf = x
  	case (x,Nil) => interf = x
  	case (_, x::xs) =>
  		if (interf.last>0 == x>0)
  		  interf =  interf.init ::: List(interf.last + x) ::: xs
  		else
  			interf = interf ::: other.get
  } 
  
  /**
   * Calculates this x other 
   */
  def ++(other:Interface): Interface = { 
  	val v = new Interface()
  	v ++= this
  	v ++= other
  	v
  }
  
  override def toString(): String = interf.mkString("[",",","]")
  override def equals(other: Any) = other match {
    case that: Interface => that.get == interf
    case _ => false
  }
  override def hashCode = interf.hashCode
}


object Interface {
	def apply(a:Int*): Interface = apply(a.toList)
	
	def apply(a:Iterable[Int]): Interface = {
		if (a.isEmpty) new Interface
		else {
			val res = new Interface
			for (x <- a) {
				if (x!=0)
					res ++= new Interface { interf = List(x)}
			}
			res
		}
	}
}