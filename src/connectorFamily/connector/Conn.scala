package connectorFamily.connector

/**
 * Second attempt to create a connector class,
 * where the derivation tree for typed connectors is built.
 */
class Conn[R<:Rep[R]](val typ: ConnType, val const: List[ConnConst], val sort:R)
{
	/** Tense product of connectors */
	def *(other:Conn[R]): Conn[R] =
	  new Conn(
	      ParConn(typ,other.typ),
	      const ++ other.const,
	      sort * other.sort 
	  )
	
	/** Sequential composition of connectors */
	def &(other:Conn[R]): Conn[R] = {
	  val x1a = new InterfaceVar("x1a") // name for reference, not for id.
	  val x1b = new InterfaceVar("x1b")
	  val x2a = new InterfaceVar("x2a")
	  val x2b = new InterfaceVar("x2b")
	  val newconst = List(
			  EqConn(typ,      InterfacePair(Interface(x1a),Interface(x1b))),
			  EqConn(other.typ,InterfacePair(Interface(x2a),Interface(x2b))),
			  EqInterf(Interface(x1b),Interface(x2a))
	      )
  	  new Conn(
  		InterfacePair(Interface(x1a),Interface(x2b)),
  		const ++ other.const ++ newconst,
  		sort & other.sort
  	  )
	}
	
}

object Conn {
  def lambda[R<:Rep[R]](x: ConnVar[R] , typ: ConnType, t:Conn[R]) : Conn[R] =
    new Conn(new FamType(x,typ,t.typ), t.const, t.sort) //........
}


class ConnVar[R<:Rep[R]](name:String,typ:ConnType,sort:R) extends Conn[R](typ,List(),sort)

class FamType[R<:Rep[R]](x: ConnVar[R] , xtyp: ConnType, ttyp: ConnType) extends ConnType


class ConnType
case class InterfacePair(left:Interface,right:Interface) extends ConnType
case class ParConn(left:ConnType, right:ConnType) extends ConnType

class ConnConst
case class EqConn(t1:ConnType,t2:ConnType) extends ConnConst
case class EqInterf(t1:Interface,t2:Interface) extends ConnConst