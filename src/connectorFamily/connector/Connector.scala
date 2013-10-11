package connectorFamily.connector

class Connector[R<:CRep[R]](val from: Interface, val to: Interface, val sort: R) {
  
	/**
	 * Tense product of connectors
	 */
	def *(other:Connector[R]): Connector[R] =
		new Connector(
				from ++ other.from,
				to ++ other.to,
				sort * other.sort 
				)
	
	/**
	 * Sequential composition of connectors 
	 */
	def -(other:Connector[R]): Connector[R] =
		if (matches(other))
		  new Connector(
		      from,
		      other.to,
		      sort - other.sort
		      )
		else
			throw new RuntimeException("Connectors are not compatible")

	/**
	 * Checks if the composition is valid.
	 */
	def matches(other:Connector[R]) =
		to == other.from
		
	/**
	 * Inverts the direction of a connector
	 */
	def inv: Connector[R] =
		new Connector(to.inv, from.inv, sort.inv)
		
	override def toString: String =
			from.toString ++ sort.toString ++ to.toString
		
}

abstract class CRep[R<:CRep[R]] {
  def *(other:R): R
  def -(other:R): R
  def inv: R
}

case class SimpleRep(val name:String) extends CRep[SimpleRep] {
  def *(other:SimpleRep): SimpleRep = SimpleRep(name ++ "*" ++ other.name) 
  def -(other:SimpleRep): SimpleRep = SimpleRep(name ++ "-" ++ other.name) 
  def inv: SimpleRep = SimpleRep(name ++ "'") 
  override def toString = name 
}