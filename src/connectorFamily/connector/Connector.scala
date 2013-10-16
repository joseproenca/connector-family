package connectorFamily.connector

class Connector[R<:Rep[R]]
(val from: Interface, val to: Interface, val sort: R) {
//extends Context[R](){

/****
PRECEDENCE of infix operators:
-----
letter other
|
^
&
! =?
< >
:?
+ -
* / %
? \
****/
	
	
	/** Tense product of connectors */
	def *(other:Connector[R]): Connector[R] =
		new Connector(
				from ++ other.from,
				to ++ other.to,
				sort * other.sort 
				)
	
	/** Sequential composition of connectors */
	def &(other:Connector[R]): Connector[R] =
		if (matches(other))
		  new Connector(
		      from,
		      other.to,
		      sort & other.sort
		      )
		else
			throw new RuntimeException("Connectors are not compatible:\n - "+
					this+"\n - "+other)
	
	/** Choice of connectors */
	def +(other:Connector[R]): Connector[R] =
		if (from == other.from && to == other.to)
			new Connector(from,to,sort + sort)
		else
      throw new RuntimeException("Connectors have different signatures:\n - "+
          this+"\n - "+other)

	/** Checks if the composition is valid. */
	def matches(other:Connector[R]) =
		to == other.from
		
	/**
	 * Inverts the direction of a connector
	 */
	def inv: Connector[R] =
		new Connector(to.inv, from.inv, sort.inv)
		
	override def toString: String =
//      from.toString ++ sort.toString ++ to.toString
      sort.toString ++ ": " ++ from.toString ++ " -> " ++ to.toString
		
}

/** The objects being composed.
 * Could be strings, reo connectors, open petri nets, etc. 
 */
abstract class Rep[R<:Rep[R]] {
	/** Product */
  def *(other:R): R
  /** Sequential composition */
  def &(other:R): R
  /** Choice */
  def +(other:R): R
  /** Inverse */
  def inv: R
}

class NullRep extends Rep[NullRep] {
	def *(other:NullRep) = other
  def &(other:NullRep) = other
  def +(other:NullRep) = other
  def inv = this
}

/** Connector with references to the context and hole used to produce it. */
class ConnectorCtx[R<:Rep[R]](from: Interface,to: Interface,sort: R,
		                          val ctx: Context[R], val hole:Connector[R])
  extends Connector[R](from,to,sort)

// needs: to create a new connector from a given one, and
//        to recover the context from an instantiated connector
/** Context: given a connector returns a new connector.
 *  
 *  Automatically creates references to this context and the hole
 *  when applied to a connector.
 */
class Context[R<:Rep[R]] (f: Connector[R] => Connector[R]) {
	def apply(arg:Connector[R]) = {
		val nc = f(arg)
		new ConnectorCtx(nc.from,nc.to,nc.sort,this,arg)
	} 
}


//class ConnectorCtxs[R<:Rep[R]](from: Interface,to: Interface,sort: R,
//                               val ctx: Contexts[R], val holes:List[Connector[R]])
//  extends Connector[R](from,to,sort)
//
//class Contexts[R<:Rep[R]](f: List[Connector[R]] => Connector[R]) {
////extends Context[R]((con:Connector[R]) => f(List(con))){
//	def apply(args:List[Connector[R]]) = {
//		val nc = f(args)
//		new ConnectorCtxs(nc.from,nc.to,nc.sort,this,args)
//	}
//}
