package connectorFamily.connector

/**
 * Second attempt to create a connector class,
 * where the derivation tree for typed connectors is built.
 */

/** Connector definition */
sealed abstract class Conn {
  def &(other:Conn) = Seq(this,other)
  def *(other:Conn) = Par(this,other)
}
case class Seq(c1:Conn,c2:Conn) extends Conn
case class Par(c1:Conn,c2:Conn) extends Conn
case class Lambda(v:CVar,  t:CType, c:Conn) extends Conn
case class LambdaV(v:VVar, t:VType, c:Conn) extends Conn
case class App(c1:Conn,c2:Conn) extends Conn
case class AppV(c1:Conn,c2:Val) extends Conn
case class IndBool(vt:VVar, t:CType, ct:Conn, cf:Conn, bool:Val) extends Conn
case class IndNat(vt:VVar, t:CType, c0:Conn, vs:CVar, cs:Conn, nat:Val) extends Conn
case class CPrim(t:CType,name:String) extends Conn {
    def inv:CPrim = CPrim(t.inv,name+"'")
}
class CVar(val name:String) extends Conn

/** Connector Type: either a pair of interfaces or ... */
sealed abstract class CType extends FType { 
  def ===(c:CType) = CEq(this,c)
  def inv:CType = this match {
    case IPair(l,r) => IPair(r.inv,l.inv)
    case CPair(l,r) => CPair(l.inv,r.inv)
  }
 }

/** Type as a pair of interfaces: left -> right */
case class IPair(left:Interface,right:Interface) extends CType
/** type as a pair of component types: left * right  --- deprecate? */
case class CPair(left:CType, right:CType) extends CType


/** Family types - types for both connectors and families (lambdas)) */
sealed abstract class FType
/** Family type: variable is a connector */
case class Prod(v:CVar,tpar:CType,t:FType) extends FType
/** Family type: variable is a value (int or bool) */
case class ProdV(v:VVar,tpar:VType,t:FType) extends FType
//class      CTVar(val name:String) extends FType
	



