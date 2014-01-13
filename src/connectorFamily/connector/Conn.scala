package connectorFamily.connector

/**
 * Second attempt to create a connector class,
 * where the derivation tree for typed connectors is built.
 */
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

sealed abstract class CType extends FType { 
  def ===(c:CType) = CEq(this,c)
  def inv:CType = this match {
    case IPair(l,r) => IPair(r.inv,l.inv)
    case CPair(l,r) => CPair(l.inv,r.inv)
  }
 }
case class IPair(left:Interface,right:Interface) extends CType
case class CPair(left:CType, right:CType) extends CType


sealed abstract class FType
case class Prod(v:CVar,tpar:CType,t:FType) extends FType
case class ProdV(v:VVar,tpar:VType,t:FType) extends FType
//class      CTVar(val name:String) extends FType
	



