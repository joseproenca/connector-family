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
    case IPair(l,r) => IPair(r,l)
    case CPair(l,r) => CPair(l.inv,r.inv)
  }
 }
case class IPair(left:Interface,right:Interface) extends CType
case class CPair(left:CType, right:CType) extends CType


sealed abstract class FType
case class Prod(v:CVar,tpar:CType,t:FType) extends FType
case class ProdV(v:VVar,tpar:VType,t:FType) extends FType
	



object PP {
  def apply(c:Conn): String = c match {
    case Seq(c1,c2) => apply(c1)+" ; "+apply(c2)
    case Par(c1,c2) => apply(c1)+" x "+apply(c2)
    case App(c1,c2) => apply(c1)+" "+apply(c2)
    case AppV(c1,c2) => apply(c1)+" "+c2
    case Lambda(v,t,c) => "\\"+v+":"+t+" . "+apply(c)
    case LambdaV(v,t,c) => "\\"+v+":"+t+" . "+apply(c)
    case IndBool(vt:VVar, t:CType, ct:Conn, cf:Conn, bool:Val) =>
      "if "+bool+"["+vt+"."+t+" then "+apply(ct)+" else "+apply(cf)
    case IndNat(vt:VVar, t:CType, c0:Conn, vs:CVar, cs:Conn, nat:Val) =>
      "IndN("+vt+"."+t+","+apply(c0)+","+vt+"."+vs+"."+apply(cs)+","+nat+")"
    case CPrim(t,n) => n//+":"+apply(t)
    case v:CVar => v.name
  }
  
  def apply(t:FType): String = t match {
    case Prod(v:CVar,tpar:CType,t:FType) => "Pi["+v+":"+tpar+"] "+apply(t)
    case ProdV(v:VVar,tpar:VType,t:FType) => "Pi["+v+":"+tpar+"] "+apply(t)
    case IPair(left:Interface,right:Interface) => "("+apply(left)+","+apply(right)+")"
    case CPair(left:CType, right:CType) => apply(left) +" * "+apply(right)
  }
  
  def apply(i:Interface) : String = i.get match {
    case Nil => "[]"
    case List(lit) => apply(lit)
    case lst:List[ILit] => lst.map(apply(_)).mkString("[",",","]")
  }
  
  def apply(i:ILit): String = i match {
    case INat(n) => n.toString
    case IDual(i) => "-"+apply(i)
    case IIndBool(a,b,c) => "IndB("+apply(a)+","+apply(b)+","+c+")"
    case IIndNat(a,b,c,d,e) => "IndN("+apply(a)+","+b+","+apply(c)+","+apply(d)+","+e+")"
    case v:IVar => v.name
  }
}

