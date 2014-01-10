package connectorFamily.connector

/**
 * Inductive values (naturals and booleans) 
 */
sealed abstract class Val
case object VZero         extends Val
case class  VSucc(v:Val)  extends Val
case object VTrue             extends Val
case object VFalse            extends Val
     class  VVar(val name:String) extends Val

sealed abstract class VType
{ def ===(v:VType) = VEq(this,v) }
case object VBool extends VType
case object VNat  extends VType