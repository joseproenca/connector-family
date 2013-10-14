package connectorFamily.featureModel

sealed abstract class AttrConstr

class IfOut(exp: Exp)  extends AttrConstr
class Require(fid: FID) extends AttrConstr
class Exclude(fid: FID) extends AttrConstr

//// Booleans - 5 operators ////
abstract class Exp extends AttrConstr {
    def &&(e:Exp) = new AndExp(this,e)
    def ||(e:Exp) = new OrExp(this,e)
    def -->(e:Exp) = new ImpliesExp(this,e)
    def <->(e:Exp) = new EquivExp(this,e)
    def unary_! = new NegExp(this)
}

case object True  extends Exp
case object False extends Exp

case class NegExp(e:Exp) extends Exp

//abstract class BoolExp extends Binary[Boolean](l,r);
case class AndExp(left:Exp,right:Exp) extends Exp
case class OrExp(left:Exp,right:Exp)  extends Exp
case class ImpliesExp(left:Exp,right:Exp) extends Exp
case class EquivExp(left:Exp,right:Exp)   extends Exp

//// Integers - 6 arithmetic + 6 relational operators ////
sealed abstract class Term {
    def *(t:Term) = new MultMultTerm(this,t)
    def /(t:Term) = new DivMultTerm(this,t)
    def %(t:Term) = new ModMultTerm(this,t)
    def +(t:Term) = new AddAddTerm(this,t)
    def -(t:Term) = new SubAddTerm(this,t)
    def unary_-   = new MinusTerm(this)
    def le(t:Term)  = new LTEQExp(this,t)
    def ge(t:Term)  = new GTEQExp(this,t)
    def lt(t:Term)  = new LTExp(this,t)
    def gt(t:Term)  = new GTExp(this,t)
    def eq(t:Term)  = new EqExp(this,t)
    def neq(t:Term) = new NotEqExp(this,t)
}

case class IntVal(n:Int) extends Term

case class MinusTerm(op:Term) extends Term

abstract class MultTerm extends Term
case class MultMultTerm(left:Term,right:Term) extends MultTerm
case class DivMultTerm(left:Term,right:Term) extends MultTerm
case class ModMultTerm(left:Term,right:Term) extends MultTerm

abstract class AddTerm extends Term
case class AddAddTerm(left:Term,right:Term) extends AddTerm
case class SubAddTerm(left:Term,right:Term) extends AddTerm

abstract class RelationalExpr extends Exp
case class LTExp(left:Term,right:Term)   extends RelationalExpr
case class GTExp(left:Term,right:Term)   extends RelationalExpr
case class LTEQExp(left:Term,right:Term) extends RelationalExpr
case class GTEQExp(left:Term,right:Term) extends RelationalExpr

abstract class EqualityExpr   extends RelationalExpr
case class EqExp(left:Term,right:Term)    extends EqualityExpr
case class NotEqExp(left:Term,right:Term) extends EqualityExpr ;


//// Variables ////
case class FIDExp(fid:FID)  extends Exp
case class AIDExp(aid:AID)  extends Exp
case class AIDTerm(aid:AID) extends Term 


//// TESTING ////
object AttrConstr extends App {
    import Utils._
    
    println("my constraint: - "+(True && ("A" --> False) || !(4 ge "attr") || ("attr" ge 4)))
}