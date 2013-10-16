package connectorFamily.featureModel

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => MuMap}
import choco.kernel.model.constraints.Constraint
import choco.Choco
import choco.kernel.model.variables.integer.IntegerVariable
import choco.cp.model.CPModel
import choco.cp.solver.CPSolver
import choco.kernel.model.variables.integer.IntegerExpressionVariable


/** Early steps to solve a feature model using Choco.
 *  I tried with the new Choco 3.0, but the types were impossible to match in Scala.
 */
class Solver {
	
	final val model: CPModel   = new CPModel
  final val solver: CPSolver = new CPSolver

	var vars: MuMap[String,IntegerVariable] = MuMap()
	
	// TODO
  def solve(fm: FeatureModel): FeatureSelection = {
  	new FeatureSelection
  }
  
  // TODO
  def solve(fm: FeatureModel, req: FeatureSelection): FeatureSelection = {
    new FeatureSelection
  }
  
  private def fm2choco(fm: FeatureModel): Constraint = {
  	Choco.TRUE
  }
  
  private def addIntVar(name: String, from: Int, to: Int) {
      val v: IntegerVariable = Choco.makeIntVar(name, from, to)
      vars(name) = v
      model.addVariable(v) // needed to include the variable in the constraints to be solved.
  }

  private def addIntVar(name: String, vals: Array[Int]) {
      val v: IntegerVariable = Choco.makeIntVar(name, vals)
      vars(name) = v
      model.addVariable(v)
  }
  
  private def addBoolVar(name: String) {
      val v: IntegerVariable = Choco.makeBooleanVar(name)
      vars(name) = v
      model.addVariable(v)
  }

  private def getVar(n:String) = vars.get(n) match {
  	case Some(x) => x
  	case _ => throw new RuntimeException("Variable not found: "+n)
  }
  	
  
  
  private def attr2choco(decl: Attrs) {
  	for ((aid,range) <- decl.decl) {
  		val a = aid.name
  		if (vars contains a)
  			throw new RuntimeException("Variable '"+a+"' declared twice.")
  		range match {  	
        case IntAttr =>
        	addIntVar(a,Integer.MIN_VALUE+1,Integer.MAX_VALUE-1)
        case IntAttrFrom(from) =>
        	addIntVar(a,from,Integer.MAX_VALUE-1)
        case IntAttrTo(to) =>
          addIntVar(a,Integer.MIN_VALUE+1,to)
        case IntAttrBounded(from,to) =>
          addIntVar(a,from,to)
        case IntAttrSet(set) =>
          addIntVar(a, set.toArray)
        case BoolAttr =>
        	addBoolVar(a)
  	}}  	
  }
  
  private def constr2choco(c: AttrConstr, root: FID): Constraint = {
  	val f = root.name
  	val fv = getVar(f)
  	c match {
  		case IfOut(exp) => Choco.implies(Choco.eq(fv, 1),constr2choco(exp,root))
  		case Require(fid) => Choco.eq(fv,getVar(fid.name))
  		case Exclude(fid) => Choco.neq(fv,getVar(fid.name))
      case True => Choco.TRUE
      case False => Choco.FALSE
      case NegExp(e) => Choco.not(constr2choco(e,root))
      case AndExp(left,right) => Choco.and(constr2choco(left,root),constr2choco(right,root))
      case OrExp(left,right) => Choco.or(constr2choco(left,root),constr2choco(right,root))
      case ImpliesExp(left,right) => Choco.implies(constr2choco(left,root),constr2choco(right,root))
      case EquivExp(left,right) => Choco.and(
      		Choco.implies(constr2choco(left,root),constr2choco(right,root)),
      		Choco.implies(constr2choco(right,root),constr2choco(left,root)))
     
  	}
  }
  
  private def term2choco(t:Term, root:FID): IntegerExpressionVariable = {
  	t match {
  		case IntVal(i) => Choco.constant(5)
  		case MinusTerm(e) => Choco.neg(term2choco(e,root))
  		case MultMultTerm(left,right) => Choco.mult(term2choco(left,root),term2choco(right,root)) 
  		case DivMultTerm(left,right) => Choco.div(term2choco(left,root),term2choco(right,root))
  		case ModMultTerm(left,right) => Choco.mod(term2choco(left,root),term2choco(right,root))

  	}
  
  }
}