package connectorFamily.featureModel

import org.junit.Assert._
import org.junit.Test

class TestAttrConstr {
  import Utils._
    
  val c1 = (True && ("A" --> False) || !(4 >== "attr") || ("attr" <== 6))
  
  //// This test will fail as soon as the structure changes.
  @Test def printConstr {
  	assertEquals(c1.toString,
  		"OrExp(OrExp(AndExp(True,"+
  		             "ImpliesExp(FIDExp(FID(A)),False)),"+
  		       "NegExp(GTEQExp(IntVal(4),AIDTerm(AIDU(attr))))),"+
  		"LTEQExp(AIDTerm(AIDU(attr)),IntVal(6)))")
  }

}