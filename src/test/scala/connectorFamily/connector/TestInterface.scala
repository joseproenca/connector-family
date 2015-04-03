package connectorFamily.connector

import org.junit.Test
import org.junit.Assert._

class TestInterface {
	implicit def int2intlit(i: Int): ILit =
	    if (i>=0) INat(i)
	    else IDual(INat(-1*i))

	@Test def SimplifyAndPrint() {
		val i1 = Interface(3,-2,-3,2)
		val i2 = Interface(1,1,1,-5,2)

    assertEquals("different simplification of "+i1, i1, i2)
//    assertTrue("different simplification of "+i1, i1==i2)
    assertEquals("well printed - "+i2,
      i2.get.map(PP(_)).mkString("[",",","]") , "[3,-5,2]")
	}
	
}