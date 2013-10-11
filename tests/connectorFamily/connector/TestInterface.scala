package connectorFamily.connector

import org.junit.Test
import org.junit.Assert._

class TestInterface {

	@Test def ++() {
		val i1 = Interface(List(3,-2,-3,2))
		val i2 = Interface(1,1,1,-5,2)
		
    assertEquals("different simplification of "+i1, i1, i2)
    assertTrue("different simplification of "+i1, i1==i2)
    assertTrue("well printed", i2.toString == "[3,-5,2]")
	}
	
}