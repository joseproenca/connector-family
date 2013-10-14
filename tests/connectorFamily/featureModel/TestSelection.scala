package connectorFamily.featureModel

import org.junit.Test
import org.junit.Assert._

class TestSelection {

	val featSel = new FeatureSelection
  featSel("Merger") = true
  featSel("Merger","size") = 5
  featSel("Merger","isExcl") = false
  featSel("Simple") = true
  
  @Test def printSelection {
	  println(featSel)

    assertTrue(featSel("Merger"))
    assertTrue("Integer attribute", featSel("Merger", "size").isRight)
    assertTrue("Boolean attribute", featSel("Merger", "isExcl").isLeft)
	  assertEquals(featSel("Merger", "size").right.get , 5)
	}

}