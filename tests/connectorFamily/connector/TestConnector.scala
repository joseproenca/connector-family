package connectorFamily.connector

import org.junit.Test
import org.junit.Assert._
import Examples._

class TestConnector {

	@Test def TestComposition() {
    
		// all should type&check
    		    
    // examples from the paper
    println(lossy & fifo)
    println(lossy*id & xrd)
    println(swap & sdrain)
    println(lossy & xr)

    // adding compact closed aspect (duals)
    println(lossy.inv*id & eps)
    println(eta*id & id*eps)
    println(eta &
            (dupl & sync*fifo & sync*dupl & sync*sync*fifo) * sync.inv &
            sync*sync*epsd)
    println(id.inv*eta &
    		    (id.inv*dupl & eps*fifo & dupl & id*fifo) * id.inv &
    		    id*epsd)
	}
   
	@Test def TestContexts() {
    // contexts (f: Connector[R] => Connector[R])
    val gamma1 = new Context(
    		(r:Connector[SimpleRep]) =>
    			eta & (dupl & id * fifo) * id.inv & id * r
    )
    
    val gamma2 = new Context(
        (r:Connector[SimpleRep]) =>
        	(dupl & id*fifo) * id.inv & id * r
    )
    
    def seq(n:Int): ConnectorCtx[SimpleRep] = n match {
    	case 0 => gamma1(epsd)
    	case _ =>
    		val prev = seq(n-1)
    		gamma1(gamma2(prev.hole))
    }
    
    assertEquals("Expected seq_0",
      "eta ; dupl ; id*fifo*id' ; id*epsd: [] -> [1]",
  		seq(0).toString)
    assertEquals("Expected seq_1",
      "eta ; dupl ; id*fifo*id' ; id*dupl ; id*fifo*id' ; id*epsd: [] -> [2]",
      seq(1).toString)
    assertEquals("Expected seq_2",
      "eta ; dupl ; id*fifo*id' ; id*dupl ; id*fifo*id' ; id*dupl ; id*fifo*id' ; id*epsd: [] -> [3]",
      seq(2).toString)
	}

}