package connectorFamily.connector

import org.junit.Test
import org.junit.Assert._

class TestConnector {

	@Test def Test() {
    val sync = new Connector(Interface(1),Interface(1),SimpleRep("sync"))
    val lossy = new Connector(Interface(1),Interface(1),SimpleRep("lossy"))
    val fifo= new Connector(Interface(1),Interface(1),SimpleRep("fifo"))
    val sdrain = new Connector(Interface(1),Interface(1),SimpleRep("sdrain"))
		
    val dupl = new Connector(Interface(1),Interface(2),SimpleRep("dupl"))
    val xr = new Connector(Interface(2),Interface(1),SimpleRep("xr"))
    val mrg = new Connector(Interface(2),Interface(1),SimpleRep("mrg"))
    val eta = new Connector(Interface(0),Interface(1,-1),SimpleRep("eta"))
    val eps = new Connector(Interface(-1,1),Interface(0),SimpleRep("eps"))
    val epsd = new Connector(Interface(1,-1),Interface(0),SimpleRep("epsd"))
    
    println(dupl - sync * lossy) // typechecks
    println(
        eta
        )
    println(
        (dupl - sync * fifo - sync * dupl - sync * sync * fifo) * sync.inv
        )
    println(
        sync * sync * epsd
        )
    println(
    		eta -
    		(dupl - sync * fifo - sync * dupl - sync * sync * fifo) * sync.inv -
    		sync * sync * epsd
    		)
	}

}