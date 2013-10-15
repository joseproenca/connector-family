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
            (dupl & sync*fifof & sync*dupl & sync*sync*fifo) * sync.inv &
            sync*sync*epsr)
    println(id.inv*eta &
            (id.inv*dupl & eps*fifof & dupl & id*fifo) * id.inv &
            id*epsr)
  }
     
  @Test def TestContexts() {
    type Conn = Connector[SimpleRep]
    type ConnCtx = ConnectorCtx[SimpleRep]
  
    val base = new Context(
      (r:Conn) => eta & (dupl & id * fifof) * id.inv & id * r
    )
    
    val more = new Context(
      (r:Conn) => (dupl & id*fifo) * id.inv & id * r
    )
    
    def seq(n:Int): ConnCtx = n match {
    case 0 => base(epsr)
    case _ => val prev = seq(n-1)
              base(more(prev.hole))
    }
    
    assertEquals("Contexts match.",seq(3).ctx,base)
    
    assertEquals("Expected seq_0",
      "eta ; dupl ; id*fifof*id' ; id*epsr: [] -> [1]",
      seq(0).toString)
    assertEquals("Expected seq_1",
      "eta ; dupl ; id*fifof*id' ; id*dupl ; id*fifo*id' ; id*epsr: [] -> [2]",
      seq(1).toString)
    assertEquals("Expected seq_2",
      "eta ; dupl ; id*fifof*id' ; id*dupl ; id*fifo*id' ; id*dupl ; id*fifo*id' ; id*epsr: [] -> [3]",
      seq(2).toString)
  }
  
  @Test def TestBigConnector() {
  	type Conn = Connector[SimpleRep]
  	type ConnCtx = ConnectorCtx[SimpleRep]

  	val syncMerge = new Context(
  			(ab: Conn) =>
  				dupl &
  				(xr(3) &
  					 dupl * dupl(3) * dupl &
             id * mrg * id * mrg * id &
             id * id * swap * id &
  					 fifo * ab * fifo * fifo &
  					 id * id * swap * id &
  					 dupl * xr * dupl(3) * xr * dupl &
  					 id * sdrain * sdrain * id * sdrain * sdrain * id &
  					 mrg(3) &
  					 dupl
  				) * fifo &
  				id * sdrain
  	)
  	
  	val lossyAB = syncMerge(lossy*lossy)
  	assertEquals("Type-checking sync merge.",
  			(lossyAB.from,lossyAB.to),
  			(Interface(1),Interface(1)))
  }

}