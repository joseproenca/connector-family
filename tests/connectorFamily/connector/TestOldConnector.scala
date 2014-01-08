package connectorFamily.connector

import org.junit.Test
import org.junit.Assert._
import OldExamples._

class TestOldConnector {

  @Test def TestComposition() {
  
    // all should type&check
  
    def pp(c:Any) = println(c)
    
	println("--- testing OLD connectors ---")

    // examples from the paper
    pp(lossy & fifo)
    pp(lossy*id & xrd)
    pp(swap & sdrain)
    pp(lossy & xr)
    
    // adding compact closed aspect (duals)
    pp(lossy.inv*id & eps)
    pp(eta*id & id*eps)
    pp(eta &
            (dupl & sync*fifof & sync*dupl & sync*sync*fifo) * sync.inv &
            sync*sync*epsr)
    pp(id.inv*eta &
            (id.inv*dupl & eps*fifof & dupl & id*fifo) * id.inv &
            id*epsr)
  }
     
  @Test def TestContexts() {
    type Conn = OldConnector[SimpleRep]
    type ConnCtx = OldConnectorCtx[SimpleRep]
  
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
  	type Conn = OldConnector[SimpleRep]
  	type ConnCtx = OldConnectorCtx[SimpleRep]

  	val syncMerge = //new Context(
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
//    )
  	
  	val lossyAB = syncMerge(lossy*lossy)
  	assertEquals("Type-checking sync merge.",
  			(lossyAB.from,lossyAB.to),
  			(Interface(1),Interface(1)))
  }

}