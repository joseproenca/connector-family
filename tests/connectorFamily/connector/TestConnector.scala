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
            sync*sync*epsd)
    println(id.inv*eta &
            (id.inv*dupl & eps*fifof & dupl & id*fifo) * id.inv &
            id*epsd)
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
    case 0 => base(epsd)
    case _ => val prev = seq(n-1)
            base(more(prev.hole))
    }
    
    assertEquals("Contexts match.",seq(3).ctx,base)
    
    assertEquals("Expected seq_0",
      "eta ; dupl ; id*fifof*id' ; id*epsd: [] -> [1]",
      seq(0).toString)
    assertEquals("Expected seq_1",
      "eta ; dupl ; id*fifof*id' ; id*dupl ; id*fifo*id' ; id*epsd: [] -> [2]",
      seq(1).toString)
    assertEquals("Expected seq_2",
      "eta ; dupl ; id*fifof*id' ; id*dupl ; id*fifo*id' ; id*dupl ; id*fifo*id' ; id*epsd: [] -> [3]",
      seq(2).toString)
  }
}