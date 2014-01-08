package connectorFamily.connector

import org.junit.Test
import org.junit.Assert._
import Examples._

class TestConnector {

  @Test def TestComposition() {
  
    // all should type&check
  
    def pp(c:Conn) = {
      val (t,cnst) = TypeCheck(c)
      println(PP(c)+" : "+PP(t)+" | "+cnst.mkString("[",",","]"))
    }
    println("--- testing new connectors ---")
    
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
     
  
  @Test def TestBigConnector() {

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
//  	assertEquals("Type-checking sync merge.",
//  			(Interface(1),Interface(1)),
//  			TypeCheck(lossyAB))
  }

}