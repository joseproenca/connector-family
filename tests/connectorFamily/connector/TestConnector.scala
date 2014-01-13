package connectorFamily.connector

import org.junit.Test
import org.junit.Assert._
import Examples._

class TestConnector {

  @Test def TestComposition() {
  
    // all should type&check
  
    def pp(c:Conn) = println(PP.typeAndPrint(c))
    
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

    println("--- testing larger connector ---")

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
  	println(PP.typeAndPrint(lossyAB))
  	
//  	assertEquals("Type-checking sync merge.",
//  			(Interface(1),Interface(1)),
//  			TypeCheck(lossyAB))
  }
  
  
  private def auxTypes = {
    val nat = new VVar("nat")
    val x   = new VVar("x")
    val xx  = new IVar("X")
    val y   = new CVar("y")    
    
    val iF = IIndNat(Interface(1), x, xx, Interface(1,xx), nat)
    val tF = IPair(iF,iF)
    
    (nat,x,xx,y,tF)    
  }
    
  
  @Test def TestIndNSuccess() {
    
    println("--- testing inductive connector ---")
    
    val (nat,_,_,y,tF) = auxTypes
    
    val parFifoAux = IndNat(nat, tF, fifo , y, fifo * y , nat)
    val parFifo    = LambdaV(nat, VNat, parFifoAux)
    
    // Should succeed
    println(PP.typeAndPrint(parFifo))
  }

  @Test ( expected = classOf[TypeException] )
  def TestIndNFail() {
    println("--- testing ill-typed inductive connector ---")

    val (nat,_,_,y,tF) = auxTypes

    // Should fail because "y*fifo" is not "fifo*y".
    val parFifoAuxF = IndNat(nat, tF, fifo , y, y * fifo , nat)
    val parFifoF    = LambdaV(nat, VNat, parFifoAuxF)
    
    // fail and print the errors
    println(PP.typeAndPrintWithErrors(parFifoF))

    // Should raise a TypeException
    val fail = PP.typeAndPrint(parFifoF)
  }
  
  @Test //( expected = classOf[TypeException] )
  def TestProdComp() {
    println("--- testing composition of connector families ---")

    val (nat1,_,_,y1,tF1) = auxTypes
    val (nat2,_,_,y2,tF2) = auxTypes

    val parFifoAuxF1 = IndNat(nat1, tF1, fifo , y1, fifo * y1, nat1)
    val parFifoF1    = LambdaV(nat1, VNat, parFifoAuxF1)

    val parFifoAuxF2 = IndNat(nat2, tF2, fifo , y2, fifo * y2, nat2)
    val parFifoF2    = LambdaV(nat2, VNat, parFifoAuxF2)
    
    val comb = Seq(parFifoF1,parFifoF2)

    // fail and print the errors
    println(PP.typeAndPrintWithErrors(comb))

    // Should raise a TypeException
    val fail = PP.typeAndPrint(comb)
  }

}