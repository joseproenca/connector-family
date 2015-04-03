package connectorFamily.connector

import org.junit.Test
import org.junit.Assert._
import Examples._

class TestConnector {

  def pp(c:Conn) = println(PP.typeAndPrint(c))
  def ppe(c:Conn) = println(PP.typeAndPrintWithErrors(c))

  @Test def TestComposition() {
 
    // all should type&check
    
    println("--- testing new connectors ---")
    
    // examples from the paper
    pp(sdrain)
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
            
    // associativity works for primitive connectors
    pp((lossy*sync)*lossy & lossy*(sync*lossy))
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

    val nat = new VVar("nat"); val X = new IVar("X"); val y = new CVar("y")
    val iF = IIndNat(Interface(1), new VVar(""), X, Interface(1,X), nat)
    val tF = IPair(iF,iF)

    val parFifoAux = IndNat(nat, tF, fifo , y, fifo * y , nat)
    val parFifo    = LambdaV(nat, VNat, parFifoAux)
    
    // Should succeed
    println(PP.typeAndPrint(parFifo))
    assert(true,"No type errors in inductive connector")
  }

//  @Test ( expected = classOf[TypeException] )
//  def TestIndNFail() {
//    println("--- testing ill-typed inductive connector ---")
//
//    val (nat,_,_,y,tF) = auxTypes
//
//    // Should fail because "y*fifo" is not "fifo*y".
//    val parFifoAuxF = IndNat(nat, tF, fifo , y, y * fifo , nat)
//    val parFifoF    = LambdaV(nat, VNat, parFifoAuxF)
//
//    // fail and print the errors
//    println(PP.typeAndPrintWithErrors(parFifoF))
//
//    // Should raise a TypeException
//    val fail = PP.typeAndPrint(parFifoF)
//  }


  // SHOULD WORK! - TODO
  @Test //( expected = classOf[TypeException] )
  def TestProdComp() {
    println("--- testing composition of connector families ---")

//    val (nat1,_,_,y1,tF1) = auxTypes
//    val (nat2,x,xx,y2,tF2) = auxTypes

    val nat1 = new VVar("nat"); val X1 = new IVar("X"); val y1 = new CVar("y")
    val nat2 = new VVar("nat"); val X2 = new IVar("X"); val y2 = new CVar("y")

    val iF1 = IIndNat(Interface(1), new VVar(""), X1, Interface(1,X1), nat1)
    val tF1 = IPair(iF1,iF1)
    val parFifoAuxF1 = IndNat(nat1, tF1, fifo , y1, fifo * y1, nat1)
    val parFifoF1    = LambdaV(nat1, VNat, parFifoAuxF1)

    // Reverse order for recursion (y*fifo vs. y*fifo)
    val iF2 = IIndNat(Interface(1), new VVar(""), X2, Interface(X2,1), nat2)
    val tF2 = IPair(iF2,iF2)
    val parFifoAuxF2 = IndNat(nat2, tF2, fifo , y2, y2 * fifo, nat2)
    val parFifoF2    = LambdaV(nat2, VNat, parFifoAuxF2)

    val comb = parFifoF1 & parFifoF2

    // fail and print the errors (if they exist - they should not now)
    println(PP.typeAndPrintWithErrors(comb))

//    // Should raise a TypeException before printing
//    println(PP.typeAndPrint(comb))
  }

  @Test
  def TestSequencer() {
    println("--- testing sequencer ---")

    val lf = lossy & fifo
    val base = eps.inv  &  (dupl & id*fifof)*id.inv  &  id*epsr
    
    // n id's in parallel
    val nat1 = new VVar("nat1")
    val nat2 = new VVar("nat2")
    val x   = new VVar("x")
    val xx  = new IVar("X")
    val y   = new CVar("y")        
    val iId = IIndNat(Interface(0), x, xx, Interface(1,xx), nat1)
    val tId = IPair(iId,iId)
    
    val parIds    = LambdaV(nat2, VNat, 
        IndNat(nat1, tId, id0 , y, id * y , nat2) )
    
    // build the recursive part
    val size = new VVar("size")
    val n = new VVar("n")
    val i = new IVar("i")
    val c   = new CVar("y")    
    // seqtype = ( [1,-1] , [nx1,1,-1] )
    val seqType:CType = IPair(Interface(1,-1), IIndNat(Interface(1,-1), n, i, Interface(1,i), n) )
    val rec = LambdaV(size, VNat,
        // IndNat(varInType . Type , zeroTerm ,  natVarInRec . termVarInRec . RecTerm  ,  natExpr)
    	//case class IndNat(vt:VVar, t:CType, c0:Conn, vs:CVar, cs:Conn, nat:Val) extends Conn
		IndNat(n, seqType
			  ,id*id.inv
			  ,c, AppV(parIds,size) * (dupl & id*fifo) * id.inv
			  ,size)
	)
    
    ppe(parIds)
    println("---")
    ppe(AppV(parIds,VZero))
    println("---")
    ppe(AppV(parIds,VSucc(VZero)))
    println("---")
    ppe(AppV(parIds,VZero) & id0)
    println("---")
    ppe(rec)

  }
}