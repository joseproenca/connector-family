Connector families [![Build Status](https://travis-ci.org/joseproenca/connector-family.svg?branch=master)](https://travis-ci.org/joseproenca/connector-family)
==================

This project encodes in [Scala](http://www.scala-lang.org) how to specify and compose families of connectors, mixing the fields of exogenous coordination languages and software product lines.

We view connectors as black boxes with interfaces: a left and a right interface, each with a sequence of input and output ports. Connectors can be composed in parallel (+) or in sequence (;). When composing connectors only the order of the ports matter, not their name.

A connector family wraps a connector parameterised by some arguments together with a feature model and a configuration that relates these two. This feature model allows a high level specification that controls the arguments of the parameterised connector.

Examples
--------
Examples of success and failure of trying to type families of connectors can be found below.

```scala
import connectorFamily.connector.Examples._
import connectorFamily.connector.PP._

typeAndPrint(lossy & fifo)
typeAndPrint(lossy*id & xrd)
typeAndPrint(swap & sdrain)
typeAndPrintWithErrors(lossy.inv*id & sync)
```

The code above produces the following result. When a connector type checks, the function prints the connector representation, its inferred type and constraints by the constraint-based type checker, the substitutions resulting from solving the constraints, and the type after applying the substitution.

```
lossy ; fifo
 : (x1a,x2b)
 | (1,1) == (x1a,x1b)
 | (1,1) == (x2a,x2b)
 | x1b == x2a
 + x1a -> 1
 + x1b -> 1
 + x2a -> 1
 + x2b -> 1
 : (1,1)
lossy * id ; xrd
 : (x1a,x2b)
 | (1,1) * (1,1) == (x1a,x1b)
 | (2,1) == (x2a,x2b)
 | x1b == x2a
 + x1a -> 2
 + x1b -> 2
 + x2a -> 2
 + x2b -> 1
 : (2,1)
swap ; sdrain
 : (x1a,x2b)
 | (2,2) == (x1a,x1b)
 | (2,ø) == (x2a,x2b)
 | x1b == x2a
 + x1a -> 2
 + x1b -> 2
 + x2a -> 2
 + x2b -> ø
 : (2,ø)
Failed to typecheck lossy' * id ; sync.
Failed to unify interfaces.
unifying: [-1,1] == 1
unifying: 1 == x2b
unifying: 1 == x2b
unifying: 1 == x2a
unifying: 1 == x2a
unifying: (1,1) == (x2a,x2b)
unifying: [-1,1] == x1b
unifying: [-1,1] == x1a
unifying: (-1,-1) * (1,1) == (x1a,x1b)
 : (x1a,x2b)
 | (-1,-1) * (1,1) == (x1a,x1b)
 | (1,1) == (x2a,x2b)
 | x1b == x2a
```

A more complex example of typing a parameterised connector is presented below.

```scala
val nat = new VVar("nat")
val X = new IVar("X")

val iF = IIndNat(Interface(1)
                ,new VVar("x")
                ,X,Interface(1,X)
                ,nat)
val tF = IPair(iF,iF)

val y = new CVar("y")    
val seqFifoAux = IndNat(nat, tF, fifo , y, fifo * y , nat)
val seqFifo    = LambdaV(nat, VNat, seqFifoAux)

typeAndPrint(seqFifo)
```

Its inferred type is the following.

```
 \nat:Nat .
  IndN(nat.(IndN(1,x.X.[1,X],nat),IndN(1,x.X.[1,X],nat))
      ,fifo
      ,nat.y.fifo * y
      ,nat)
 : Pi[nat:VNat] (IndN(1,x.X.[1,X],nat),IndN(1,x.X.[1,X],nat))
 | (1,1) == (IndN(1,x.X.[1,X],0),IndN(1,x.X.[1,X],0))
 | (1,1) * (IndN(1,x.X.[1,X],nat),IndN(1,x.X.[1,X],nat)) == 
            (IndN(1,x.X.[1,X],nat+1),IndN(1,x.X.[1,X],nat+1))
 : Pi[nat:VNat] (IndN(1,x.X.[1,X],nat),IndN(1,x.X.[1,X],nat))
```

By swapping the order of composition of the inductive step we obtain an ill-typed connector. That is, if we define:

```scala
val seqFifoAux = IndNat(nat, tF, fifo , y, y * fifo, nat)
val seqFifo    = LambdaV(nat, VNat, seqFifoAux)
```

When typing it we obtain the following error.

```
Failed to typecheck
\nat:Nat .
  IndN(nat.(IndN(1,x.X.[1,X],nat),IndN(1,x.X.[1,X],nat))
      ,fifo,nat.y.y * fifo,nat).
Failed to unify constraints
unifying: IndN(1,x.X.[1,X],nat) == 1
unifying: [IndN(1,x.X.[1,X],nat),1] == [1,IndN(1,x.X.[1,X],nat)]
unifying: (IndN(1,x.X.[1,X],nat),IndN(1,x.X.[1,X],nat)) * (1,1) ==
          (IndN(1,x.X.[1,X],nat+1),IndN(1,x.X.[1,X],nat+1))
unifying: 1 == 1
unifying: 1 == 1
unifying: 1 == 1
unifying: 1 == 1
unifying: (1,1) == (IndN(1,x.X.[1,X],0),IndN(1,x.X.[1,X],0))
 : Pi[nat:VNat] (IndN(1,x.X.[1,X],nat),IndN(1,x.X.[1,X],nat))
 | (1,1) == (IndN(1,x.X.[1,X],0),IndN(1,x.X.[1,X],0))
 | (IndN(1,x.X.[1,X],nat),IndN(1,x.X.[1,X],nat)) * (1,1) ==
   (IndN(1,x.X.[1,X],nat+1),IndN(1,x.X.[1,X],nat+1))
```
