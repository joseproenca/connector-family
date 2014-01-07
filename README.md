Connector families
==================

This project encodes in [Scala](http://www.scala-lang.org) how to specify and compose families of connectors, mixing the fields of exogenous coordination languages and software product lines.

We view connectors as black boxes with interfaces: a left and a right interface, each with a sequence of input and output ports. Connectors can be composed in parallel (+) or in sequence (;). When composing connectors only the order of the ports matter, not their name.

A connector family wraps a connector parameterised by some arguments together with a feature model and a configuration that relates these two. This feature model allows a high level specification that controls the arguments of the parameterised connector.
