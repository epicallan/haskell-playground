> module RandomTypesTheory where 


Existential Quantified types is away of squashing types into one. 
Think usage of forall in function definitions.

< map :: forall a b . (a -> b) -> [a] -> [b]

Example of an Existential data types

> data T = forall a . Show a => MKT a 

The interaction of the universal quantifier with data types produces a qualified subset of types guaranteeing certain facilities as described by one or more class constraints.


