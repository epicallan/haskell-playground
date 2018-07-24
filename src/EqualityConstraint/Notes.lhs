> module EqualityConstraint.Notes where 

- (~) represents EqualityConstraint operator. It denotes that 2 types need to be the same. 


Examples

function:

< sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) => c1 -> c2 -> c2


Classes:

< class C a b | a -> b

to

< class (F a ~ b) => C a b where
   type F Char = 'True
   type F Int  = 'False
   showl :: a -> b


