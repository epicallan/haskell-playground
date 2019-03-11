> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-#LANGUAGE GADTs #-}

> module BookOfTypes.DataKinds.BuiltInTypes where 


The promoted version of a String is called a SYMBOL. 
SYMBOLs are not lists of characters—they are completely distinct things

Eg

< :kind "hello"
< "hello" :: Symbol

Natural numbers are of Kind Nat 

< :kind 5085072209
< 5085072209 :: Nat

Lists are promoted as below given a below list definition

< data [a] = [] | a : [a]

< '[] of kind [A]
< '(:) of kind a -> [a]

Make sure to add a space after a promoted list

< :kind '[ 'True ]
<'[ 'True ] :: [Bool]
