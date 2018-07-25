> {-# LANGUAGE FunctionalDependencies #-}

> module FnDependencies.Notes where 

consider a mutliparameter type class below

> class Coerce a b where
>   coerce :: a -> b 

> instance Coerce Int String where 
>   coerce  = show

> instance Coerce Int [Int] where 
>   coerce  x = [x]

due to mutliple type parameters the compiler will reject expression below

< coerce 12 :: String 

you have to help compiler 

< coerce (12 :: Int) :: String

Function dependencies enable us to reduce this ambiguity

> class Coerce2 a b | b -> a where 
>   coerce2 :: a -> b

> instance Coerce2 Int String where 
>   coerce2 = show 

The relation (b -> a ) tells compiler that if it can infer b, it can simply look up a in 
one of the ype class instances of Coerce2. 


< coerce2 12  :: String

Compiler can infer b :: String and can find uniquely corresponding type a :: Int from instance declaration 
Coerce2 Int String ...

This has an implication that we can't define conflicting instances eg 

< instance Coere2 Float String where  ... would be invalid, since String is already associated with Int

