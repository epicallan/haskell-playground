> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE TypeFamilies          #-}
> {-# LANGUAGE TypeApplications     #-}
> module FnDependencies.Notes where

> import Data.Kind (Type)
> import Data.Proxy

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
one of the type class instances of Coerce2.

> testCoerce2 :: String
> testCoerce2 = coerce2 12

Compiler can infer b :: String and can find uniquely corresponding type a :: Int from instance declaration
Coerce2 Int String ...

This has an implication that we can't define conflicting instances eg

< instance Coere2 Float String where  ... would be invalid, since String is already associated with Int

Type family solutions
_____________________________________


> class Coerce3 a b where
>   type Coerceable a b :: Type
>   coerce3 :: Proxy b -> a -> Coerceable a b

> instance Coerce3 Int String where
>   type Coerceable Int String = String
>   coerce3 _ y = show y

> testCoerce3 :: String
> testCoerce3 = coerce3 (Proxy @String) (10 :: Int)

> class Coerce4 a where
>   type Coerceable2 a :: Type
>   coerce4 :: a -> Coerceable2 a

> instance Coerce4 Int where
>   type Coerceable2 Int = String
>   coerce4 y = show y

> testCoerce4 :: String
> testCoerce4 = coerce4 @Int 10
