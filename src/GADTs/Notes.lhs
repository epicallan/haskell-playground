> {-#LANGUAGE GADTs #-}
> module GADTs.Notes where 

GADTs allow us to restrict the return types of data type constructors. 



Consider a small simple language represented in below data type, in which a is `a` is a phantomType that
allows us to restrict return type of `Exp``

> data Expr'' a = I'' Int
>            | B'' Bool
>            | Add'' (Expr'' a) (Expr'' a)
>            | Mul'' (Expr'' a) (Expr'' a)
>            | Eq''  (Expr'' a) (Expr'' a)

this allows us to have evaluation statement that type check

> add :: Expr'' Int -> Expr'' Int -> Expr'' Int
> add = Add''

if we didn't have phantom type, the add function below would have type checked although it doesnt make sense

> data Expr' = I' Int
>            | B' Bool
>            | Add' Expr' Expr'
>            | Mul' Expr' Expr'
>            | Eq'  Expr' Expr'

< add :: Expr' -> Expr'  -> Expr'
< B' True `Add'` I' 5 :: Expr'


One of the big difference between GADTs and phantomTypes is that we can pattern match over the parameterized contructors 
we get from GADTs. Which is not the case with phantomTypes.

Since the phantom type in phantomType is not inbuilt with in data constructors of a phantomType, expressions
as the one below are valid and we can't even pattern match over constructors.


< I 5 :: Expr String

GADTs allow us to restrict the return type of constructors in a more concise manner. 
Put another way with GADTs we can control exactly what kind of data structure we return. 

What would be just a phantomType a is part of data type constructors

> data Expr a where
>    I   :: Int  -> Expr Int
>    B   :: Bool -> Expr Bool
>    Add :: Expr Int -> Expr Int -> Expr Int
>    Mul :: Expr Int -> Expr Int -> Expr Int
>    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool


with GADTs we can take advantage of new type constructors that allow us to type match over our data constructors

> eval :: Expr a -> a
> eval (I n) = n
> eval (B b) = b
> eval (Add e1 e2) = eval e1 + eval e2
> eval (Mul e1 e2) = eval e1 * eval e2
> eval (Eq  e1 e2) = eval e1 == eval e2

GADTs give us the ability to control exactly what kind of `Foo`` you return. 
With GADTs, a constructor for `Foo a` is not obliged to return `Foo a`; it can return any `Foo blah`

> data FooInGadtClothing a where
>   MkFooInGadtClothing :: a -> FooInGadtClothing a

which is no different from:  

> data Haskell98Foo a = MkHaskell98Foo a

by contrast, consider:
 
> data TrueGadtFoo a where
>   MkTrueGadtFoo :: a -> TrueGadtFoo Int

This has no Haskell 98 equivalent.

NOTE: The constructor functions must return some kind of Foo or another. 
Returning anything else simply wouldn't work.

> data Bar where
>   BarNone :: Bar -- This is ok

< data Foo where
<  MkFoo :: Bar Int-- This will not typecheck

safe lists with GADTs
____

consider a type `SafeList x y`
The idea is to have something similar to normal Haskell lists [x], but with a little extra information in the type. 
This extra information (the type variable y) tells us whether or not the list is empty. 
Empty lists are represented as SafeList x Empty, whereas non-empty lists are represented as SafeList x NonEmpty

> data Empty
> data NonEmpty

the idea is that you can have either 

< SafeList a Empty

or 

< SafeList a NonEmpty

> data SafeList a b where
>   Nil  :: SafeList a Empty
>   Cons :: a -> SafeList a b -> SafeList a NonEmpty

> safeHead :: SafeList a NonEmpty -> a
> safeHead (Cons x _) = x

TODO: read through http://haroldcarr.com/posts/2013-08-24-type-level-computation-in-haskell-via-gadts.html