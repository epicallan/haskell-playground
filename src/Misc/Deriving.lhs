> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module Misc.Deriving where

consider a newtype Dollars below
 
> newtype Dollars = Dollars Int deriving (Eq,Show,Num)

if you want to use arithmetic on Dollars you have to explicitly define instance of Num:

< instance Num Dollars where
<   Dollars a + Dollars b = Dollars (a+b)

but with  GeneralizedNewtypeDeriving extension you don't have to

> add :: Dollars -> Dollars -> Dollars
> add x y = x + y