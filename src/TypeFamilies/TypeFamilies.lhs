> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE KindSignatures #-}
> {-#LANGUAGE GADTs #-}

> module TypeFamilies.TypeFamilies where


> or :: Bool -> Bool -> Bool
> or True _  = True
> or False y = y

or can be promoted as a closed type family

> type family Or (x :: Bool) (y :: Bool) :: Bool where
>   Or 'True y = 'True
>   Or 'False y = y



exercise: a closed type family to compute and

> type family Add (x :: Bool) (y :: Bool) :: Bool where
>   Add 'True y  = y
>   Add 'False y = 'False


< type family Foo (x :: Bool) (y :: Bool) :: Bool

Kind for Foo is Bool -> Bool -> Bool

< type family Bar x y :: Bool -> Bool -> Bool

kind for Bar is Type -> Type -> Bool -> Bool -> Bool
