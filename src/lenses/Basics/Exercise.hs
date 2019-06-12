-- Refrence: https://artyom.me/lens-over-tea-1
-- This is needed so that we can have constraints in type synonyms.
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Lenses.Basics.Exercise where


type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (x, y) = (,y) <$> f x

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, y) = (x,) <$> f y

-- Make a lens out of a getter and a setter.
-- 'lens' :: 'Functor' f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set afb s = set s <$> afb (get s)


-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
-- choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
--          -> Lens (Either s1 s2) (Either t1 t2) a b
-- choosing _ _= undefined

-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l $ (\x -> (x, x)) . f

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~)  _ _= undefined

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
-- united :: Lens' s ()
-- united = undefined
