-- This is needed so that we can have constraints in type synonyms.
{-# LANGUAGE RankNTypes #-}

module Lenses.Basics.Core where

import Control.Applicative
import Data.Bifunctor

{-
   A lens allows us to do something to a big structure given that we know how to do something to a part of it.
   Note that “doing something” means more than “applying a function” – for instance,
   “randomly shuffling a list” (which requires IO) or “getting all permutations”
   (which you could say is done in list monad) are all examples of “doing something”.
-}

{-------------------------------------------------------------------------------
  Lens as getter and setter
-------------------------------------------------------------------------------}

-- | Getter + setter = LensGs
data LensGs s a = LensGs
    { getter :: s -> a
    , setter :: a -> s -> s
    }


setIth :: Int -> a -> [a] -> [a]
setIth index new list
    | index < 0        = error "setIth: negative index"
    | null list        = error "setIth: index too large"
    | old:rest <- list = if index == 0
                            then new : rest
                            else old : setIth (index-1) new rest

-- | the ixGs lense, accesses and sets the i-th element of a list
ixGs :: Int -> LensGs [a] a
ixGs i = LensGs { getter = (!! i)
                , setter = setIth i
                }


{-------------------------------------------------------------------------------
  Lens as Modifier
-------------------------------------------------------------------------------}

type LensM s a = (a -> a) -> s -> (a, s)

-- ix :: Int -> (a -> a) -> [a] -> (a, [a])

ixM :: Int -> LensM [a] a
ixM index f list
    | index < 0        = error "setIth: negative index"
    | null list        = error "setIth: index too large"
    | old:rest <- list = if index == 0
                            then (old, f old : rest)
                            else second (old:) $ ixM (index - 1) f rest

{-
second (old:) $ ix (index-1) f rest
is same as:
    -- let (x', s') = ix (index-1) f rest
    -- in  (x', old : s')
-}

-- using id as modifying function
-- > fst $ ixM 3 id [7, 4, 1, 8]
-- 8

-- To set the value, we can use const:
-- > snd $ ixM 3 (const 1000) [7, 4, 1, 8]
-- [7, 4, 1, 1000]

-- | with a functor
type LensF s a = Functor f => (a -> f a) -> s -> (a, f s)

ixF :: Int -> Lens [a] a
ixF index f list
    | index < 0        = error "setIth: negative index"
    | null list        = error "setIth: index too large"
    | old:rest <- list = if index == 0
                            then (old, (: rest) <$> f old)
                            else  second ((old:) <$>) $ ixF (index - 1) f rest


data Storey x f a = Storey x (f a)
    deriving Show

instance Functor f => Functor (Storey x f) where
    fmap f (Storey x fa) = Storey x (fmap f fa)
