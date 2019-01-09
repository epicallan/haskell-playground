-- Refrence: https://artyom.me/lens-over-tea-1
-- This is needed so that we can have constraints in type synonyms.
{-# LANGUAGE RankNTypes #-}

module Lenses.Basics.Core where

import Control.Applicative
import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity

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
type LensF s a = forall f. Functor f => (a -> f a) -> s -> (a, f s)

ixF :: Int -> LensF [a] a
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

-- >> ixF 3 (\x -> Storey x [1..x]) [7, 4, 1, 8]
-- >> 8,Storey 8 [[7,4,1,1],[7,4,1,2],[7,4,1,3],[7,4,1,4],
-- >>                 [7,4,1,5],[7,4,1,6],[7,4,1,7],[7,4,1,8]])

-----------------------------------------------------
-- Actual lens type

-- Read `s` as `Source` and `t` as target

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- simple lens definition
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s


ix :: Int -> Lens' [a] a
ix index f list
  | index < 0        = error "ix: negative index"
  | null list        = error "ix: index too large"
  | old:rest <- list = if index == 0
                         then (: rest) <$> f old
                         else (old :) <$> ix (index-1) f rest


-- if you just need to set or mofify a value, use identity functior.
--   > -- Setting.
--   > runIdentity $ ix 2 (\x -> Identity 88) [0..4]
--    [0, 1, 88, 3, 4]

 -- Modification.
--    > runIdentity $ ix 2 (\x -> Identity (x * 44)) [0..4]
--   [0, 1, 88, 3, 4]

over :: Lens s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)

-- > over (ix 2) (const 88) [0..4]
-- [0,1,88,3,4]

-- over (ix 2) (* 44) [0..4]
-- [0, 1, 88, 3, 4]

--- To just get a value (view)
-- we use Const
-- data Const  x  a   = Const  x
getByConst :: Lens s t a b -> s -> a
getByConst lens s = x
  where
    Const x = lens Const s

-- view :: Lens s t a b -> s -> a
-- view = getByConst

-- Traversals 101
{-
    A lens focuses on a single value – it doesn't matter whether it's actually a single value or not,
    as long as conceptually it is.
    A couple of examples to illustrate what I'm talking about:
-}
------------

{-
We can make a lens for focusing on several elements in the list,
as long as they are equal to the given one:
-}
-- Make a lens out of a getter and a setter.
-- 'lens' :: 'Functor' f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set afb s = set s <$> afb (get s)

_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
    where
        get s = ref
        set s new = map (\old -> if old == ref then new else old) s


-- > set (_all 0) (-8) [100, 600, 0, 200, 0]
-- [100, 600, -8, 200, -8]

-- rewriting _all with an Applicative lens

type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a

_all' :: Eq a => a -> AppLens' [a] a
_all' ref f = traverse update
    where
      update old = if old == ref then f old else pure old

-- (_all' 0) (const $ putStr "? new: " >> readLn) [100, 600, 0, 200, 0]

-- getting to use _all with view, over and set
{-
    view :: Lens s t a b -> s -> a
    over :: Lens s t a b -> (a -> b) -> s -> t
    set  :: Lens s t a b -> b -> s -> t

    -- old type:
    --      ((a ->    f    a) -> s ->    f    s) -> s -> a
    view :: ((a -> Const a a) -> s -> Const a s) -> s -> a

    -- old type:
    --      ((a ->     f    b) -> s ->     f    t) -> (a -> b) -> s -> t
    over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t


-}


type Getting s a = (a -> Const a a) -> s -> Const a s

type Setting s t a b = (a -> Identity b) -> s -> Identity t

view :: Getting s a -> s -> a
view l = getConst . l Const


-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

