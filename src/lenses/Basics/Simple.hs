-- Refrence:http://mchaver.com/posts/2017-07-12-lens-tutorial-1.html
-- This is needed so that we can have constraints in type synonyms.
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Lenses.Basics.Simple where

import Control.Applicative
import Data.Functor.Identity

{- Building custom lens for an ADT -}

type SimpleLens s a = forall f. Functor f => (a -> f a) -> s -> f s

data Person = Person
    { name :: String
    , age  :: Int
    } deriving (Eq, Read, Show)

-- _name :: forall f. Functor f => (String -> f String) -> Person -> f Person
_name :: SimpleLens Person String
_name f Person{..} = (`Person` age) <$> f name

_age :: SimpleLens Person Int
_age f Person{..} = (name `Person`) <$> f age

-- view :: ((a -> f a) -> s -> f s) -> s -> a
view :: SimpleLens s a -> s -> a
view l = getConst . l Const

viewName :: Person -> String
viewName Person{..} = getConst $ (`Person` age) <$> Const name

-- set :: ((a -> f a) -> s -> f s) -> a -> s -> s
set :: SimpleLens s a -> a -> s -> s
set l b = runIdentity . l (\_ -> Identity b)

setName :: String -> Person -> Person
setName b Person{..} = runIdentity $ (`Person` age) <$> Identity b

-- over :: ((a -> f a) -> s -> f s) -> (a -> a) -> s -> s
over :: SimpleLens s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

overName :: (String -> String) -> Person -> Person
overName f Person {..} = runIdentity $ (`Person` age) <$> Identity (f name)

-- over _name (++ " Allan") $ Person "Marina" 21
