{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lenses.Tutorial where

import Universum

import Control.Lens hiding (Traversal')

{-
   Lens' a b is actually a type synonym for a certain type of higher-order function:

    type Lens' a b =
        forall f . (Functor f) => (b -> f b) -> (a -> f a)

    Lens' a b is a higher-order function that accepts a function of type (b -> f b) as an argument,
    and returns a new function of type (a -> f a)

    boss :: Lens' Game Unit
    -- expands to:
    boss :: (Functor f) => (Unit -> f Unit) -> (Game -> f Game)

    health :: Lens' Unit Int
    -- expands to:
    health :: (Functor f) => (Int -> f Int) -> (Unit -> f Unit)

    (.) :: (b -> c) -> (a -> b) -> (a -> c)
    (f . g) x = f (g x)

    Notice that if we specialize our type variables to:

    a ~ (Int  -> f Int)
    b ~ (Unit -> f Unit)
    c ~ (Game -> f Game)

    (.) :: ((Unit -> f Unit) -> (Game -> f Game))
    -> ((Int  -> f Int ) -> (Unit -> f Unit))
    -> ((Int  -> f Int ) -> (Game -> f Game))

 -}

data Game = Game
    { _score :: Int
    , _units :: [Unit]
    , _boss  :: Unit
    } deriving (Show)

data Unit = Unit
    { _health   :: Int
    , _position :: Point
    } deriving (Show)

data Point = Point
    { _x :: Double
    , _y :: Double
    } deriving (Show)

makeLenses ''Game
makeLenses ''Unit
makeLenses ''Point

initialState :: Game
initialState = Game
    { _score = 0
    , _units =
        [ Unit
            { _health = 10
            , _position = Point { _x = 3.5, _y = 7.0 }
            }
        , Unit
            { _health = 15
            , _position = Point { _x = 1.0, _y = 1.0 }
            }
        , Unit
            { _health = 8
            , _position = Point { _x = 0.0, _y = 2.1 }
            }
        ]
    , _boss = Unit
        { _health = 100
        , _position = Point { _x = 0.0, _y = 0.0 }
        }
    }

strike :: StateT Game IO ()
strike = do
    liftIO $ putTextLn "*shink*"
    boss . health -= 10

-- >>> execStateT strike initialState

{-
traversed lets us "dig in" to the values in a list so that we can manipulate them
as a single unit instead of manually looping over the list.

traversed :: Traversal' [a] a

type Traversal' a b =
    forall f . (Applicative f) => (b -> f b) -> (a -> f a)

If you compose Lens' with a Traversal', you get the weaker of the two: a Traversal'.
-}

partyHP :: Traversal' Game Int
partyHP = units . traversed . health

fireBreath :: StateT Game IO ()
fireBreath = do
    liftIO $ putTextLn "*raw*"
    -- units . traversed . health -= 3
    partyHP -= 3
{-
    to get values from a traversal you use specific functions
    (^..) ::   Traversal' a b -> a -> [b]
    toListOf :: Traversal' a b -> a -> [b]

    >>> toListOf partyHP newState
-}


-- With lenses we can zoom in on subsets of our global state:

partyLoc :: Traversal' Game Point
partyLoc = units.traversed.position

retreat :: StateT Game IO ()
retreat = do
    lift $ putTextLn "Retreat!"
    zoom partyLoc $ do
        x += 10
        y += 10

-- you can access points as below
-- initialState ^.. partyLoc

battle :: StateT Game IO ()
battle = do
    -- Charge!
    forM_ ["Take that!", "and that!", "and that!"] $ \taunt -> do
        liftIO $ putTextLn taunt
        strike

    -- The dragon awakes!
    fireBreath

    replicateM_ 3 $ do
        -- The better part of valor
        retreat

        -- Boss chases them
        zoom (boss.position) $ do
            x += 10
            y += 10
