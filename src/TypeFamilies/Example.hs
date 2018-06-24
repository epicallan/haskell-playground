{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module TypeFamilies.Example where

data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show

class (Show pokemon, Show (Move pokemon)) => Pokemon pokemon where
    data Move pokemon :: *
    pickMove :: pokemon -> Move pokemon

instance Pokemon Fire where
    data Move Fire = Ember | FlameThrower | FireBlast deriving Show
    pickMove Charmander = Ember
    pickMove Charmeleon = FlameThrower
    pickMove Charizard  = FireBlast

instance Pokemon Water where
    data Move Water = Bubble | WaterGun deriving Show
    pickMove Squirtle = Bubble
    pickMove _        = WaterGun

instance Pokemon Grass where
    data Move Grass = VineWhip deriving Show
    pickMove _ = VineWhip


printBattle :: String -> String -> String -> String -> String -> IO ()
printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
    putStrLn $ pokemonOne ++ " used " ++ moveOne
    putStrLn $ pokemonTwo ++ " used " ++ moveTwo
    putStrLn $ "Winner is: " ++ winner ++ "\n"

class (Pokemon pokemon, Pokemon foe) => Battle pokemon foe where
    battle :: pokemon -> foe -> IO ()
    battle pokemon foe =
        printBattle (show pokemon) (show move) (show foe) (show foeMove) (show pokemon)
        where
        foeMove = pickMove foe
        move = pickMove pokemon

instance Battle Water Fire
instance Battle Fire Water where
    battle = flip battle

instance Battle Grass Water
instance Battle Water Grass where
    battle = flip battle

instance Battle Fire Grass
instance Battle Grass Fire where
    battle = flip battle

mainT :: IO ()
mainT = do
    battle Squirtle Charmander
    battle Charmeleon Wartortle
    battle Bulbasaur Blastoise
    battle Wartortle Ivysaur
    battle Charmeleon Ivysaur
    battle Venusaur Charizard

