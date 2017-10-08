{-# LANGUAGE MultiParamTypeClasses #-}
module TypeFamilies where


data Fire = Charmander | Charmeleon | Charizard deriving Show
data Water = Squirtle | Wartortle | Blastoise deriving Show
data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show

data FireMove = Ember | FlameThrower | FireBlast deriving Show
data WaterMove = Bubble | WaterGun deriving Show
data GrassMove = VineWhip deriving Show

-- show
class (Show pokemon, Show move) => Pokemon pokemon move where
    pickMove :: pokemon -> move

instance Pokemon Fire FireMove where
    pickMove Charmander = Ember
    pickMove Charmeleon = FlameThrower
    pickMove Charizard  = FireBlast

instance Pokemon Water WaterMove where
    pickMove Squirtle = Bubble
    pickMove _        = WaterGun

instance Pokemon Grass GrassMove where
    pickMove _ = VineWhip

mainT :: IO ()
mainT = do
    print (pickMove Charmander :: FireMove)
    print (pickMove Blastoise :: WaterMove)
    print (pickMove Bulbasaur :: GrassMove)
-- show /
