-- Evaluating use of various type level techniques on a small problem

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}

module TypesEx.Distance where

-- phantom types usage

newtype DistanceP a = DistanceP Double deriving (Num, Show)

data Kilometer
data Mile

marathonDistance :: DistanceP Kilometer
marathonDistance = DistanceP 42.195

distanceKmToMiles :: DistanceP Kilometer -> DistanceP Mile
distanceKmToMiles (DistanceP km) = DistanceP (0.621371 * km)

marathonDistanceInMiles :: DistanceP Mile
marathonDistanceInMiles = distanceKmToMiles marathonDistance


-- data kinds example / usage

data LengthUnit = KM | Mile

newtype DistanceK (a :: LengthUnit) = DistanceK Double
  deriving (Num, Show)

marathonDistance' :: DistanceK 'KM
marathonDistance' = DistanceK 42.195

distanceKmToMiles' :: DistanceK 'KM -> DistanceK 'Mile
distanceKmToMiles' (DistanceK km) = DistanceK (0.621371 * km)

marathonDistanceInMiles' :: DistanceK 'Mile
marathonDistanceInMiles' = distanceKmToMiles' marathonDistance'

-- using GADTs

data Distance a where
    KMDistance :: Double -> Distance Kilometer
    MileDistance :: Double -> Distance Mile


data DistanceF :: (LengthUnit -> * ) where
  KDistance :: Double -> DistanceF 'KM
  MDistance :: Double -> DistanceF 'Mile


marathonDistance'' :: Distance Kilometer
marathonDistance'' = KMDistance 42.195

distanceKmToMiles'' :: Distance Kilometer -> Distance Mile
distanceKmToMiles'' (KMDistance km) = MileDistance (0.621371 * km)


marathonDistanceInMiles'' :: Distance Mile
marathonDistanceInMiles'' = distanceKmToMiles'' marathonDistance''

kmDistance :: DistanceF 'KM
kmDistance = KDistance 43.3

