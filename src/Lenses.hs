{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import           Control.Lens

data Point = Point
    { _positionX :: Double
    , _positionY :: Double
    } deriving (Show)
makeLenses ''Point

data Segment = Segment
    { _segmentStart :: Point
    , _segmentEnd   :: Point
    } deriving (Show)
makeLenses ''Segment

makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)
