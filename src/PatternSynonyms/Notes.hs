{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module PatternSynonyms.Notes where

-- | Good resource  https://haskell-explained.gitlab.io/blog/posts/2019/08/27/pattern-synonyms/index.html 

-- | from GHC docs

data Type = App String [Type]

pattern Arrow :: Type -> Type -> Type
pattern Arrow t1 t2 = App "->" [t1, t2]

pattern Int :: Type
pattern Int = App "Int" []

pattern Maybe :: Type -> Type
pattern Maybe t = App "Maybe" [t]

collectArgs :: Type -> [Type]
collectArgs (Arrow t1 t2) = t1 : collectArgs t2
collectArgs _             = []


isInt :: Type -> Bool
isInt  = \case
  Int -> True
  _   ->  False


-- | from school of haskell blogpost
-- https://www.schoolofhaskell.com/user/icelandj/Pattern%20synonyms

data Date = Date { month :: Int, day :: Int } deriving Show


-- Months
pattern January :: Int -> Date
pattern January day = Date { month = 1, day = day}

pattern February :: Int -> Date
pattern February day = Date { month = 2, day = day }

pattern March :: Int -> Date
pattern March day = Date { month = 3, day = day }

pattern December :: Int -> Date
pattern December day = Date { month = 12, day = day}

-- Holidays
pattern Christmas :: Date
pattern Christmas = December 25

describe :: Date -> String
describe = \case
  January 1 -> "First Day of the year"
  February n -> show n <> " th of February"
  Christmas -> "Presents"
  _ -> "meh"

pattern BeforeChristmas :: Date
pattern BeforeChristmas <- December (compare 25 -> GT)

pattern AfterChristmas :: Date
pattern AfterChristmas <- December (compare 25 -> LT)

pattern Christmax :: Date
pattern Christmax <- December (compare 25 -> EQ)


react :: Date -> String
react = \case
  BeforeChristmas -> "Waiting"
  AfterChristmas -> "Done Xmas"
  Christmax -> "Presents"
  _ -> "Its not December"


isItXmas :: Int -> (Ordering, Int)
isItXmas day = (compare 25 day, day)

pattern BeforeXmas :: Int -> Date
pattern BeforeXmas day <- December (isItXmas -> (GT, day))

pattern AfterXmas :: Int -> Date
pattern AfterXmas day <- December (isItXmas -> (LT, day))

pattern Xmax :: Date
pattern Xmax <- December (isItXmas -> (EQ, _))

daysTilChristmas :: Date -> Int
daysTilChristmas (BeforeXmas n) = 25 - n
daysTilChristmas (AfterXmas n)  = 365 + 25 - n
daysTilChristmas Xmax           = 0
daysTilChristmas _              = error "Should provide a December date"
