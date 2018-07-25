{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 708
#error "requires GHC 7.10 or newer"
#endif
module Overlap.Notes (main) where

import qualified Data.Text as T
import Data.Type.Equality
import Unsafe.Coerce

-------------------------------------------------------------------------------
-- OverlappingInstances
-------------------------------------------------------------------------------

-- | Example class counting things
--
-- With special instance for 'String' which counts words, not characters
class Count a where
     count :: a -> Int

countListDefault :: [a] -> Int
countListDefault = length

instance {-# OVERLAPPABLE #-} Count [a] where
     count = countListDefault

-- This requires @FlexibleInstances@
instance {-# OVERLAPPING #-} Count [Char] where
    count = length . words

-- |
-- >>> test1
-- 2
test1 :: Int
test1 = count "foo bar"

-- |
-- >>> test2
-- 3
test2 :: Int
test2 = count [True, False, True]

{-
Polymorphic use

> poly :: [a] -> Int
> poly = count

is problematic:
    Overlapping instances for Count [a] arising from a use of ‘count’
    Matching instances:
      instance [overlappable] Count [a] -- Defined at overlap.hs:13:31
      instance [overlapping] Count [Char] -- Defined at overlap.hs:17:30
-}

-------------------------------------------------------------------------------
-- Incoherent instances
-------------------------------------------------------------------------------

class CountI a where
     countI :: a -> Int

instance CountI [a] where
     countI = length

instance {-# INCOHERENT #-} CountI [Char] where
    countI = length . words

-- |
-- >>> testI_1
-- 2
testI_1 :: Int
testI_1 = countI "foo bar"

-- |
-- >>> testI_2
-- 3
testI_2 :: Int
testI_2 = countI [True, False, True]

{-
Now we can write 'polyI' function. And it uses generic [a] instance.
The rules are somehow complicated. It's chosen because the rule

"If exactly one non-incoherent candidate remains, select it."

apply.

See <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances>
-}

-- |
-- >>> polyI "foo bar"
-- 7
polyI :: [a] -> Int
polyI = countI

-- | Note that we can defer the selection of instance:
--
-- >>> polyI' "foo bar"
-- 2
polyI' :: CountI [a] => [a] -> Int
polyI' = countI

-- This scales also if we wan't to add more "special" instances
--
-- /Note:/ that polyI still works.
instance {-# INCOHERENT #-} CountI [Int] where
    countI = sum

intlist :: [Int]
intlist = [1..4]

-- |
-- >>> exampleSumI
-- 10
exampleSumI :: Int
exampleSumI = countI intlist

---------------------------------------------------------------------------------
-- Read/Show approach
-------------------------------------------------------------------------------

{-
This is Haskell98 definition, which is good thing.

But there are two problems:

* class definition is more complicated

* Need to define instances for Char and Bool, yet we don't reealy need them.
  Generally we have to define instances for each element type we might use.
  It's not a problem for classes where per element instances are meaningful,
  but maybe not for our 'Count'.
-}

class CountRS a where
    countRS :: a -> Int
    countRS _ = 1
    countRSList :: [a] -> Int
    countRSList = countListDefault

instance CountRS Char where
    countRSList = length . words

instance CountRS a => CountRS [a] where
    countRS = countRSList

instance CountRS Bool

-- |
-- >>> testRS_1
-- 2
testRS_1 :: Int
testRS_1 = countRS "foo bar"

-- |
-- >>> testRS_2
-- 3
testRS_2 :: Int
testRS_2 = countRS [True, False, True]

-- |
-- >>> polyRS "foo bar"
-- 7
polyRS :: [a] -> Int
polyRS = countListDefault

-- |
-- >>> polyRS' "foo bar"
-- 2
polyRS' :: CountRS a => [a] -> Int
polyRS' = countRS

-- Adding special case for [Int] works as well:

-- This scales also if we wan't to add more "special" instances
--
-- /Note:/ that polyI still works.
instance CountRS Int where
    countRSList = sum

-- |
-- >>> exampleSumRS
-- 10
exampleSumRS :: Int
exampleSumRS = countRS intlist

-- Adding special case for some other container, e.g. @Maybe Int@
-- would require adding new member to the class.

-------------------------------------------------------------------------------
-- Data.Type.Equality
-------------------------------------------------------------------------------

-- Type level equality approach
--
-- Con: requires many extensions

class CountE a where
    countE :: a -> Int

instance SBoolI (a == Char) => CountE [a] where
    countE = case sbool :: SBool (a == Char) of
        STrue  -> gcastWith (eqToRefl :: a :~: Char) $ length . words
        SFalse -> countListDefault

eqToRefl :: forall a b. (a == b) ~ 'True => a :~: b
eqToRefl = unsafeCoerce (Refl :: () :~: ())

-- |
-- >>> testE_1
-- 2
testE_1 :: Int
testE_1 = countE "foo bar"

-- |
-- >>> testE_2
-- 3
testE_2 :: Int
testE_2 = countE [True, False, True]

-- |
-- >>> polyE "foo bar"
-- 7
polyE :: [a] -> Int
polyE = countListDefault

-- |
-- >>> polyE' "foo bar"
-- 2
polyE' :: SBoolI (a == Char) => [a] -> Int
polyE' = countE

-- | We have ability to write function which could accept all lists,
-- except @[Char] ~ String@
--
-- >>> polyE'' [(), (), ()]
-- 3
--
--  > polyE'' "foo"
-- <interactive>:55:1:
--    Couldn't match type ‘'True’ with ‘'False’
polyE'' :: (a == Char) ~ 'False => [a] -> Int
polyE'' = countE

-- Now it's relatively easy to add special treatment of 'Maybe Int' instance

-- |
-- >>> countE (Just 'a')
-- 1
--
-- >>> countE (Just (5 :: Int))
-- 5

-------------------------------------------------------------------------------
-- singgleton Bool
-------------------------------------------------------------------------------

data SBool (b :: Bool) where
    STrue  :: SBool 'True
    SFalse :: SBool 'False

class    SBoolI b      where
    sbool :: SBool b
instance SBoolI 'True  where
    sbool = STrue
instance SBoolI 'False where
    sbool = SFalse

instance SBoolI (a == Int) => CountE (Maybe a) where
    countE = case sbool :: SBool (a == Int) of
        STrue  -> gcastWith (eqToRefl :: a :~: Int) $ maybe 0 id
        SFalse -> maybe 0 (const 1)

-- OTOH it's requires change of CountE [a] definition to add special treatment
-- of [a]

-------------------------------------------------------------------------------
-- Bonus, when you don't care about containers:
-------------------------------------------------------------------------------

class CountT a where
    countT :: a -> Int

-- | The only valid instance for lists is lists of chars
instance a ~ Char => CountT [a] where
    countT = length . words

instance CountT T.Text where
    countT = length . T.words

-- |
-- >>> testT_1
-- 2
testT_1 :: Int
testT_1 = countT "foo bar"

{-
> testT_2 :: Int
> testT_2 = countT [True, False, True]

overlap.hs:284:11:
  Couldn't match type ‘Bool’ with ‘Char’
-}

-- |
-- >>> testT_3
-- 3
testT_3 :: Int
testT_3 = countT $ T.pack "foo bar baz"


-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = pure ()
