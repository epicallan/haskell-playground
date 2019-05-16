{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
-- | examples from ghc wiki
module TypeFamilies.Printf where

import Prelude hiding (lookup)

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits

-- | Type constructor for generating an Heterogeneous kinded linked list
data (a :: k1) :<< (b :: k2)
infixr 5 :<<

class HasPrintf a where
  type Printf a :: Type
  format
    :: String -- ^ accumulator
    -> Proxy a
    -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
  type Printf text = String
  format s _ = s <> symbolVal (Proxy @text)

instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a) where
  type Printf (text :<< a) = Printf a
  format s _ = format (s <> symbolVal(Proxy @text)) (Proxy @a)

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
  type Printf (param :<< a) = param -> Printf a
  -- type of Printf changes to a allow us take an extra parameter
  format s _ param = format (s <> show param) (Proxy @a)

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
  type Printf (String :<< a) = String -> Printf a
  format s _ param = format (s <> param) (Proxy @a)

-- walk through of types for expression below:

-- Printf(Int :<< ":" :<< Bool :<< "!")
-- initially translates to: (param :<< a) = Int -> Printf (":" :<< Bool :<< "!")
-- ":" :<< Bool :<< "!" == Printf (Bool :<< "!")
-- intermediate stage is Int -> Print (Bool :<< "!") = Int -> Bool -> String


-- format "-- " Proxy @"test" == String -> Proxy a -> (String == Printf @"test")
-- evaluation is "--" <> "test"

-- format "--" Proxy(@(Int :<< "+" :<< Int :<< "=3")) 1 2 == String -> Proxy a -> (Int -> Int -> String)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

testPrintF :: IO ()
testPrintF = do
  putStrLn $ printf (Proxy @"test")
  putStrLn $ printf (Proxy @(Int :<< "+" :<< Int :<< "=3")) 1 2
  -- without the string instance hello would be wrapped in quotes
  putStrLn $ printf (Proxy @(String :<< " world!")) "hello"
