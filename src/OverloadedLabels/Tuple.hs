{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module OverloadedLabels.Tuple where
import Data.Kind
import Data.Proxy
import GHC.OverloadedLabels
import GHC.TypeLits

data (a :: Symbol) := (b :: Type) where
  (:=) :: Proxy a -> b -> a := b

instance (Show t, KnownSymbol l) => Show ( l := t) where
  show (l := t) = symbolVal l <> " := " <> show t

instance {-# OVERLAPPING #-} (KnownSymbol l) => Show (l := String) where
  show (l := t) = symbolVal l <> " := " <> t

class Has (l :: Symbol) r a | l r -> a where
  get :: r -> a

-- Instances which we could easily generate with TH.
instance Has l (l := a) a where get (_ := a) = a
instance Has l ((l := a), u0) a where get ((_ := a),_) = a
instance Has l (u0, (l := a)) a where get (_,(_ := a)) = a
instance Has l ((l := a), u0, u1) a where get ((_ := a),_,_) = a
instance Has l (u0, (l := a), u1) a where get (_,(_ := a),_) = a
instance Has l (u0, u1, (l := a)) a where get (_,_,(_ := a)) = a

instance IsLabel symbol (Proxy symbol) where
  fromLabel = Proxy @symbol

instance Has l r a => IsLabel l (r -> a) where
  fromLabel = get @l

type Address = ("city" := String, "street" := String)

address :: Address
address = (#city := "Kampala", #street := "mutundwe")

somePerson :: ("name" := String, "age" := Int, "address" := Address)
somePerson = (#name := "Allan", #age := 16, #address := address)

myAge :: Has "age" r Int => r -> Int
myAge = #age

main :: IO ()
main = do
  print $ myAge somePerson
  putStrLn $ #name somePerson
  putStrLn . #street . #address $ somePerson
