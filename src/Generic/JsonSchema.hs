{-# LANGUAGE AllowAmbiguousTypes #-}
module Generic.JsonSchema where

import Control.Monad.Writer
import Data.Aeson
import Data.Kind
import Data.Text

{-

Given a Person type its Json Schema can be evaluated as below type

{ "required": ["name", "age", "permissions"],
  "title": "Person",
  "type": "object",
  "properties": {
     "name": { "type": "string" } ,
     "age": {"type":"integer"} ,
     "phone": { "type": "string" } ,
     "permissions": { "type": "array", "items": { "type": "boolean" }
}

-}

class GSchema (a :: Type -> Type) where
  gschema :: Writer [Text] Value

{-
 newtype M1 (i :: Type) (c :: Meta) (f :: k -> Type) (p :: k)
 newtype M1  i t f p = M1 { unM1 :: f p }  -- a wrapper
-}

