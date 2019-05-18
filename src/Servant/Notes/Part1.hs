module Servant.Notes.Part1 where

import Data.Kind (Type)
-- https://www.servant.dev/posts/2018-07-12-servant-dsl-typelevel.html

{-
 Naive approach to creating servant like DSL
-}

data Method = Get | Post

data EndPoint =
    Static String EndPoint
  | Capture EndPoint
  | Verb Method

-- Get / hello/:name

getHello_ :: EndPoint
getHello_ = Static "hello" (Capture (Verb Get))

-- Making it more servanty

infixr 5 **>
(**>) :: (EndPoint -> EndPoint) -> EndPoint -> EndPoint
f **> x = f x

getHello :: EndPoint
getHello = Static "hello" **> Capture **> Verb Get

type Link  = [String] -- a list of path components

{-
`linkTo` to be Endpoint -> Link, Endpoint -> String -> Link, Endpoint -> String -> String -> Link and so on
depending on what the Endpoint argument is. In other words, we want the return type of linkTo (when really seen as a function of one argument, which it is anyway) to depend on the value of type Endpoint it gets as input.
That is, we want a type that depends on a value, i.e dependent types.
-}

-- | A naive approach
-------------------------
-- A linkTo that assumes that the capture values are given in the same order as we want
-- them to appear in the url.

linkTo :: EndPoint -> [String] -> Link
linkTo (Static str rest) captureValues = str : linkTo rest captureValues

linkTo (Capture rest) (c:cs) = c : linkTo rest cs

linkTo (Capture _) [] = error "linkTo capture values needed"

linkTo (Verb _) _ = []


-- exercise
{-
Fortunately, GADTs can help here. We could turn Endpoint into a GADT that tracks captures and then use some type-level computations to get the type of the link-making function from our list of captures, as well as define the link making function through typeclass instances that would go through the captures and add an argument for each of them. Request bodies, query parameters, headers? We could probably track them too, in a similar way.
-}
