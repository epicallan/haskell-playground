> {-# LANGUAGE TemplateHaskell #-}
> module Prisms.Notes where
> import Universum

references
- https://blog.jle.im/entry/lenses-products-prisms-sums.html
- https://artyom.me/lens-over-tea-5


A lens is a description of a product
Take for instance
Person <-> (firstName, lastName) which is isomorphic to a Person Record can be
described as a Product.

The description of how to access the product values formulates a lens
`lens' Person firstName i.e lens' s a`

A Lens' s a is nothing more than a witness for the fact that there exists some q where s <~> (a, q).

-- | s <~> (a, q)

````
data Lens' s a = forall q. Lens'
    { split   :: s -> (a, q)
    , unsplit :: (a, q) -> s
    }
````
Now, if split and unsplit form an isomorphism, this can only represent valid lenses!2

ie `split . unsplit = id`

data Person = P
    { _pName :: String
    , _pAge  :: Int
    }

Because Person is a product between String and Int, we get two lenses:
`Lens' Person String` and `Lens' Person Int`.
Every product gives us a lens for every item in the product.

-- Person <~> (String, Int)


pName :: Lens' Person String
pName = Lens'
    { split   = \(P n a) -> (n, a)
    , unsplit = \(n, a)  -> P n a
    }

pAge :: Lens' Person Int
pAge = Lens'
    { split   = \(P n a) -> (a, n)
    , unsplit = \(a, n)  -> P n a
    }

So that’s really the essence of what a Lens' is.
A Lens' s a is the embodiment of the fact that s can be represented as a product between a and something else — that s <~> (a, q). All of the lens laws just boil down to this. Lenses embody products.


Whereas a Lens' s a is nothing more than a witness to the fact that s is a product (a, q) …
a Prism' s a is nothing more than a witness to the fact that s is a sum Either a q.
If it is possible to represent s as some Either v w…then you have two prisms!
Prisms are nothing more than descriptions of sums! If you are able to “split” a type into one of two possibilities,
then each possibility represents a prism.

A Prism' s a is nothing more than saying that there exists some type q that can be used to witness a s <~> Either a q isomorphism.

Under this interpretation, we can write a nice representation of Prism':

```
-- | s <~> Either a q


data Prism' s a = forall q. Prism'
    { match  :: s -> Either a q
    , inject :: Either a q -> s
    }
```
