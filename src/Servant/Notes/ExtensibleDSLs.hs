module Servant.Notes.ExtensibleDSLs where

{-
How can we reasonably easily (that is, in Haskell 98) achieve full extensibility in both directions
(constructors and interpretations) in Haskell. It boils down to:

- Turn what would be a constructor into its own little data type.
- Turn what would be a simple function that operates on the data type into a typeclass with a method.
- Write instances of those typeclasses for the data types representing the DSL’s constructs.
-}

-- our expression constructs, one data type per
-- constructor we had previously.
data I = I Integer

data Add a b = Add a b

-- an open union to describe valid types
class Expr a

instance Expr I
instance (Expr a, Expr b) => Expr (Add a b)

-- our first interpretation

class Expr a => Eval a where
  eval :: a -> Integer

instance Eval I where
  eval (I x) = x

instance (Eval a, Eval b) => Eval (Add a b) where
  eval (Add x y) = eval x + eval y


-- our second interpretation, pretty printing
class Expr a => Pretty a where
  pretty :: a -> String

instance Pretty I where
  pretty (I n) = show n

instance (Pretty l, Pretty r) => Pretty (Add l r) where
  pretty (Add a b) = unwords [pretty a, "+", pretty b]

-- to support multiplication in another module

data Mul l r = Mul l r
instance (Expr l, Expr r) => Expr (Mul l r)

instance (Eval l, Eval r) => Eval (Mul l r) where
  eval (Mul a b) = eval a * eval b
