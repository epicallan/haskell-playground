{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
-- | Generic Sop paper
module Generic.Sops where

import Data.Kind
import Data.Proxy
import Data.Text

-- | In dependently typed languages a universe consists of a type of
-- of codes together with an interpretation function mapping codes to
-- types. The idea of the SOP universe is that the kinds of codes is a promoted
-- list of types. The universe is represented as the Code type family
-- The goal of Code type family is to map a data type to its "code"

type family Code (a :: Type) :: [[Type]]

-- Example of mapping Expr to its codes
data Expr = Num Int | Add { _left :: Expr , _right :: Expr }

type instance Code Expr = '[ '[Int], '[Expr, Expr]]
-- | The outer list has one element per constructor. For each constructor the corresponding
-- inner list contains the types of the constructor argument

-- | the SOP (sum of products) interpretation of the codes of kind [[Type]] views the outer list
-- as an n-ary sum, representing the choice between the constructors and the
-- inner lists as a n-ary products

newtype I (a :: Type) = I { unI :: a}
newtype K (a :: Type) (b :: k) = K { unK :: a}

-- | SOP I (Code a) is isomorphic to the original datatype a.
-- This isomorphism is captured by the type class Generic

type Rep a = SOP I (Code a) -- SOP is defined below

-- actual definition is SingI (Code a) => Generic (a :: Type) where...
class Generic (a :: Type) where
  from :: a -> Rep a
  to   :: Rep a -> a

-- | If we can define a function that works for all codes then we can turn
-- that function into a data type generic function by making it work on all
-- suitable instances of class Generic

-- | POP f (for products of products) views both the outer and inner type level lists
-- as n-ary products and applies f to all the elements.

infixr 5 :**

-- | In order to translate Haskell values into the SOP universe
-- we need support for n-ary sums and products

data NP' :: [Type] -> Type where
  Nil' :: NP' '[]
  (:**) :: x -> NP' xs -> NP' ( x ': xs)

-- | example with nested pair
type NestedPair = (Bool, (Char, Int))

nestedPairNP ::NP' '[ Bool, Char, Int ]
nestedPairNP = True :** 'x' :** 3 :** Nil'

-- | we however often need a product
-- f T1 * f T2 ... f Tn

-- | so define NP with functor application built in

infixr 5 :*

data NP :: (k -> Type) -> [k] -> Type where
  Nil :: NP f '[]
  (:*) :: f x -> NP f xs -> NP f ( x ': xs)

nestedPairNP' :: NP I '[ Bool, Char, Int]
nestedPairNP' = I True :* I 'x' :* I 3 :* Nil

-- | we can define n-ary sums

data NS :: (k -> Type) -> [k] -> Type where
  Z :: f x -> NS f ( x ': xs)
  S :: NS f xs -> NS f (x ': xs)

{-
 The constructor Z injects into the first component of a sum (with at least one component)
 S . Z into the second component of a sum with at least 2 components
-}
-- | By nesting NS and NP applications we can define both SOP and POP

type SOP (f :: k -> Type) (xss :: [[k]] ) = NS (NP f) xss

type POP (f :: k -> Type) (xss :: [[k]]) = NP (NP f) xss

-- SOP

-- | Arithmetic expressions SOP example

instance Generic Expr where
  from :: Expr -> SOP I (Code Expr)
  from (Num n)   = Z (I n  :* Nil)
  from (Add e f) = S (Z (I e :* I f :* Nil))

  to :: SOP I (Code Expr) -> Expr
  to (Z (I n :* Nil ))           = Num n
  to (S (Z (I e :* I f :* Nil))) = Add e f
  to (S (S _ ))                  = error "GHC error"


-- | Lists example
data List a = NilL | ConsL a (List a)

type instance Code (List a) = '[ '[ ], '[ a, List a ] ]

type instance Code [ a ] = '[ '[], '[ a, [ a ] ]]

instance Generic [a] where
  from :: [a] -> SOP I (Code [ a ])
  from []       = Z Nil
  from (x : xs) = S (Z (I x :* I xs :* Nil))

  to :: SOP I (Code [ a ]) -> [a]
  to (Z Nil)                      = []
  to (S (Z (I x :* I xs :* Nil))) = x : xs
  to (S (S _))                    = error "GHC error"

-- | There is template haskell for automatic derivation

-- Constructing products (POP)
{-
we need to treat NP values as applicatives and for that we need a pure function

 pure :: forall a . f a -> NP (f a) xs

the pure function would create NP for a given element for as many times as xs, this
requires performing induction over xs which subsequently requires using singletons
-}

data family Sing (a :: k) :: Type

data instance Sing ( a :: [k]) where
  SNil :: Sing '[]
  SCons :: (SingI x, SingI xs) => Sing (x ': xs)

data instance Sing (a :: Type) where
  SStar :: Sing (a :: Type)

class SingI (a :: k) where
  sing :: Sing a

instance SingI (a :: Type) where
  sing = SStar

instance SingI '[] where
  sing = SNil

instance (SingI k, SingI ks) => SingI (k ': ks) where
  sing = SCons

purenp :: forall f xs. SingI xs => (forall a. f a) -> NP f xs
purenp fx = case (sing :: Sing xs) of
  SNil  -> Nil
  SCons -> fx :* purenp fx
  -- ^ type matching on SCons brings into scope
  -- (SingI x, SingI xs) where x is of kind k and xs is of kind [k]
  -- purenp requires SingI xs and it ends up using it, therefore recursively
  -- reducing the computation

purepop :: forall f xss . SingI xss => (forall a. f a) -> POP f xss
purepop fx = case (sing :: Sing xss) of
  SNil  -> Nil
  SCons -> purenp fx :* purepop fx


-- | we often require to use a value of a with a constraint c in the definition for pure

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All _c '[] = ()
  All c (x ': xs) = (c x, All c xs)

cpure :: forall c f xs. (All c xs, SingI xs) => (forall a. c a => f a) -> NP f xs
cpure fx = case (sing :: Sing xs) of
  SNil  -> Nil
  SCons -> fx :* cpure @c fx

-- | generalizing to pop and sop definition

type family All2 (c :: k -> Constraint) (xs :: [[k]]) :: Constraint where
  All2 _c '[] = ()
  All2 c ( x ': xs ) = (All c x, All2 c xs)

cpurepop :: forall c f xss. (All2 c xss, SingI xss) => (forall a. c a => f a) -> POP f xss
cpurepop fx = case (sing :: Sing xss) of
  SNil  -> Nil
  SCons -> cpure @c fx :* cpurepop @c fx

-- | defining ap (applicative) is analogous to applying a product of functions to a product of arguments
-- to do this we need  to define a lifted function space
infixr 0 ~>
-- | a lifted function space
newtype  (f ~> g) a = Fn {apFn :: f a -> g a}

-- | for convenience we define auxiliary constructors for lifted functions with several arguments

fn2 :: (f a -> f' a -> f'' a) -> (f ~> f' ~> f'') a
fn2 = undefined

fn3 ::  (f a -> f' a -> f'' a -> f''' a) -> (f ~> f' ~> f'' ~> f''') a
fn3 = undefined

apnp :: NP (f ~> g) xs -> NP f xs -> NP g xs
apnp Nil Nil                = Nil
apnp (Fn f :* fs) (x :* xs) = f x :* apnp fs xs

apns :: NP (f ~> g) xs -> NS f xs -> NS g xs
apns (Fn f :* _) (Z x) = Z (f x)
apns (_ :* fs) (S xs)  = S (apns fs xs)
apns Nil _             = error "ghc error"

appop :: POP (f ~> g) xs -> POP f xs -> POP g xs
appop = undefined

apsop :: SOP (f ~> g) xs -> SOP f xs -> SOP g xs
apsop = undefined

-- | armed with pure and ap we can define a host of derived functions

liftAnp :: SingI xs => (forall a. f a -> g a) -> NP f xs -> NP g xs
liftAnp f xs = purenp (Fn f) `apnp` xs

-- | Metadata in SOP
-- The TypeInfo data type is yet another interpretation of SOP codes.

type Name = Text

data TypeInfo :: [[Type]] -> Type where
  ADT :: Name -> NP ConInfo xss -> TypeInfo xss
  New :: Name -> ConInfo '[x] -> TypeInfo '[ '[ x ] ]

data ConInfo :: [Type] -> Type where
  Con :: SingI xs => Name -> ConInfo xs
  Rec :: SingI xs => Name -> NP (K Name) xs -> ConInfo xs

class HasTypeInfo a where
  typeInfo :: Proxy a -> TypeInfo (Code a)


instance HasTypeInfo Expr where
  typeInfo _ = ADT "Expr" $
       Con "Num"
    :* Rec "Add" (K "_left" :* K "_right" :* Nil)
    :* Nil
