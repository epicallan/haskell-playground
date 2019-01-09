-- source https://markkarpov.com/tutorial/th.html
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}

module TemplateHaskell.Basic where

import Control.Monad
import Data.Proxy
import Data.Data (Data)
import Data.String (IsString (..))
import Data.Typeable (cast, Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Syntax


{-
As it exists today, Template Haskell has two main areas of application:
- Haskell code generation at compile-time
- facilitating the embedding of domain specific languages.

common use cases
__________

- Automatic deriving of type class instances
- Creation of TH DSLs that are integrated into systems built in Haskell.
 Examples of such DLSs are the language for model declaration used in persistent,
- Compile-time construction of values of refined types that turns invalid inputs into compilation failures.
- Compile-time loading and processing of data from external files,

The Q monad
____________________

- The abilities required to use TH are provided through the Q monad short for quotation

In TH The Q monad can have the below valye types

- Declaration (Dec) -- top level function and data type definitions
- Expression (Exp) -- Expressions such as x + 1 or \x -> x + 1
- Type (Type) such as Int
- Pattern (Pat) used for pattern matching
-}

myFunc :: Q Exp
myFunc = do
    x <- newName "x" -- generate a unique variable name, we'll cover names later
    return $ LamE -- lambda expression
        [VarP x] -- pattern matching on 'x'
        (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))
        -- here we have an infix expression: we apply (+) to 'x' and integer
        -- literal 1


-- λ> :set -XTemplateHaskell -- don't forget to enable the extension
-- λ> $(myFunc) 3

{-

Quotation
________________

AST of arbitrary haskell code can be got by using quotation

Thing produced | Quotation Syntax | Type
_______________|__________________|_________
Declaration    | [d| ... |]       | Q [Dec]
Expression     | [e|..|]          | Q Exp
Type           | [t|..|]          | Q Type
Pattern        | [p|..|]          | Q Pat

[|..|] is a more light weitght quote syntax that evaluates to Q Exp

Not only quotation can be used to quickly discover representation of a piece of Haskell code,
it can be used in place of manually constructed ASTs:

eg

-}

myFunc' :: Q Exp
myFunc' = [|(+ 1)|]

-----
{-
 The most wonderful thing about quoters is that we can actually se splicing inside them
-}

add2 :: Q Exp
add2 = [| $myFunc' . $myFunc'|]

-- $add2 3
-- 5

{-
use `runQ add2` to obtain AST

runQ :: Quasi m => Q a -> m a

Quasi is the type class for monads that provide all the capabilities for meta-programming

-}

{-
Naming
_______________
    - to quote a function name add a single quote eg id -> 'id
    - to quote a type, add 2 single quotes MyRecord -> ''MyRecord

eg
    makeLenses :: Name -> Q [Dec]
    spliced as: makeLenses ''MyRecord

    in the AST for the infix expression involving quoted (+) function

    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))

A way to introduce capturable name is via mkName :: string -> Name function

λ> runQ [| $(varE (mkName "x")) + 1|]
InfixE (Just (VarE x)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))
λ> let xPlus1 = it
λ> let x = 99 in $(return xPlus1) -- value of variable named 'x' influences
                                  -- the result of evaluation

-}

{-
    Retrieving information about names in scope can be done by reifying functions ie;
    reify, extsEnabled, isExtEnabled etc

    Example instance generation;
-}

-- Suppose we want to know how many different non-bottom values inhabit a type.
-- We could start first without TH like this:

class Countable a where
    count :: Proxy a -> Integer


instance (Enum a, Bounded a) => Countable a where
    count Proxy = fromIntegral $
        1 + fromEnum (maxBound :: a) - fromEnum (minBound :: a)

{-
we can do better by writing a TH helper handling 2 cases
    - if a type is an instance of Enum and Bounded then generate instances as above
    - otherwise anlysze and compute for value for the type component types
-}

deriveCountableSimple :: Name -> Q [Dec]
deriveCountableSimple name = [d|
    instance Countable $a where
        count Proxy = fromIntegral $
            1 + fromEnum (maxBound :: $a) - fromEnum (minBound :: $a)
    |]
    where
        a = conT name -- conT = return . ContT
        -- ConT is a dta constructor of Type

-- Testing, import into a new file
------------------------
-- deriveCountableSimple ''Bool

-- λ> count (Proxy :: Proxy Bool)
-- 2
-- λ> count (Proxy :: Proxy Word8)
-- 256
-- λ> count (Proxy :: Proxy Char)
-- 1114112

deriveCountableComposite :: Name -> Q [Dec]
deriveCountableComposite name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  [d|
     instance Countable $(conT name) where
       count Proxy = $(foldr addE [| 0 |] $ f <$> cons')
   |]
  where
    f (NormalC _ ts) = handleCon (snd <$> ts)
    f (RecC    _ ts) = handleCon (thd <$> ts)
    f _              = fail "unsupported data type"
    handleCon ts = foldr mulE [| 1 |] (countTypeE <$> ts)
    countTypeE t = [| count (Proxy :: Proxy $(return t)) |]
    addE x y     = [| $x + $y |]
    mulE x y     = [| $x * $y |]
    thd (_,_,x)  = x

{-
Testing

data Foo
  = Foo Bool Bool

deriveCountableComposite ''Foo

λ> count (Proxy :: Proxy Foo)
4 -- = 2 + 2

-}

{-
 Generated code can be seen with -ddump-splices flag in GHC
 In stack, also add -ddump-to-file and look in .stack-work/dist for splices files
-}

{-

Lifting Haskell values to TH expressions
_________

This could be used to deliver values generated in the Q monad to the outside world.

class Lift t where
  lift :: t -> Q Exp

instance Lift Integer where
  lift x = return (LitE (IntegerL x))


The template-haskell package defines Lift instances for all common data types.
GHC also knows how to define Lift for new types.
It is enough to enable the DeriveLift language extension and we’re all done
-}

data Bar a
    = Bar1 a (Bar a)
    | Bar2 String
    deriving Lift


{-
    For values of types that don't derive Lift we can use `liftData` for any type
    that's an instance of the Data type class eg Text

    liftData :: Data a => a -> Q Exp
-}

{-

foo :: Text -> Q Exp
foo txt = [|$(liftData txt) <> "!"|]

un-fotunately this will blow up into errors when used in a separate module without Text.pack
-}

liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

{-
When Text is inside a data structure below is the more general solution
-}

foo :: Text -> Q Exp
foo txt = [| $e <> "!" |]
  where
    e = dataToExpQ (fmap liftText . cast) txt

{-
 Using quasi quoter
 _______


-}

data QuasiQuoter = QuasiQuoter
  { quoteExp  :: String -> Q Exp
  , quotePat  :: String -> Q Pat
  , quoteType :: String -> Q Type
  , quoteDec  :: String -> Q [Dec]
  }

newtype URI = URI { unUri :: Text }
    deriving (Data, Typeable)

mkURI :: Text -> Maybe URI
mkURI = error "implement me"


uri :: QuasiQuoter
uri = QuasiQuoter
    { quoteExp  = \str ->
        case mkURI (T.pack str) of
            Nothing -> fail "The input does not contain a valid URI"
            Just x  -> dataToExpQ (fmap liftText . cast) x
    , quotePat  = error "Usage as a parttern is not supported"
    , quoteType = error "Usage as a type is not supported"
    , quoteDec  = error "Usage as a declaration is not supported"
    }

-- example usage
--  x = [uri| https://markkarpov.com |]

{-

to run IO from TH use

runIO :: IO a -> Q a
-}

embedFile :: FilePath -> Q Exp
embedFile path = do
  str <- runIO (readFile path)
  addDependentFile path
  -- We lift the 'String' literal to the polymorphic 'IsString a => a' form.
  [| fromString str |]

{- Usage

TIO.putStrLn $(embedFile "src/Main.hs")
-}
