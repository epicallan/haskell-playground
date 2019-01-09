{-# LANGUAGE RankNTypes #-}

module Lenses.Basics.Traversal where

import Control.Applicative
import Control.Lens
import qualified Data.Map as M

{-
    How do Traversals fit into the lens hierarchy?

    All Lenses are usable as Traversals over a single element
    All traversals are also Folds
    Most traversals are Indexable and can contain location info. E.g. index in their list, key of a Map

-}

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

{-
    s: Starting structure, e.g. [Transaction] in our case
    t: Ending structure, e.g. [result]
    a: Starting focus, e.g. Transaction
    b: Ending focus, e.g. result
-}


-- type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t


data  Transaction =
    Withdrawal { amount :: Int}
  | Deposit { amount :: Int}
  deriving Show

{-
Let's say we have a list of Transactions and we want to focus
on each element of that list using a traversal.
-}

-- traverse :: Traversable f => Traversal (f a) (f b) a b
                            --              s     t
-- traverse :: Applicative f => (a -> f b) -> f a -> f b

-- simpleTransactions ::  forall f . Applicative f => (Transcation -> [Transcation ]) -> [Transaction] -> [Transaction]
simpleTransactions :: Traversal' [Transaction] Transaction
simpleTransactions = traverse


{-
    type Traversal' s a = Traversal s s a a
    Traversal' structure focus
-}

-- when you relax type signature, you control return type

typeChangingTransactions :: Traversal [Transaction] [result] Transaction result
typeChangingTransactions = traverse

someTransactions :: [Transaction]
someTransactions = [Deposit 100, Withdrawal 50]

-- > someTransactions & typeChangingTransactions .~ "a string"
-- ["a string","a string"] -- can be re-writted with traversed

-- writing a traversal that only focuses on withDrawals
-- traverse :: (Traversable t , Applicative f) => (a -> f b) -> t a -> f (t b)

-- withDrawals :: Traversal' [Transaction] Int

-- withDrawals
--     :: Applicative f
--     => (Int -> f Int)
--     -> f [Transaction]
--     -> f Transaction
-- withDrawals f transactions = f <$> transactions


allAmounts
    :: Applicative f
    => (Int -> f Int)
    -> [Transaction]
    -> f [Transaction]
allAmounts f = traverse go
    where
        go (Withdrawal amt) = Withdrawal <$> f amt
        go (Deposit amt)  = Deposit <$> f amt


-- to fold a traversal into a list
-- > someTransactions ^.. allAmounts
--   [100,50]


-- withdrawals :: Traversal' [Transaction] Int
-- a.k.a.
withdrawals
    :: (Applicative f)
    => (Int -> f Int)
    -> [Transaction]
    -> f [Transaction]
withdrawals f = traverse go
    where
        go (Withdrawal amt) = Withdrawal <$> f amt
        go (Deposit    amt) = Deposit <$> pure amt

-- > someTransactions ^.. allAmounts
--   [50]

-- someTransactions & withdrawals +~ 10
-- [Deposit {amount = 100},Withdrawal {amount = 60}]

-- Handling embedded travels types

data AccountType = Chequing | Savings
    deriving Show


data BankAccount = BankAccount
    { accountType :: AccountType
    , transactions :: [Transaction]
    } deriving Show


-- a traversal which can access withdrawals from within a bank account!
-- accountWithdrawals :: Traversal' BankAccount Int
accountWithdrawals
    :: Applicative f
    => (Int -> f Int)
    -> BankAccount
    -> f BankAccount
accountWithdrawals f (BankAccount accType transactions) =
    BankAccount accType <$> withdrawals f transactions


account :: BankAccount
account = BankAccount Savings [Deposit 100, Withdrawal 50]

-- account ^.. accountWithdrawals
-- [50]

--  account & accountWithdrawals +~ 10
-- BankAccount {
--    accountType = Savings,
--    transactions = [Deposit {amount = 100},Withdrawal {amount = 60}]}

{-
 Assumming we have lenses over Banck account
 i.e
 transactions :: Lens' BankAccount [Transaction]
 we can compose and dervive accountWithdrawlas

    accountWithdrawals :: Traversal' BankAccount Int
    accountWithdrawals = transactions . withdrawals

-}
