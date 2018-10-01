-- | a simple lang

{-#LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-} 
module BookOfTypes.GADT.Lang where 

data Expr (a :: *) where 
    LitInt   :: Int -> Expr Int 
    LitBool :: Bool -> Expr Bool
    Add      :: Expr Int -> Expr Int -> Expr Int 
    Not      :: Expr Bool -> Expr Bool 
    If       :: Expr Bool -> Expr a -> Expr a -> Expr a 


evalExpr :: Expr a -> a 
evalExpr (LitInt i) = i
evalExpr (LitBool b) = b
evalExpr (Add x y) = evalExpr x + evalExpr y
evalExpr (Not x) = not $ evalExpr x 
evalExpr (If b x y) = if evalExpr b then evalExpr x else evalExpr y

-- evalExpr If (LitBool False) (LitInt 1) . Add (LitInt 5) $ LitInt 13