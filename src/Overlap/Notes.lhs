< {-# LANGUAGE DataKinds    #-}
< {-# LANGUAGE TypeFamilies #-}
< {-# LANGUAGE UndecidableInstances #-}
< {-# LANGUAGE FunctionalDependencies #-}
< {-# LANGUAGE IncoherentInstances #-}

> module Overlap.Notes where 


Suppose you have this class:

< class Print a where
<   print :: a -> IO b

Now suppose you want to say "if type a
is in class Show, print one way, otherwise print another way". You'd probably try to write this:

< instance Show a => Print a where
<    print x = putStrLn (show x)

< instance Print a where
<    print x = putStrLn "No show method"


But that is illegal in Haskell, because the heads of the two instance declarations are identical.

Solution 1:

First define an auxiliary class Print'


< class Print' flag a b  where
<    print' :: flag -> a -> IO ()
 
< instance (ShowPred a flag, Print' flag a) => Print a where
<    print = print' (undefined::flag)


< data HTrue    
< data HFalse   
 
< class ShowPred a flag | a -> flag where {}
 
< -- Used only if the other instances don't apply
< instance (flag ~ HFalse) => ShowPred a flag
< instance ShowPred Int  HTrue   -- These instances must be
< instance ShowPred Bool HTrue   -- the same as Show's
< instance ShowPred a flag => ShowPred [a] flag

These instances do make use of overlapping instances, 
but they do not rely on the *context* to distinguish which one to pick, 
just the instance *head*. Notice that (ShowPred ty flag)
always succeeds! If ty is a type for which there is a Show instance, flag gets unified to HTrue
; otherwise flag gets unified to HFalse
. Now we can write the (non-overlapping) instances for Print'


< instance (Show a) => Print' HTrue a where
<   print' _ x = putStrLn (show x)

< instance Print' HFalse a where
<   print' _ x = putStrLn "No show method"

Solution 2:  (using incoherent instances) 

Turn ShowPred
into a type family:

-- ShowPred is a predicate on types, which says
-- which ones are instances of class Show

< type family ShowPred a
 
< type instance ShowPred a     = HFalse
< type instance ShowPred Int   = HTrue
< type instance ShowPred Bool  = HTrue
< type instance ShowPred [a]   = ShowPred a
< type instance ShowPred (a,b) = And (ShowPred a, ShowPred b)

There's a problem: overlap is not generally allowed for type families!! (The first ShowPred
instance makes all invalid


< class Print a where
<    print :: a -> IO ()

< instance (ShowPred a ~ flag, Print' flag a) => Print a where
    print = print' (undefined::flag)
 
< class Print' flag a where
-- can also have type family here
<   print' :: flag -> a -> IO ()

< instance Show a => Print' True' a where
<  print' _ x = putStrLn (show x)

< instance Print' flag a where
<  print' _ _ = putStrLn "No show method"


< type family ShowPred a 
< type instance ShowPred Int = True'   -- use datakind over Bool type
< type instance ShowPred Bool = False' 


Solution 3 (using closed type families)

< type family ShowPred a where
<  ShowPred Int    = True'
<  ShowPred Bool   = True'
<  ShowPred [a]   = ShowPred a
<  ShowPred (a,b) = And (ShowPred a, ShowPred b)
<  ShowPred a     = False'

