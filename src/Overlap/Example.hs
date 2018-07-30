{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Overlap.Example where

import Data.Proxy

-- closed type families
type family (F a) :: Bool where
    F Char  = 'True
    F Bool  = 'True
    F a     = 'False


class ShowList a where
    showl :: [a] -> String

-- The type checker computes the type of flag by evaluating F a and
-- dispatches the method based on the type of flag.
-- If it is 'True, it searches the special case instances. Otherwise, it searches the generic case instance.

instance (F a ~ flag, ShowList' flag a) => ShowList a where
    showl = showl' (Proxy :: Proxy flag)


class ShowList' (flag :: Bool) a where
    showl' :: Proxy flag -> [a] -> String

instance ShowList' 'True Char where
    showl' _ x = x

instance (Show a) => ShowList' 'False a where
    showl' _ = show

instance ShowList' 'True Bool where
    showl' _ = map toBinaryDigit
        where
            toBinaryDigit False = '0'
            toBinaryDigit True  = '1'


