> module PhantomTypes.Notes where

Phantom types are a way in which we can have a stronger type system.

The motivation behind using phantom types is to specialize the return type of data constructors. 
For example, consider:

> data T' = TI' Int | TS' String

< plus' :: T' -> T' -> T'
< concat :: T' -> T' -> T'

Phantom type version is

> data T a = TI Int | TS String

< plus :: T Int -> T Int -> T Int
< concat :: T String -> T String -> T String

Note that type a is a type placeholder and is not used in any of the constructors for T.
Hence the name phantom type.

Phantom types enable us to write more type safe code, make invalid states less likely to exist.

Use case example for tagging boolean data http://oleg.fi/gists/posts/2019-03-21-flag.html

given a function that uses boolean flags

myFun :: Bool -> Bool -> Bool -> IO ()

> newtype Flag t = MkFlag Bool

> toFlag :: t -> Bool -> Flag t
> toFlag _ = MkFlag

> fromFlag :: t -> Flag t -> Bool
> fromFlag _ (MkFlag b) = b

> data ShowGlobal = ShowGlobal
> data ShowFlags = ShowFlags
> data ShowLogs = ShowLogs

> myFun :: Flag ShowLogs -> Flag ShowGlobal -> Flag ShowFlags -> IO ()
> myFun = error "do me"

