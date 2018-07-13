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