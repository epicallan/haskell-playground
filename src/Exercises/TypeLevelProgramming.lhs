> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE EmptyCase            #-}
> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE InstanceSigs         #-}
> {-# LANGUAGE KindSignatures       #-}
> {-# LANGUAGE LambdaCase           #-}
> {-# LANGUAGE RankNTypes           #-}
> {-# LANGUAGE ScopedTypeVariables  #-}
> {-# LANGUAGE StandaloneDeriving   #-}
> {-# LANGUAGE TemplateHaskell      #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE UndecidableInstances #-}

> module Exercises.TypeLevelProgramming where

what is an existentially quantified type. Illustrate with code sample

What is a universally quantified type. Illustrate with code sample

What is the role of data kinds extension.

Describe the types created as a result of enabling data kinds extension

How can you lift a type to term level

Write type safe version of printf or fmt function from format lib

-- singletons

write singleton instances for a type level list

write singleton instances for Maybe a

Infix Associativness
___

Given function below

multiplyAndIncrement :: (Num a) => a -> a -> a
multiplyAndIncrement x y = x * y + 1

with defined operator

infixr 8 @@
(@@) = multiplyAndIncrement

detail evaluation of 2 @@ 3 @@ 3 + 1 that leads to a value of 22

