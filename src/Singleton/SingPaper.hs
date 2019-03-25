{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | notes from https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf

module Singleton.SingPaper where

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TH

{-
 Elements that support the singleton library

 data family Sing (a :: K)

 class SingI (a :: k) where
   sing :: Sing a

 class SingE (a :: k) where
   type Demote a :: *
   fromSing :: Sing a -> Demote (Any :: k)

 class (SingI a, SIngE a) => SingRep a
 instance (SingI a , SingE a) => SingRep a

 data SingInstance(a :: k) where
   SingInstance :: SingRep a => SingInstance a

 class (t ~ Any) => SingKind(t :: k) where
   singInstance :: forall (a :: k). Sing a -> SingInstance a
-}
