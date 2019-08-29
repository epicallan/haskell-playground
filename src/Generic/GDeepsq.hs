-- | implementation of rnf function from NFData class
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
module Generic.GDeepsq where

import Control.DeepSeq
import Generics.SOP
import qualified GHC.Generics as GHC

class GNFData a where
  grnf :: (Generic a, All2 NFData (Code a)) => a -> ()

  default grnf :: (Generic a, All2 NFData (Code a)) => a -> ()
  grnf a = grnfs (from a) where
    grnfs :: (All2 NFData xss) => SOP I xss -> ()
    grnfs (SOP (Z xs))  = grnfp xs
    grnfs (SOP (S xss)) = grnfs (SOP xss)

    grnfp :: All NFData xs => NP I xs -> ()
    grnfp Nil         = ()
    grnfp (I x :* xs) = x `deepseq` grnfp xs


data A = C Bool | D A Int
  deriving (Show, GHC.Generic, Generic, GNFData)

