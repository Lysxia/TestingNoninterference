{-# LANGUAGE TemplateHaskell #-}
module ArbitraryFInternal where

import Flags
import Test.QuickCheck
import Language.Haskell.TH

class ArbitraryF a where
  arbitraryF :: (?f :: DynFlags) => Gen a
  shrinkF :: (?f :: DynFlags) => a -> [a]
  shrinkF x = []

instanceArbitraryF :: Q Type -> Q [Dec]
instanceArbitraryF ty =
  [d| instance ArbitraryF $ty where
        arbitraryF = arbitrary
        shrinkF = shrink |]

instanceArbitraryF' :: Q Type -> Q Type -> Q [Dec]
instanceArbitraryF' cxt ty =
  [d| instance $cxt => ArbitraryF $ty where
        arbitraryF = arbitrary
        shrinkF = shrink |]

