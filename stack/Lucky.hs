{-# LANGUAGE TemplateHaskell #-}
module Lucky where

import Data.Function

import Luck.Main
import Test.QuickCheck
import Language.Haskell.TH

import Observable
import Machine

maybeGen :: Gen (Maybe a) -> Gen a
maybeGen g = fix $ \g' -> g >>= maybe g' return

genByExec_Arbitrary_EquivLow :: Gen (Maybe (Variation AS))
genByExec_Arbitrary_EquivLow
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec.core") "statePred@26" asGen2

genByExec_QInit_EquivLow :: Gen (Maybe (Variation AS))
genByExec_QInit_EquivLow
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec2.core") "statePred@28" asGen2

