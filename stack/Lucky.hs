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

genByExec_QInit_EquivFull :: Gen (Maybe (Variation AS))
genByExec_QInit_EquivFull
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec.core") "statePred@28" asGen2

