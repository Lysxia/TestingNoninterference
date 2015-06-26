{-# LANGUAGE TemplateHaskell #-}
module Lucky where

import Observable
import Machine

import Luck.Main
import Test.QuickCheck
import Language.Haskell.TH
import Paths_tni

luckyGen :: Gen (Maybe (Variation AS))
luckyGen = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec.core") "statePred@28" asGen2

