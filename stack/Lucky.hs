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

type GMVA = Gen (Maybe (Variation AS))

genByExec_Arbitrary_EquivLow :: GMVA
genByExec_Arbitrary_EquivLow
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec.core") "statePred@28" asGen2

genByExec_QInit_EquivLow :: GMVA
genByExec_QInit_EquivLow
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugArith :: GMVA
genByExec_QInit_EquivLow_BugArith
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGARITH.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugPush :: GMVA
genByExec_QInit_EquivLow_BugPush
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGPUSH.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugPop :: GMVA
genByExec_QInit_EquivLow_BugPop
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGPOP.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugLoad :: GMVA
genByExec_QInit_EquivLow_BugLoad
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGLOAD.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugStoreValue :: GMVA
genByExec_QInit_EquivLow_BugStoreValue
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGSTOREVALUE.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugStorePointer :: GMVA
genByExec_QInit_EquivLow_BugStorePointer
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGSTOREPOINTER.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugStorePC :: GMVA
genByExec_QInit_EquivLow_BugStorePC
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGSTOREPC.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugJumpNoRaise :: GMVA
genByExec_QInit_EquivLow_BugJumpNoRaise
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGJUMPNORAISE.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugJumpLower :: GMVA
genByExec_QInit_EquivLow_BugJumpLower
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGJUMPLOWER.core") "statePred@28" asGen2
genByExec_QInit_EquivLow_BugCall :: GMVA
genByExec_QInit_EquivLow_BugCall
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGCALL.core") "statePred@28" asGen2

genByExec_QInit_EquivLow_BugReturn :: GMVA
genByExec_QInit_EquivLow_BugReturn
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/PicoGenExec-DBUGRETURN.core") "statePred@28" asGen2
