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

luckyItem = "statePred@34"

type GMVA = Gen (Maybe (Variation AS))

genByExec_Arbitrary_EquivLow :: GMVA
genByExec_Arbitrary_EquivLow
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec.core") luckyItem asGen2

genByExec_QInit_EquivLow :: GMVA
genByExec_QInit_EquivLow
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugArith :: GMVA
genByExec_QInit_EquivLow_BugArith
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/core/PicoGenExec-DBUGARITH.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugPush :: GMVA
genByExec_QInit_EquivLow_BugPush
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGPUSH.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugPop :: GMVA
genByExec_QInit_EquivLow_BugPop
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGPOP.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugLoad :: GMVA
genByExec_QInit_EquivLow_BugLoad
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGLOAD.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugStoreValue :: GMVA
genByExec_QInit_EquivLow_BugStoreValue
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGSTOREVALUE.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugStorePointer :: GMVA
genByExec_QInit_EquivLow_BugStorePointer
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGSTOREPOINTER.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugStorePC :: GMVA
genByExec_QInit_EquivLow_BugStorePC
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGSTOREPC.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugJumpNoRaise :: GMVA
genByExec_QInit_EquivLow_BugJumpNoRaise
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGJUMPNORAISE.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugJumpLower :: GMVA
genByExec_QInit_EquivLow_BugJumpLower
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGJUMPLOWER.core") luckyItem asGen2
genByExec_QInit_EquivLow_BugCall :: GMVA
genByExec_QInit_EquivLow_BugCall
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGCALL.core") luckyItem asGen2

genByExec_QInit_EquivLow_BugReturn :: GMVA
genByExec_QInit_EquivLow_BugReturn
  = fmap (uncurry Variation) <$> $(mkBoundedGenQ 1000 "luck/core/PicoGenExec-DBUGRETURN.core") luckyItem asGen2
