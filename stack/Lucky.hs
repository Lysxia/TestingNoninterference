{-# LANGUAGE TemplateHaskell #-}
module Lucky where

import Data.Function

import Luck.Template
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
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL.luck" tProxy2Q)

genByExec_QInit_EquivLow :: GMVA
genByExec_QInit_EquivLow
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugArith :: GMVA
genByExec_QInit_EquivLow_BugArith
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGARITH.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugPush :: GMVA
genByExec_QInit_EquivLow_BugPush
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGPUSH.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugPop :: GMVA
genByExec_QInit_EquivLow_BugPop
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGPOP.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugLoad :: GMVA
genByExec_QInit_EquivLow_BugLoad
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGLOAD.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugStoreValue :: GMVA
genByExec_QInit_EquivLow_BugStoreValue
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGSTOREVALUE.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugStorePointer :: GMVA
genByExec_QInit_EquivLow_BugStorePointer
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGSTOREPOINTER.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugStorePC :: GMVA
genByExec_QInit_EquivLow_BugStorePC
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGSTOREPC.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugJumpNoRaise :: GMVA
genByExec_QInit_EquivLow_BugJumpNoRaise
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGJUMPNORAISE.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugJumpLower :: GMVA
genByExec_QInit_EquivLow_BugJumpLower
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGJUMPLOWER.luck" tProxy2Q)
genByExec_QInit_EquivLow_BugCall :: GMVA
genByExec_QInit_EquivLow_BugCall
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGCALL.luck" tProxy2Q)

genByExec_QInit_EquivLow_BugReturn :: GMVA
genByExec_QInit_EquivLow_BugReturn
  = fmap (uncurry Variation) <$> $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGRETURN.luck" tProxy2Q)
