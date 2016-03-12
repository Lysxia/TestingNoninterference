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
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow :: GMVA
genByExec_QInit_EquivLow
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL.luck")
        defFlags{_maxUnroll=1} TProxy2

genSSNI :: GMVA
genSSNI
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/SSNI.luck")
        defFlags{_maxUnroll=3} TProxy2

genByExec_QInit_EquivLow_BugArith :: GMVA
genByExec_QInit_EquivLow_BugArith
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGARITH.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow_BugPush :: GMVA
genByExec_QInit_EquivLow_BugPush
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGPUSH.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow_BugPop :: GMVA
genByExec_QInit_EquivLow_BugPop
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGPOP.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow_BugLoad :: GMVA
genByExec_QInit_EquivLow_BugLoad
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGLOAD.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow_BugStoreValue :: GMVA
genByExec_QInit_EquivLow_BugStoreValue
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGSTOREVALUE.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow_BugStorePointer :: GMVA
genByExec_QInit_EquivLow_BugStorePointer
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGSTOREPOINTER.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow_BugStorePC :: GMVA
genByExec_QInit_EquivLow_BugStorePC
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGSTOREPC.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow_BugJumpNoRaise :: GMVA
genByExec_QInit_EquivLow_BugJumpNoRaise
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGJUMPNORAISE.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow_BugJumpLower :: GMVA
genByExec_QInit_EquivLow_BugJumpLower
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGJUMPLOWER.luck")
        defFlags{_maxUnroll=1} TProxy2
genByExec_QInit_EquivLow_BugCall :: GMVA
genByExec_QInit_EquivLow_BugCall
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGCALL.luck")
        defFlags{_maxUnroll=1} TProxy2

genByExec_QInit_EquivLow_BugReturn :: GMVA
genByExec_QInit_EquivLow_BugReturn
  = (fmap . fmap . uncurry) Variation $
      $(mkGenQ "luck/luck/PicoGenExec-DSTARTQUASIINITIAL-DBUGRETURN.luck")
        defFlags{_maxUnroll=1} TProxy2
