{-# LANGUAGE DeriveDataTypeable #-}
module Flags where

import Data.Typeable
import Data.Data

data GenStrategy
  = GenNaive              -- Arbitrary memory/stack/instruction stream.
  | GenWeighted
  | GenSequence

  | GenByExec             -- See Note [GenByExec] in Generation.
  | GenByExec1
  | GenByExec2
  | GenByExec3
  | GenByExec4

  | GenVariational
  | GenVariational1
  | GenVariational2
  | GenVariational3
  | GenVariational4

{-
  | GenByExecBothBranches -- See Note [GenByExecBothBranches] in Generation.
  | GenByExecAllBranchesFwd
  | GenByExecAllBranchesFwd2
  | GenByExecAllBranchesFwd3
-}

  | GenByFwdExec          -- See Note [GenByExecFwdOnly] in Generation.
  | GenTinySSNI

  deriving (Eq, Read, Show, Data, Typeable)

allStrategies :: [GenStrategy]
-- All individual strategies: NB /not/ the iterate one
allStrategies
  = [ GenNaive
    , GenWeighted
    , GenSequence
    , GenByExec             
    , GenByExec1
    , GenByExec2
    , GenByExec3
    , GenByExec4

    , GenVariational
    , GenVariational1
    , GenVariational2
    , GenVariational3
    , GenVariational4

{-
    , GenNaiveInstrOnly     
    , GenByExecBothBranches 
    , GenByExecAllBranchesFwd
    , GenByExecAllBranchesFwd2
    , GenByExecAllBranchesFwd3
-}
    , GenByFwdExec 
    , GenTinySSNI ] 


data PropTest
  = 
    -- Noninterference
    PropSynopsisNonInterference -- WTF is this?
                                -- prop_semantic_noninterference
  | PropLLNI
  | PropSSNI
  | PropEENI
    
    -- broken variant
  | PropEENInoLow
  
    -- Profiling
  | PropJustProfile -- Profiling execution lengths
                    -- and termination reasons.
  | PropJustProfileVariation
                    -- Profile execution of a test
                    -- and its variation

  deriving (Eq, Read, Show, Data, Typeable)

isTestableProp :: PropTest -> Bool
isTestableProp s
  | PropJustProfile <- s = False
  | PropJustProfileVariation <- s = False
  | otherwise = True


-- CH: if it comes to generating configurations,
--     we will want to internalize the "Bug"/"Variant" part
--     which is now just a naming convention
data IfcSemantics
  = IfcDefault
      -- correct default configuration
  | IfcBugArithNoTaint
      -- addition and subtraction don't taint their result
  | IfcBugPushNoTaint
      -- push drops taint
  | IfcBugPopPopsReturns
      -- pop pops return addresses
  | IfcBugLoadNoTaint
      -- load loses the label of the memory location
  | IfcBugStoreNoValueTaint
      -- store drops all taint
  | IfcBugStoreNoPointerTaint
      -- store forgets to taint by the label of the address
  | IfcBugStoreNoPcTaint
      -- store forgets to taint by the PC label

  | IfcBugJumpNoRaisePc
      -- jumping to a high address does not raise the pc
  | IfcBugJumpLowerPc
      -- jumping to a low address lowers the pc
  
  | IfcBugCallNoRaisePc
      -- calling a high address does not raise the pc
  | IfcBugReturnNoTaint
      -- returning from a high address forgets to taint the return value
  | IfcBugValueOrVoidOnReturn
      -- we choose whether to return a value or not at return time
      -- instead of function call time
  
  | IfcBugAllowWriteDownThroughHighPtr
      -- allow stores to a currently low memory cell
      -- through a high pointer
  | IfcBugAllowWriteDownWithHighPc
      -- allow stores to a currently low memory cell with high pc

  | IfcVariantDisallowStoreThroughHighPtr
      -- this correct variant disallows any store through a high pointer,
      -- irrespective of the current label of the written memory cell
      -- (this is more restrictive than preventing high pointer write-downs)
  | IfcVariantWriteDownAsNoop
      -- this correct variant turns both kinds of write-downs
      -- into NoOps (instead of just stopping the machine)

  deriving (Eq, Read, Show, Data, Typeable, Enum)

allIfcBugs :: [IfcSemantics]
-- NB: Just the bug list, no more!
allIfcBugs = [ IfcBugArithNoTaint .. IfcBugAllowWriteDownWithHighPc ]

readIfcSemanticsList :: DynFlags -> [IfcSemantics]
-- Reads the string representing IfcSemantics
readIfcSemanticsList df
  | ifc_semantics df == "*" = allIfcBugs
  | otherwise = read (ifc_semantics df)

data GenInstrs
  = InstrsBasic -- Generate only very basic instructions (Add/Push/Noop/Load/Store/Halt)
  | InstrsJumpy -- + allowed to generate jumps
  | InstrsCally -- + allowed to generate calls/returns
  deriving (Eq, Read, Show, Data, Typeable,
            Ord -- n.b.!
            )

callsAllowed :: GenInstrs -> Bool
callsAllowed = (>= InstrsCally)

jumpAllowed :: GenInstrs -> Bool
jumpAllowed = (>= InstrsJumpy)

data StartingAS = StartInitial | StartQuasiInitial | StartArbitrary
                deriving (Eq, Read, Show, Ord, Enum, Bounded, Data, Typeable)

data Equiv = EquivMem | EquivLow | EquivWrongFull | EquivFull
                deriving (Eq, Read, Show, Ord, Enum, Bounded, Data, Typeable)

data AtomEquiv = LabelsObservable    -- correct + default
                                     -- labels are not fully observable,
                                     -- but since we only have 2 of
                                     -- them this makes no difference
               | LabelsNotObservable -- incorrect
               | HighEquivEverything -- incorrect
               deriving (Eq, Read, Show, Ord, Enum, Bounded, Data, Typeable)

data StkEltEquiv = TagOnTop   -- correct + default (high values and high
                              -- return addresses are distinguishable)                   
                 | LabelOnTop -- incorrect (all high values indistinguishable
                              --            from all high return addresses)
                 deriving (Eq, Read, Show, Ord, Enum, Bounded, Data, Typeable)

data TMUDriver
  = TMUDriver { -- Generation behavior

               gen_instrs       :: GenInstrs
             
             , gen_strategy     :: GenStrategy
             -- Overrides gen_strategy
             , gen_lucky        :: Bool
             , gen_instrs_range :: (Int,Int)
                  -- (x,y) <- gen_instrs_range
                  -- Generate an arbitrary number of instructions >= x and <= y

             , starting_as :: StartingAS
             , equiv :: Equiv
             
             , smart_ints :: Bool -- affects both generation and variation

               -- Shrinking behavior
               -- CH: TODO: how about a datatype?
             , shrink_nothing :: Bool
             , shrink_to_noop :: Bool
             , shrink_noops   :: Bool

               -- Bugs in indistinguishability relation
             , atom_equiv :: AtomEquiv
             , stk_elt_equiv :: StkEltEquiv
               
               -- Information flow semantics               
             , ifc_semantics :: String
               -- A string that represents an [IfcSemantics], or "*"
               
               -- Other configuration settings
             , step_no               :: Int -- How many steps to test for

             , timeout               :: Int -- Timeout in seconds
             , max_tests         :: Int -- Maximum # of tests (subsidiary to timeout)
             , max_discard_ratio :: Int
             , prop_test         :: PropTest -- The property to test
             , extrapol_mul      :: Int
             , extrapol_add      :: Int
             , show_counterexamples  :: Bool -- print counterexamples of not?
             , conf_max_call_args    :: Int

             , latex_output          :: Bool
             , print_all_datapoints  :: Bool -- print all times taken to find bugs
             , run_timeout_tests     :: Bool

             , genSequence_wInstr :: Int
             , genSequence_add :: Int
             , genSequence_load :: Int
             , genSequence_store :: Int
             , genSequence_jump :: Int
             , genSequence_mem :: Int

             , w_noop :: Int
             , w_halt :: Int
             , w_add :: Int
             , w_push :: Int
             , w_pop :: Int
             , w_store :: Int
             , w_load :: Int
             , w_call :: Int
             , w_return :: Int
             , w_jump :: Int

             , mw_single :: Int
             , mw_load :: Int
             , mw_store :: Int
             , mw_jump :: Int
             , mw_call :: Int

             , w_halt_mul :: Int

             , w_push_maddr :: (Int, Int, Int)
             , w_maddr :: (Int, Int)
             , w_vary :: (Int, Int)
             , w_extraret :: (Int, Int)

             , w_smartInt :: (Int, Int, Int)
             , w_smartInt2 :: (Int, Int, Int)
             , w_data_ret :: (Int, Int)
             , derivedFlags :: DerivedFlags
             }
  deriving (Eq, Read, Show, Data, Typeable)

type DynFlags = TMUDriver
data DerivedFlags = DerivedFlags
  { bugArithNoTaint :: Bool
  , bugPushNoTaint :: Bool
  , bugPopPopsReturns :: Bool
  , bugLoadNoTaint :: Bool
  , bugStoreNoValueTaint :: Bool
  , bugStoreNoPointerTaint :: Bool
  , bugStoreNoPcTaint :: Bool
  , bugJumpNoRaisePc :: Bool
  , bugJumpLowerPc :: Bool
  , bugCallNoRaisePc :: Bool
  , bugReturnNoTaint :: Bool
  , bugValueOrVoidOnReturn :: Bool
  , bugAllowWriteDownThroughHighPtr :: Bool
  , bugAllowWriteDownWithHighPc :: Bool
  , variantDisallowStoreThroughHighPtr :: Bool
  , variantWriteDownAsNoop :: Bool
  } deriving (Eq, Read, Show, Data, Typeable)

getMaxBugs :: DynFlags -> Int
getMaxBugs f = extrapol_add f + timeout f * extrapol_mul f

deriveFlags :: [IfcSemantics] -> DynFlags -> DynFlags
deriveFlags bugs f = f' `seq` f{derivedFlags=DerivedFlags{..}}
  where
  f'@[
    bugArithNoTaint,
    bugPushNoTaint,
    bugPopPopsReturns,
    bugLoadNoTaint,
    bugStoreNoValueTaint,
    bugStoreNoPointerTaint,
    bugStoreNoPcTaint,
    bugJumpNoRaisePc,
    bugJumpLowerPc,
    bugCallNoRaisePc,
    bugReturnNoTaint,
    bugValueOrVoidOnReturn,
    bugAllowWriteDownThroughHighPtr,
    bugAllowWriteDownWithHighPc,
    variantDisallowStoreThroughHighPtr,
    variantWriteDownAsNoop]
    = map (\x -> x `elem` bugs) [IfcBugArithNoTaint .. IfcVariantWriteDownAsNoop]

finalizeFlags :: DynFlags -> DynFlags
finalizeFlags = finalizeWeights

-- Spaghetti
finalizeWeights :: DynFlags -> DynFlags
finalizeWeights f =
  let byExec = [GenByExec, GenByExec1, GenByExec2, GenByExec3, GenByExec4]
      variat = [GenVariational, GenVariational1, GenVariational2,
                GenVariational3, GenVariational4]
  in case gen_strategy f of
    GenNaive -> setWeights f (10,10,10,10,10,10,10,10,10,10)
    GenWeighted -> propWeights f
    GenSequence
      -> (propWeights f{ w_halt = w_halt f ? 120 }){ w_call = 0, w_return = 0 }
    GenTinySSNI -> ssni f
    g -> execW f
    {- g | g `elem` byExec -> execW f
    g | g `elem` variat -> execW f
    GenByFwdExec -> execW f -}
  where
    propEENI = [PropEENI, PropJustProfile, PropJustProfileVariation]
    propWeights f =
      case prop_test f of
        PropLLNI -> llni f
        p | p `elem` propEENI -> eeni f
        _ -> f -- unsupported property
    setWeights f
      ( w_noop', w_halt'
      , w_add', w_push'
      , w_pop' , w_store'
      , w_load', w_call'
      , w_return', w_jump')
      = f{ w_noop = w_noop f ? w_noop', w_halt = w_halt f ? w_halt'
         , w_add = w_add f ? w_add', w_push = w_push f ? w_push'
         , w_pop = w_pop f ? w_pop', w_store = w_store f ? w_store'
         , w_load = w_load f ? w_load', w_call = w_call f ? w_call'
         , w_return = w_return f ? w_return', w_jump = w_jump f ? w_jump'
         }
    0 ? x = x
    y ? _ = y
    execW f = f{w_halt_mul = w_halt_mul f ? if prop_test f `elem` propEENI then 10 else 0}
      `setWeights` ( 1, 5
                   , 40, 300
                   , 40, 60
                   , 60, 40
                   , 40, 40 )
    eeni f = setWeights f
      ( 40, 70
      , 40, if starting_as f == StartInitial then 400 else 200
      , 40, 40, 40, 40, 40, 40 )
  -- CH: TODO: tweak this more
  -- why is there a distinction here wrt LLNI?
  -- is it just to account for NOOPs and HALTs which have different weights?
    llni f = setWeights f
      ( 1, 5
      , 40, if starting_as f == StartInitial then 300 else 150
      , 40, 40, 40, 40, 40, 40 )
    ssni f = setWeights f (1, 1, 10, 10, 10, 20, 20, 10, 20, 10)

-- Note [Max Tests Too Large]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If the maximum number of TMU tests to run (tmu_max_tests) is too large,
-- QuickCheck may (somewhat surprisingly) die by "giving up"---that is,
-- producing too many discards.  The reason for this is integer overflow.  In
-- order to calculate the maximum number of allowed discards, QuickCheck
-- computes `maxDiscardRatio * maxSuccess`¹.  If that multiplication overflows,
-- then we might get a negative number and immediately fail.  Or we might happen
-- to get a large positive number; this actually happened to us accidentally.
-- The default value for `maxDiscardRatio` is 10, and it so happens that while
-- `10 * (maxBound :: Int) = -10`, dividing by two first produced
-- `10 * ((maxBound :: Int) `div` 2) = 9223372036854775798`, which---although
-- rather large---is still smaller than `maxBound `div` 2`.
--
-- Consequently, how large should we make this value when we're working with
-- timeouts?  Well, suppose that (a) we're working with 64-bit integers (which
-- we are); and (b) that every test is one operation on a 4 GHz machine, taking
-- 0.25 ns (which is a vast underestimate, but a lower bound).  Then:
--   * maxBound tests will take 73.12 years;
--   * maxBound/2 tests will take 36.56 years;
--   * maxBound/10 tests will take 7.312 years;
--   * maxBound/20 tests will take 3.656 years;
--   * maxBound/50 tests will take 1.462 years; and
--   * maxBound/100 tests will take 0.7312 years.
-- Thus, any of those values is MORE than reasonable; going with the last
-- provides plenty of space for a nice discard ratio.
--
-- ¹ For QuickCheck 2.5.1.1, this is in the definition of
-- Test.QuickCheck.Test.quickCheckWithResult, at line 110;
-- http://hackage.haskell.org/packages/archive/QuickCheck/2.5.1.1/doc/html/src/Test-QuickCheck-Test.html#line-110
-- and/or
-- http://hackage.haskell.org/packages/archive/QuickCheck/2.5.1.1/doc/html/src/Test-QuickCheck-Test.html#quickCheckWithResult will take you there.


-- the_flags_ref :: IORef DynFlags
-- the_flags_ref = unsafePerformIO (newIORef dynFlagsDflt)
-- 
-- setFlagsRef :: DynFlags -> IO ()
-- setFlagsRef fgs = writeIORef the_flags_ref fgs
-- 
-- getFlagsRef :: IO DynFlags
-- getFlagsRef = readIORef the_flags_ref
-- 
--class Flaggy a where
--  getFlags :: a

