{-# LANGUAGE FlexibleContexts, UndecidableInstances, RankNTypes, ScopedTypeVariables,
    NamedFieldPuns #-}

module Driver where

import Test.QuickCheck
import Test.QuickCheck.Random

import Control.Monad
import Control.Applicative
import Control.Arrow()
import Data.List (genericLength, unzip4)

import System.Console.CmdArgs
import Text.Printf

import Data.Maybe

import Data.IORef

import ArbitraryF
import Flags
import Machine
import Generation () -- Import just Arbitrary
import Observable
import ObservableInst ()
import Lucky

import System.Time
import System.Exit
import System.IO
import System.IO.Unsafe

import DriverUtils

import Debug.Trace

-- The default setting for flags should produce a correct machine
dynFlagsDflt :: DynFlags
dynFlagsDflt
  = TMUDriver { gen_instrs       = InstrsCally
              , gen_strategy     = GenByExec
              , gen_lucky        = False
              , gen_instrs_range = (20,50)
               
              , starting_as = StartQuasiInitial
              , equiv       = EquivFull
              
              , smart_ints = True
              
              , shrink_nothing = True
              , shrink_to_noop = True
              , shrink_noops   = True 
             
              , atom_equiv = LabelsObservable
              , stk_elt_equiv = TagOnTop
              
              , ifc_semantics = "[IfcDefault]"
              
              , step_no    = 50
              , timeout    = 1
              , max_tests         = maxBound `div` 100 -- See Notes [Max Tests Too Large]
              , max_discard_ratio = 30
              , prop_test         = PropLLNI
              , extrapol_mul      = 10
              , extrapol_add      = 1000
              , show_counterexamples  = False
              , conf_max_call_args    = 2
              , latex_output          = False
              
              , print_all_datapoints  = False
              , run_timeout_tests     = True

              , genSequence_wInstr = 10
              , genSequence_add = 1
              , genSequence_load = 1
              , genSequence_store = 1
              , genSequence_jump = 1
              , genSequence_mem = 1

              -- 0 = default, set by finalizeFlags, which must be called
              -- just after running cmdArgs
              , w_noop = -1
              , w_halt = -1
              , w_add = -1
              , w_push = -1
              , w_pop = -1
              , w_store = -1
              , w_load = -1
              , w_call = -1
              , w_return = -1
              , w_jump = -1

              , mw_single = 10
              , mw_load = 1
              , mw_store = 3
              , mw_jump = 1
              , mw_call = 1

              , w_halt_mul = -1

              , w_maddr = (1, 9)
              , w_push_maddr = (200, 5, 1)

              , w_vary = (9, 1)
              , w_extraret = (9, 1)

              , w_smartInt = (1, 1, 1)
              , w_smartInt2 = (1, 1, 4)

              , w_data_ret = (5, 1)
              , derivedFlags = noBug
              }

noBug = DerivedFlags
  { bugArithNoTaint = False
  , bugPushNoTaint = False
  , bugPopPopsReturns = False
  , bugLoadNoTaint = False
  , bugStoreNoValueTaint = False
  , bugStoreNoPointerTaint = False
  , bugStoreNoPcTaint = False
  , bugJumpNoRaisePc = False
  , bugJumpLowerPc = False
  , bugCallNoRaisePc = False
  , bugReturnNoTaint = False
  , bugValueOrVoidOnReturn = False
  , bugAllowWriteDownThroughHighPtr = False
  , bugAllowWriteDownWithHighPc = False
  , variantDisallowStoreThroughHighPtr = False
  , variantWriteDownAsNoop = False
  }

withFlags :: [IfcSemantics] -> DynFlags -> ((?f :: DynFlags) => a) -> a
withFlags bugs f a = let ?f = deriveFlags bugs f in a

withFlags' :: DynFlags -> ((?f :: DynFlags) => a) -> a
withFlags' f a = withFlags (bugList f) f a

show_some_testcases :: Int -> IO ()
show_some_testcases n
  = do { flags <- cmdArgs dynFlagsDflt
       -- ; let ?dfs = flags
       ; withFlags [] flags $
         do { gen <- newQCGen
            ; when (latex_output ?f) (putStr "% ") >> print gen

            ; quickCheckWith
              stdArgs { maxSuccess = n
                      , replay = Just (gen, 42)
                      , chatty = True } $
              forAll arbitraryF $ \(as :: AS) ->
                whenFail (return ()) $
                  collect (show as) $
                    property True }
       }

main :: IO ()
main = do { flags <- finalizeFlags <$> cmdArgs dynFlagsDflt
          ; let ?f = flags
          -- ; sample asGen
          -- ; exitSuccess
          ; success <- do_strategy flags
          ; if success then exitSuccess else exitFailure }

means :: [Maybe Rational] -> (Maybe Double, Maybe Double, Maybe Double)
means [] = (Just 0,Just 0,Nothing)
means xs =
  let double                        = fromRational :: Rational -> Double
      fins                          = catMaybes xs
      n                             = genericLength fins
      isInfinity                    = elem Nothing xs
      arithmetic | isInfinity       = Nothing
                 | n == 0           = Nothing
                 | otherwise        = Just $ double $ sum fins / n
      geometric  | isInfinity       = Nothing
                 | n == 0           = Nothing
                 | otherwise        = Just $ double (product fins) ** double (recip n)
      harmonic   | 0 `elem` fins    = Just 0 -- See Note [Harmonic Mean]
                 | n == 0           = Nothing
                 | otherwise        = Just $ double $ n / sum (map recip fins)
  in (arithmetic, geometric, harmonic)
  -- Note [Harmonic Mean]
  -- Informally, 1/(1/0 + …) ≈ 1/(∞ + …) ≈ 1/∞ ≈ 0.  Also, this
  -- satisfies the AM ≥ GM ≥ HM inequality.  And it concords with this
  -- online Q&A: http://stats.stackexchange.com/q/37628. Also, if any mean
  -- time to failure is infinity, we just have to discard it.

do_strategy :: DynFlags -> IO Bool
do_strategy f
  = do when (run_timeout_tests f) $
         putStrLn $
           if print_all_datapoints f then
             "% Format = time to find bug in milli seconds."
           else
             "% Format = bug & #tests/sec & discard ratio & mean time to failure"
       (success',speeds,discRates,allBugsPerSecs) <-
         unzip4 <$>
         if run_timeout_tests f
         then mapM (do_ifc f) (bugList f)
         else (:[]) <$> runTimeOutTests f -- Kinda counterintuitive
       let success = and success'
           avSpeed = if null speeds then 0
                     else sum speeds / genericLength speeds
           avDiscRate = if null discRates then 0
                        else sum discRates / genericLength discRates

       when (run_timeout_tests f) $ do
         case means allBugsPerSecs of
           (a,g,h) ->
             let p Nothing = "---"
                 p (Just d) = printf "%.2f" d in
             when not_profiling $
             void $ printf "\\means{}{%s}{%s}{%s}\n" (p a) (p g) (p h)
         timestamp <- getClockTime
         when not_profiling $ do
           void $ printf "\\extrabuginfo{%s}{\\%s}{%s}{%s}{\\%s}{%s}{%s}{%d}{%s}\n"
                    (show (gen_instrs f))
                    (show (prop_test f))
                    (show (equiv f))
                    (show (starting_as f))
                    (show (gen_strategy f))
                    (show (smart_ints f))
                    (show_timeout (timeout f))
                    (getMaxBugs f)
                    (show timestamp)
           void $ printf "\\averagespeed{%0.0f}\n" avSpeed
           void $ printf "\\averagediscrate{%0.0f\\%%}\n" avDiscRate
       return success
  where not_profiling | PropJustProfile <- prop_test f
                      = False
                      | PropJustProfileVariation <- prop_test f
                      = False
                      | otherwise = True

show_timeout i = show i ++"sec"

runTimeOutTests f =
  withFlags (bugList f) f $ do
    putStrLn $ "% Flags: "++show f
    ior <- newIORef 0
    (r,_) <- checkProperty ior (prop_test f) $ 365*24*60*60*10^6
    pure . (,0,0,Nothing) $ case r of
      Right (Success{})  -> True
      Right (GaveUp{..}) -> numTests > 0
      _          -> False

do_ifc :: DynFlags -> IfcSemantics -> IO _
do_ifc f bug
  | PropJustProfile <- prop_test f
  = withFlags' f (profileTests >> return (True,0,0,Nothing))
  | PropJustProfileVariation <- prop_test f
  = withFlags' f (profileVariations >> return (True,0,0,Nothing))
  | otherwise
  = let -- Compute MTTF in ms
        mean_time_to_failure_stats :: TestCounters -> (Maybe Rational, Maybe Rational)
        mean_time_to_failure_stats c
          | null $ times_c c = (Nothing, Nothing)
          | otherwise =
            let mttf = sum (map fromIntegral $ times_c c)
                       / (fromIntegral (bugs_c c) * 1000)
                quadDev t = (mttf - fromIntegral t / 1000) ^ 2
                var | bugs_c c < 2 = Nothing
                    | otherwise = Just $ sum (map quadDev $ times_c c)
                                  / (fromIntegral (bugs_c c) - 1) in
            (Just mttf, var)

        extrap_info (Left ()) = " (A)"
        extrap_info (Right (_r,_b,_d)) = " (E)"
             -- = printf " (E, b = %d, r=%d, d=%d)" b r d
    in
    do { counters <- action [bug] f
       ; let gen_speed :: Double =
                (fromIntegral (run_c counters + disc_c counters) :: Double)
                    / (fromIntegral (timeout f) :: Double)
       ; let disc_rate :: Double = 100.00 *
                   (fromIntegral (disc_c counters) /
                      (fromIntegral (run_c counters + disc_c counters):: Double))
       ; let (mttf, var) = mean_time_to_failure_stats counters
       ; let mttfStr = case mttf of
                         Just mean ->
                           printf "%0.2f" (fromRational mean :: Double)
                           ++ extrap_info (extrapolated counters)
                         Nothing -> "---"
       ; let varStr = case var of
                        Just var ->
                           printf "%0.2f" (fromRational var :: Double)
                        Nothing -> "---"
       ; let acc95Str = case var of
                          Just var ->
                            -- We are here assuming that the mean of the data points has
                            -- a normal distribution with variance given by the expression
                            -- below, which is not very accurate when only few bugs were found.
                            -- Assuming that this approximation is valid, the resulting number
                            -- v yields a 95% confidence interval of the form
                            -- [m-1.96sqrt(v),m+1.96sqrt(v)]
                            let varAvg = fromRational var / fromIntegral (bugs_c counters) :: Double in
                            printf "%0.2f" $ 1.96 * sqrt varAvg
                          Nothing -> "---"
       ; if print_all_datapoints f then do
           putStrLn ("% " ++ show bug)
           mapM_ (print . (/ 1000) . fromIntegral) $ times_c counters
         else
           void $ printf "\\row{\\%s}{%s}{%s}{%s}{%d}{%s}{%s} %%%d/%d %0.4f\n"
                       (show bug)
                       (printf "%0.0f" gen_speed :: String)
                       (printf "%0.0f\\%%" disc_rate :: String)
                       mttfStr
                       (bugs_c counters)
                       varStr
                       acc95Str
                       (bugs_c counters)
                       (run_c counters)
                       (fromIntegral (bugs_c counters) / fromIntegral (run_c counters) :: Double)
       ; hFlush stdout
       ; return (True, gen_speed,
                       disc_rate,
                       mttf)
       }

bugList = readIfcSemanticsList

action :: [IfcSemantics] -> DynFlags -> IO TestCounters
action bugs flags = withFlags bugs flags checkTimeoutProperty
