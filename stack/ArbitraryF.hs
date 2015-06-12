{-# LANGUAGE TemplateHaskell, RankNTypes, ImplicitParams #-}
module ArbitraryF ( ArbitraryF(..), Flaggy(..), Flagged(..) ) where

import ArbitraryFInternal
import Flags
import Test.QuickCheck
import Language.Haskell.TH
import Control.Monad
import Test.QuickCheck

instance (ArbitraryF a, ArbitraryF b) => ArbitraryF (a, b) where
  arbitraryF = liftM2 (,) arbitraryF arbitraryF
  shrinkF (a, b) = [ (x, b) | x <- shrinkF a ] ++ [ (a, y) | y <- shrinkF b ]

instance (ArbitraryF a) => ArbitraryF [a] where
  arbitraryF = listOf arbitraryF
  shrinkF = shrinkList shrinkF

instance (ArbitraryF a, ArbitraryF b, ArbitraryF c) => ArbitraryF (a, b, c) where
  arbitraryF = liftM3 (,,) arbitraryF arbitraryF arbitraryF
  shrinkF (a, b, c) = [ (x, y, z) | (x, (y, z)) <- shrinkF (a, (b, c)) ]

instance (ArbitraryF a, ArbitraryF b, ArbitraryF c, ArbitraryF d) => ArbitraryF (a, b, c, d) where
  arbitraryF = liftM4 (,,,) arbitraryF arbitraryF arbitraryF arbitraryF
  shrinkF (a, b, c, d) = [ (w, x, y, z) | (w, x, (y, z)) <- shrinkF (a, b, (c, d)) ]

newtype Flaggy a = Flaggy { unFlaggy :: a }
  deriving (Eq, Ord, Show)

-- | Allow to use @shrink@ from @Arbitrary@ by passing a flag to @shrinkF@
-- explicitly. The @arbitrary@ method is not implemented
-- (as it does not make sense).
data Flagged a = Flagged { getFlag :: DynFlags, unFlag :: a }
  deriving Show

instance Arbitrary a => ArbitraryF (Flaggy a) where
  arbitraryF = Flaggy <$> arbitrary
  shrinkF (Flaggy a) = Flaggy <$> shrink a

instance ArbitraryF a => Arbitrary (Flagged a) where
  arbitrary = error "No arbitrary generator for flagged values."
  shrink (Flagged f a) =
    let ?f = f in Flagged f <$> shrinkF a

instanceArbitraryF [t| Int |]
instanceArbitraryF [t| Bool |]
-- return <$> VarT <$> newName "a" >>= \a ->
--   instanceArbitraryF' [t| Arbitrary $a |] [t| Smart $a |]
-- return <$> VarT <$> newName "a" >>= \a ->
--   instanceArbitraryF' [t| Arbitrary $a |] [t| Shrink2 $a |]

instance ArbitraryF a => ArbitraryF (Smart a) where
  arbitraryF = Smart 0 <$> arbitraryF
  shrinkF (Smart n a) =
    [ Smart m x | Smart m (Flagged _ x) <- shrink (Smart n (Flagged ?f a)) ]

instance ArbitraryF a => ArbitraryF (Shrink2 a) where
  arbitraryF = Shrink2 <$> arbitraryF
  shrinkF (Shrink2 a) =
    [ Shrink2 x | Shrink2 (Flagged _ x) <- shrink (Shrink2 (Flagged ?f a)) ]

