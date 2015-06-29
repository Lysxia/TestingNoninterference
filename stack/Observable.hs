{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, CPP #-}

module Observable where

import Data.Data
import Test.QuickCheck.Gen
import Test.QuickCheck
import Control.Monad
import Data.Function

import Distances
import LaTeX

import ArbitraryF
import Labels
import Flags

data Variation a = Variation a a
  deriving (Eq, Typeable, Data)

instance (Eq a, Show a) => Show (Variation a) where
#ifdef SHOW
  show (Variation a a') = showDiff (show a) (show a')
#else
  show = error "SHOW"
#endif

instance (Eq a, LaTeX a) => LaTeX (Variation a) where
  toLaTeX (Variation a a')
    | a == a'   = toLaTeX a
    | otherwise = let (pre,l,l',post) = (common `on` toLaTeX) a a'
                  in concat [pre, "\\variation{", l, "}{", l', "}", post]
    where
      common xs ys = let (pre,  xs', ys')  = commonL True xs ys
                         (tsop, sx', sy') = (commonL False `on` reverse) xs' ys'
                     in (pre, reverse sx', reverse sy', reverse tsop)

commonL :: Bool -> String -> String -> (String, String, String)
commonL checkBackslash = common'
  where common' xs     []                 = ([],xs,[])
        common' []     ys                 = ([],[],ys)
        common' (x:xs) (y:ys)
          | x == y    = let (zs,xs',ys') = common' xs ys
                        in case (checkBackslash,x,zs) of
                             (True,'\\',[]) -> (zs,'\\':xs,'\\':ys)
                             _              -> (x:zs,xs',ys')
          | otherwise = ([],x:xs,y:ys)

instance Functor Variation where
  fmap f (Variation a a') = Variation (f a) (f a')

-- (~~~) represents an equivalence relation which is not capable of
-- discerning the difference between H-labeled data.  So a@L ~~~ b@L
-- iff a == b, and a@H ~~~ b@H for all a and b.  Differently-labeled
-- values are never equivalent.  The vary function changes H-tagged
-- values in a structure to arbitrary other values; it should always
-- be the case that liftA2 (~~~) (vary x) (pure x) is true.
class Observable a where
  (~~~) :: (?f :: DynFlags) => a -> a -> Bool
     -- low-indistinguishable
  vary  :: (?f :: DynFlags) => a -> Gen a
     -- produces ~~~ outputs
  shrinkV :: (?f :: DynFlags) => Variation a -> [Variation a]
     -- demands ~~~ inputs and produces ~~~ outputs

errorShrinkV :: (Show a, Observable a, ?f :: DynFlags) => String -> Variation a -> b
errorShrinkV inst (Variation a a') =
  error $ "shrinkV for " ++ inst ++ " received " ++ (if a ~~~ a'
                                                       then "unhandled ~~~ arguments"
                                                       else "non-~~~ arguments" ++ show a ++ "\n" ++ show a')


instance (ArbitraryF a, Observable a) => ArbitraryF (Variation a) where
  arbitraryF = do a <- arbitraryF
                  a' <- vary a
                  return $ Variation a a'
  shrinkF = shrinkV

instance Observable a => Observable (Flaggy a) where
  Flaggy a ~~~ Flaggy b = a ~~~ b
  vary (Flaggy a) = Flaggy <$> vary a
  shrinkV (Variation (Flaggy a) (Flaggy b)) = [ Variation (Flaggy x) (Flaggy y) | Variation x y <- shrinkV (Variation a b) ]

instance (ArbitraryF a, Observable a) => Observable (Labeled a) where
  (Labeled L x) ~~~ (Labeled L y) = x ~~~ y
  (Labeled H _) ~~~ (Labeled H _) = True
  (Labeled _ x) ~~~ (Labeled _ y) =
    case atom_equiv ?f of
      LabelsObservable -> False
      LabelsNotObservable -> x ~~~ y
      HighEquivEverything -> True

  vary _ = error "Observable (Labeled a) implements no vary"

  shrinkV (Variation (Labeled L x) (Labeled L x')) | x ~~~ x' =
    map (fmap $ Labeled L) . shrinkV $ Variation x x'
  shrinkV (Variation (Labeled H x) (Labeled H x')) =
    (if x ~~~ x' then
      [Variation (Labeled L x) (Labeled L x')]
    else
      [Variation (Labeled L x) (Labeled L x),
       Variation (Labeled L x') (Labeled L x')])
   ++ 
    [Variation (Labeled H y) (Labeled H y') | (y,y') <- shrinkF (x,x') ]
   ++ 
    [Variation (Labeled H y) (Labeled H y') | y <- shrinkF x, y' <- shrinkF x' ]
  shrinkV (Variation (Labeled l x) (Labeled l' y)) =
    case atom_equiv ?f of
      LabelsObservable -> []
      LabelsNotObservable ->
        if l==H || l'==H then
          [Variation (Labeled L x) (Labeled L y) | x ~~~ y] ++
          [Variation (Labeled l x') (Labeled l' y') |
               x ~~~ y
             , Variation x' y' <- shrinkV (Variation x y) ]
        else []
      HighEquivEverything ->
        if l==H || l'==H then
          [Variation (Labeled L x) (Labeled L y) | x ~~~ y] ++
          [Variation (Labeled l x') (Labeled l' y) | x' <- shrinkF x] ++
          [Variation (Labeled l x) (Labeled l' y') | y' <- shrinkF y]
        else []

prop_shrinkV :: (Observable a, Arbitrary a, ?f :: DynFlags) => Variation a -> Bool 
prop_shrinkV = all (\ (Variation v v') -> v ~~~ v') . shrinkV

instance Observable Int where
  (~~~)                 = (==)
  vary = return
  shrinkV (Variation i _i') = [Variation j j | j <- shrink i]

instance Observable Bool where
  (~~~) = (==)
  vary = return
  shrinkV (Variation i _i') = [Variation j j | j <- shrink i]

instance (Show a, Observable a) => Observable (Maybe a) where
  Just x  ~~~ Just y  = x ~~~ y
  Nothing ~~~ Nothing = True
  _       ~~~ _       = False
  
  vary (Just x) = Just <$> vary x
  vary Nothing  = pure Nothing
  
  shrinkV (Variation Nothing Nothing)   = []
  shrinkV (Variation (Just x) (Just y)) =
    Variation Nothing Nothing : map (fmap Just) (shrinkV $ Variation x y)
  shrinkV v = errorShrinkV "Maybe a" v

instance (Show a, Observable a) => Observable [a] where
  xs ~~~ ys             = length xs == length ys && and (zipWith (~~~) xs ys)
  vary = mapM vary
  shrinkV (Variation [] []) = []
  shrinkV (Variation (a:as) (a':as')) = 
       Variation as as'
    :  [Variation (v:as) (v':as') | Variation v v' <- shrinkV (Variation a a')]
    ++ [Variation (a:vs) (a':vs') | Variation vs vs' <- shrinkV (Variation as as')]
  shrinkV v = errorShrinkV "[a]" v

instance (Observable a,Observable b) => Observable (a,b) where
  (a,b) ~~~ (a',b') = a ~~~ a' && b ~~~ b'
  vary (a,b) = liftM2 (,) (vary a) (vary b)
  shrinkV (Variation (a,b) (a',b')) = 
      [Variation (v,b) (v',b') | Variation v v' <- shrinkV (Variation a a')]
   ++ [Variation (a,v) (a',v') | Variation v v' <- shrinkV (Variation b b')]

instance (Observable a,Observable b,Observable c) => Observable (a,b,c) where
  (a,b,c) ~~~ (a',b',c') = (a,(b,c)) ~~~ (a',(b',c'))
  vary (a,b,c) = liftM3 (,,) (vary a) (vary b) (vary c)
  shrinkV  = map (fmap $ \ (a,(b,c)) -> (a,b,c)) .
                 shrinkV . fmap (\ (a,b,c) -> (a,(b,c)))

prop_observable_refl  :: (Observable a, _) => a -> Bool
prop_observable_sym   :: (Show a, Observable a, _) => a -> Property 
prop_observable_trans :: (Show a, Observable a, _) => a -> Property
prop_observable_refl  a = a ~~~ a
prop_observable_sym   a = forAll (vary a) $ \b -> (a ~~~ b) && (b ~~~ a)
prop_observable_trans a = forAll (vary a) $ \b ->
                          forAll (vary b) $ \c ->
                            a ~~~ c

prop_observable :: (Show a, Observable a, _) => a -> Property
prop_observable a = 
  forAll (vary a) (~~~ a)

