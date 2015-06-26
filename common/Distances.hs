module Distances (showDiff) where

import Data.Function
import qualified Data.Vector as V
import Data.Vector.Distance
import Data.Monoid

data Opt a = Some a | None deriving (Eq, Ord, Show)

instance Monoid a => Monoid (Opt a) where
  mempty = Some mempty
  mappend (Some a) (Some b) = Some (mappend a b)
  mappend _ _ = None

data Oper v = Del Int v | Ins Int v | Sub Int v v deriving (Eq, Ord, Show)

params = Params
  { equivalent = (==)
  , delete = Del
  , insert = Ins
  , substitute = Sub
  , cost = \x -> case x of { Sub _ _ _ -> None ; _ -> Some (1 :: Sum Int) }
  , positionOffset = \x -> case x of { Del _ _ -> 0 ; _ -> 1 }
  }

data InsDel a = InsDel [a] [a] | Match [a] deriving Show

leastCh :: String -> String -> (Opt (Sum Int), [Oper Char])
leastCh = leastChanges params `on` V.fromList

asInsDel :: String -> [Oper Char] -> [InsDel Char]
asInsDel s ops = asInsDel' s 0 ops
  where
    asInsDel' [] m [] = []
    asInsDel' s m [] = [Match s]
    asInsDel' s m (Del m' x : ops) =
      let (s1, s2) = splitAt (m' - m) s in
      case s2 of
        x' : s2' | x' == x -> match s1 . del x $ asInsDel' s2' m' ops
        _ -> e
    asInsDel' s m (Ins m' x : ops) =
      let (s1, s2) = splitAt (m' - m) s in
      match s1 . ins x $ asInsDel' s2 (m' + 1) ops
      
    e = error $ "asInsDel: " ++ s ++ " " ++ show ops

del :: a -> [InsDel a] -> [InsDel a]
del a (InsDel is ds : ids) = InsDel is (a : ds) : ids
del a ids = InsDel [] [a] : ids

ins :: a -> [InsDel a] -> [InsDel a]
ins a (InsDel is ds : ids) = InsDel (a : is) ds : ids
ins a ids = InsDel [a] [] : ids

match :: [a] -> [InsDel a] -> [InsDel a]
match [] = id
match as = (Match as :)

showInsDel :: [InsDel Char] -> String
showInsDel [] = ""
showInsDel (Match s : ids) = s ++ showInsDel ids
showInsDel (InsDel is ds : ids) = "{" ++ is ++ "/" ++ ds ++ "}" ++ showInsDel ids

showDiff :: String -> String -> String
showDiff s1 s2 =
  let (_, ops) = leastCh s1 s2 in
  showInsDel $ asInsDel s1 ops

