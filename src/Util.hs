{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Util where

import Data
import qualified Data.List
import qualified Data.Map

merge :: [Note] -> [Note] -> [Note]
merge n1 n2 = fromMap $ Data.Map.union (toMap n2) (toMap n1) 
 where
  toMap :: [Note] -> Data.Map.Map Version Note
  toMap notes = Data.Map.fromList $ fmap (\x -> (version x, x)) notes
  fromMap :: Data.Map.Map Version Note -> [Note]
  fromMap map = reverse $ Data.Map.elems map

group :: Int -> [Note] -> [Group]
group groupSize ns = fmap (fillGroup ns) (generateGroups groupSize ns)
 
fillGroup :: [Note] -> Group -> Group
fillGroup ns (Group f t _) = Group f t ns1
 where
    ns1 = (Prelude.filter (\ (Note v _ _) -> f <= v && v <= t) ns)

generateGroups :: Int -> [Note] -> [Group]
generateGroups groupSize ns = Data.List.nub $ fmap (range . version) ns
 where 
     range (Version m1 m2 b r) = Group { from = Version m1 m2 b (floor r), to = Version m1 m2 b (ceil r), notes = []}
     floor r = r `div` groupSize * groupSize
     ceil r = r `div` groupSize * groupSize + groupSize -1
