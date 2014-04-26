{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Util where

import Data
import qualified Data.Map

merge :: [Note] -> [Note] -> [Note]
merge n1 n2 = fromMap $ Data.Map.union (toMap n2) (toMap n1) 
 where
  toMap :: [Note] -> Data.Map.Map Version Note
  toMap notes = Data.Map.fromList $ fmap (\x -> (version x, x)) notes
  fromMap :: Data.Map.Map Version Note -> [Note]
  fromMap map = reverse $ Data.Map.elems map

group :: [Note] -> [Group]
group ns = fmap (fillGroup ns) Util.groups
 
fillGroup :: [Note] -> Group -> Group
fillGroup ns (Group f t _) = Group f t ns1
 where
    ns1 = (Prelude.filter (\ (Note v _ _) -> f <= v && v <= t) ns)

groups = reverse [ 
    Group { from = Version 3 0 10 70, to = Version 3 0 10 79, notes = []},
    Group { from = Version 3 0 10 80, to = Version 3 0 10 89, notes = []},
    Group { from = Version 3 0 10 90, to = Version 3 0 10 99, notes = []}
    ]

