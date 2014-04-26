{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Util where

import Data
import qualified Data.Map

merge :: [Note] -> [Note] -> [Note]
merge n1 n2 = fromMap $ Data.Map.union (toMap n2) (toMap n1) 

toMap :: [Note] -> Data.Map.Map Version Note
toMap notes = Data.Map.fromList $ fmap (\x -> (version x, x)) notes

fromMap :: Data.Map.Map Version Note -> [Note]
fromMap map = Data.Map.elems map

group :: [Note] -> [Group]
group ns = [ Group { from = Version 3 0 0 0, to = Version 4 0 0 0, notes = ns}]
