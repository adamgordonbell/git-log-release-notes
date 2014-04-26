{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data where

import Data.Word
import Data.Time
import Data.Text as T
import Data.Data 
import Data.Generics 

-----------------------
-------- TYPES --------
-----------------------
data Date = Date { year :: Int, month :: Int, day :: Int} deriving (Eq,Show, Data, Typeable)
data Version = Version { major :: Int, minor :: Int, build :: Int, revision :: Int} deriving (Eq,Show, Data, Typeable, Ord)
data Note = Note { version :: Version, date :: Date, description :: Text } deriving (Eq,Show, Data, Typeable)

instance Ord Note where
  n1 <= n2 = version n1 <= version n2

data Group = Group { from :: Version, to :: Version, notes :: [Note]} deriving (Eq,Show, Data, Typeable)

data Groups = Groups { groups :: [Group] } deriving (Data, Typeable)
