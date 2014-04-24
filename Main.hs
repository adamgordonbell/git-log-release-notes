{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative
import Data.Either (rights)
import Data.Monoid hiding (Product)
import Data.String
import Data.Char (toLower)
import Data.Foldable (foldMap)
-- ByteString stuff
import Data.ByteString.Char8 (ByteString,singleton)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy (toChunks)
-- HTTP protocol to perform downloads
import Network.HTTP.Conduit

import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.Text.Lazy.IO as TL 
import Data.Text.Lazy.Encoding as TE
import Data.Data 
import Data.Generics 
import Data.Char


-----------------------
-------- TYPES --------
-----------------------

data Note = Note { version :: String, date :: LocalTime, description :: String } deriving (Eq,Show, Data, Typeable)

instance Ord Note where
  n1 <= n2 = date n1 <= date n2

data Notes = Notes { notes :: [Note] } deriving (Data, Typeable)

-----------------------
------- PARSING -------
-----------------------



-----------------------
------- MERGING -------
-----------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
  if x <= y
     then x : merge xs (y:ys)
     else y : merge (x:xs) ys



----------------------
-------- MAIN --------
----------------------
main = hastacheFile defaultConfig template context
 >>= TL.putStrLn . TE.decodeUtf8 
   


-- begin example
template = "note.html"
context = mkGenericContext $ Notes [
    Note { version="VCH 3.0.0.201", date= LocalTime { localDay = fromGregorian 2001 1 1, localTimeOfDay = TimeOfDay 12 0 0 } ,  description = "changed x"},
    Note { version="VCH 3.0.0.205", date= LocalTime { localDay = fromGregorian 2001 1 1, localTimeOfDay = TimeOfDay 12 0 0 } ,  description = "changed x"}
    ]
