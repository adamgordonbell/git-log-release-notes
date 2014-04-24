{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Word
import Data.Time
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Either (rights)
import Data.Monoid hiding (Product)
import Data.String
import Data.Foldable (foldMap)
import Data.Text as T
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
data Date = Date { year :: Int, month :: Int, day :: Int} deriving (Eq,Show, Data, Typeable)
data Note = Note { version :: Text, date :: Date, description :: Text } deriving (Eq,Show, Data, Typeable)

instance Ord Note where
  n1 <= n2 = date n1 <= date n2

instance Ord Date where
  compare  d1 d2 = compare (year d1) (year d2) 
    <> compare (month d1) (month d2)
    <> compare (day d1) (day d2)

data Notes = Notes { notes :: [Note] } deriving (Data, Typeable, Show)

-----------------------
------- PARSING -------
-----------------------
anyBetween start ends = start *> Data.Attoparsec.Text.takeWhile (not.flip elem ends)
fromUptoIncl startP endChars = startP *> takeTill (flip elem endChars)

dateParser :: Parser Date
dateParser = do
  y  <- Data.Attoparsec.Combinator.count 4 digit
  char '-'
  mm <- Data.Attoparsec.Combinator.count 2 digit
  char '-'
  d  <- Data.Attoparsec.Combinator.count 2 digit
  return $
    Date (read y) (read mm) (read d)

versionParser :: Parser Text
versionParser = fromUptoIncl (stringCI "(tag: ") "("


messageParser :: Parser Text
messageParser = 
    skipWhile (/= '|') 
    *> skip (== '|') 
    *> takeTill isEndOfLine


noteParser :: Parser Note
noteParser = do
  d <- dateParser
  string "| "
  v <- versionParser
  s <- messageParser
  return $ Note v d s

notesParser :: Parser Notes
notesParser = liftA Notes $ many $ noteParser <* endOfLine

-- Test Parse
testDate = print $ parseOnly dateParser "2013-06-30"

testVersion = print $ parseOnly versionParser "(tag: VCH3.0.10.206(default))"
   
testNotes = print $ parseOnly notesParser "2014-04-17| (tag: VCH3.0.10.206(default))|Subject Line\n2014-04-17| (tag: VCH3.0.10.206(default))|Subject Line"



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


fromRight (Right a) = a

----------------------
-------- MAIN --------
----------------------

main = 
-- printTemplate
    testNotes

printTemplate = hastacheFile defaultConfig template parsedContext
 >>= TL.putStrLn . TE.decodeUtf8 

-- begin example
template = "note.html"

simpleContext = mkGenericContext $ Notes [
    Note { version="VCH3.0.10.217", date= Date 2014 03 02 ,  description = "Added Transfer of data from existing web.configs to new web.configs, this means we can push a new web.config as part of the update."},
    Note { version="VCH3.0.10.217", date= Date 2014 03 01 ,  description = "Added spinner to all ajax requests"}
    ]

parsedContext =   mkGenericContext $ fromRight $ parseOnly notesParser "2014-04-17| (tag: VCH3.0.10.206(default))|Subject Line\n2014-04-17| (tag: VCH3.0.10.206(default))|Subject Line"

----
-- Todo
-- Parsing multiple notes not working??
-- Read from file??
