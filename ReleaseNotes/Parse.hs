{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module ReleaseNotes.Parse where
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text as Parse
import qualified Data.Attoparsec.Combinator as Parse 
import qualified Data.Attoparsec.ByteString.Char8 as BParse
import Control.Applicative
import Data.Text as T
-- ByteString stuff

import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.Text.Lazy 
import qualified Data.Text.Lazy.IO
import Data.Text.Lazy.Encoding as TE 
import Control.Monad

import ReleaseNotes.Data

-----------------------
------- PARSING -------
-----------------------

notes :: String -> IO [Note]
notes filepath = do
    text <- getFile filepath
    return . fromRight $ parseOnly notesParser text

getFile :: String -> IO Text
getFile fp = Data.Text.Lazy.toStrict <$> Data.Text.Lazy.IO.readFile fp

fromRight (Right a) = a
fromRight (Left a) = error a

----------------------------------------------

notesParser :: Parser [Note]
notesParser = many1 $ noteParser <* endOfLine

noteParser :: Parser Note
noteParser = do
  d <- dateParser
  string "| "
  v <- versionParser
  s <- messageParser
  return $ Note v d s

dateParser :: Parser Date
dateParser = do
  y  <- Parse.count 4 digit
  char '-'
  mm <- Parse.count 2 digit
  char '-'
  d  <- Parse.count 2 digit
  return $
    Date (read y) (read mm) (read d)

versionParser :: Parser Version
versionParser = do
    header <- asciiCI "(tag: VCH"
    major <- Parse.takeWhile BParse.isDigit
    char '.'
    minor <- Parse.takeWhile BParse.isDigit
    char '.'
    build <- Parse.takeWhile BParse.isDigit
    char '.'
    revision <- Parse.takeWhile BParse.isDigit
    return $ Version (read . unpack $ major) (read . unpack $  minor) (read . unpack $ build) (read . unpack $ revision)


messageParser :: Parser Text
messageParser = 
    skipWhile (/= '|') 
    *> skip (== '|') 
    *> takeTill isEndOfLine


anyBetween start ends = start *> Data.Attoparsec.Text.takeWhile (not.flip elem ends)
fromUptoIncl startP endChars = startP *> takeTill (flip elem endChars)

-- Test Parse
-- testDate = print $ parseOnly dateParser "2013-06-30"

-- testVersion = print $ parseOnly versionParser "(tag: VCH3.0.10.206(default))"
   
-- testNotes = print $ parseOnly notesParser "2014-04-17| (tag: VCH3.0.10.206(default))|Subject Line\n2014-04-17| (tag: VCH3.0.10.206(default))|Subject Line\n"

-- exampleTrouble = "(HEAD, tag: VCH3.0.10.218(default), tfs/default, master)"
-- pointerParser = "(" .*> many1 pointer1Parser <*. ")"
