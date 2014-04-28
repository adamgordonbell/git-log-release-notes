{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module ReleaseNotes.Parse2 where
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

lines :: String -> IO [Line]
lines filepath = do
    text <- getFile filepath
    return . fromRight $ parseOnly linesParser text

getFile :: String -> IO Text
getFile fp = Data.Text.Lazy.toStrict <$> Data.Text.Lazy.IO.readFile fp

fromRight (Right a) = a
fromRight (Left a) = error a

----------------------------------------------

linesParser :: Parser [Line]
linesParser = many1 $ lineParser <* endOfLine

lineParser :: Parser Line
lineParser =  
 do
  d <- dateParser
  string "|"
  -- Zero or one lists of ref, flattened 
  -- to handle commits with no refs
  rs <- many' refsParser
  string "|"
  s <- messageParser
  return $ Line d (join rs) s

dateParser :: Parser Date
dateParser = do
  y  <- Parse.count 4 digit
  char '-'
  mm <- Parse.count 2 digit
  char '-'
  d  <- Parse.count 2 digit
  return $
    Date (read y) (read mm) (read d)

refsParser :: Parser [Ref]
refsParser = do
    char ' '
    char '('
    refs <- refParser `sepBy` asciiCI ", " -- <* many' (char ',')
    char ')'
    return refs
    
refParser :: Parser Ref
refParser = -- undefined
    versionParser <|>
    tagParser <|>
    branchParser
 where
     branchParser = fmap Branch $ words
     versionParser = asciiCI "tag: VCH" *> fmap VersionTag versionParser1 <* asciiCI "(default)"
     tagParser = asciiCI "tag: " *> fmap Tag words
     words = many1 $ satisfy (notInClass ",)")

versionParser1 :: Parser Version
versionParser1 = do
    major <- Parse.takeWhile BParse.isDigit
    char '.'
    minor <- Parse.takeWhile BParse.isDigit
    char '.'
    build <- Parse.takeWhile BParse.isDigit
    char '.'
    revision <- Parse.takeWhile BParse.isDigit
    return $ Version (read . unpack $ major) (read . unpack $  minor) (read . unpack $ build) (read . unpack $ revision)

-- data Ref = Branch String | VersionTag Version | Tag String deriving (Eq,Show, Data, Typeable, Ord)

messageParser :: Parser Text
messageParser = takeTill isEndOfLine


anyBetween start ends = start *> Data.Attoparsec.Text.takeWhile (not.flip elem ends)
fromUptoIncl startP endChars = startP *> takeTill (flip elem endChars)

-- Test Parse
-- testDate = print $ parseOnly dateParser "2013-06-30"

-- testVersion = print $ parseOnly versionParser "(tag: VCH3.0.10.206(default))"
   
-- testNotes = print $ parseOnly notesParser "2014-04-17| (tag: VCH3.0.10.206(default))|Subject Line\n2014-04-17| (tag: VCH3.0.10.206(default))|Subject Line\n"

-- exampleTrouble = "(HEAD, tag: VCH3.0.10.218(default), tfs/default, master)"
-- pointerParser = "(" .*> many1 pointer1Parser <*. ")"

testLine = print $ parseOnly lineParser "2014-04-23||javascript redirect to login if 401 returned from ajax"

testRefParser = print $ parseOnly refsParser "(HEAD, tag: VCH3.0.10.218(default), tfs/default, master)"
