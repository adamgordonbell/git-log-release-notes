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

lines :: String -> IO [Line]
lines filepath = do
    text <- getFile filepath
    return . fromRight $ parseOnly linesParser text
  where        
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
    refs <- refParser `sepBy` asciiCI ", "
    char ')'
    return refs
    
refParser :: Parser Ref
refParser = -- undefined
    versionTagParser <|>
    tagParser <|>
    branchParser
 where
     versionTagParser = asciiCI "tag: VCH" *> fmap VersionTag versionParser <* asciiCI "(default)"
     branchParser = fmap Branch $ words
     tagParser = asciiCI "tag: " *> fmap Tag words
     words = many1 $ satisfy (notInClass ",)")

versionParser :: Parser Version
versionParser = do
    major <- takeDigits
    char '.'
    minor <- takeDigits
    char '.'
    build <- takeDigits
    char '.'
    revision <- takeDigits
    return $ Version (readText major) (readText minor) (readText build) (readText revision)
  where 
      takeDigits = Parse.takeWhile BParse.isDigit
      readText = read. unpack

messageParser :: Parser Text
messageParser = takeTill isEndOfLine

-- Test Parse
testLine = print $ parseOnly lineParser "2014-04-23||javascript redirect to login if 401 returned from ajax"

testRefParser = print $ parseOnly refsParser "(HEAD, tag: VCH3.0.10.218(default), tfs/default, master)"
