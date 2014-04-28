{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import qualified ReleaseNotes.Parse as Parse
import ReleaseNotes.Data
import ReleaseNotes.Util
import ReleaseNotes.Render

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.Console.CmdArgs



----------------------
-------- MAIN --------
----------------------
main = go1 --Parse2.testLine -- go2 --Parse.testDate

go1 = do 
      Args{..} <- cmdArgs $ defaultArgs
      n <- fmap convertLines $ Parse.lines logs
      n1 <- fmap convertLines $ Parse.lines overrideLogs
      ns <-  return $ merge n n1
      grp <- return $ group groupSize ns
      html <- render template grp
      Text.putStrLn html
      Text.writeFile resultFile html

--These are the command line arguments
data Args = Args { 
    logs :: String, 
    overrideLogs:: String, 
    groupSize :: Int,
    template :: String,
    resultFile :: String
    } deriving (Data,Typeable,Show)
    
defaultArgs = Args {
    logs= "notes.log", 
    overrideLogs="override_notes.log", 
    groupSize=10,
    template = "notesGroup.html",
    resultFile = "releaseNotes.html"
    }
