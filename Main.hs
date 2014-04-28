{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}

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
      (Args logArg logOverrideArg groupArg) <- cmdArgs $ Args "notes.log" "notes_.log" 10
      n <- fmap convertLines $ Parse.lines logArg
      n1 <- fmap convertLines $ Parse.lines logOverrideArg
      ns <-  return $ merge n n1
      grp <- return $ group groupArg ns
      html <- render "notesGroup.html" grp
      Text.putStrLn html
      Text.writeFile "releaseNotes.html" html

data Args = Args { logs :: String, overrideLogs:: String, groupSize :: Int} deriving (Data,Typeable,Show)