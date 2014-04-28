{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified ReleaseNotes.Parse as Parse
import qualified ReleaseNotes.Parse2 as Parse2
import ReleaseNotes.Data
import ReleaseNotes.Util
import ReleaseNotes.Render

import qualified Data.Text as Text
import qualified Data.Text.IO as Text


----------------------
-------- MAIN --------
----------------------
main = go2 --Parse2.testLine -- go2 --Parse.testDate


go1 = do
    n <- Parse.notes "notes.log"
    n1 <- Parse.notes "override_notes.log"
    ns <-  return $ merge n n1
    grp <- return $ group 10 ns
    html <- render "notesGroup.html" grp
    Text.putStrLn  html
    Text.writeFile "releaseNotes.html" html

go2 = do 
      lines <- Parse2.lines "notes_complicated.log"
      notes <- return $ fmap convert lines
      putStrLn . show $ notes