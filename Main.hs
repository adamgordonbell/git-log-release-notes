{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified ReleaseNotes.Parse as Parse
import ReleaseNotes.Data
import ReleaseNotes.Util
import ReleaseNotes.Render

import qualified Data.Text as Text
import qualified Data.Text.IO as Text


----------------------
-------- MAIN --------
----------------------
main = go1 --Parse2.testLine -- go2 --Parse.testDate

go1 = do 
      n <- fmap convertLines $ Parse.lines "notes_complicated.log"
      n1 <- fmap convertLines $ Parse.lines "override_notes.log"
      ns <-  return $ merge n n1
      grp <- return $ group 10 ns
      html <- render "notesGroup.html" grp
      Text.putStrLn html
      Text.writeFile "releaseNotes.html" html