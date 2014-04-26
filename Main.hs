{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Parse
import Data
import Util
import Render
----------------------
-------- MAIN --------
----------------------

main = go1 --Parse.testDate


go1 = do
    n <- Parse.notes "override_notes.log"
    n1 <- Parse.notes "override_notes.log"
    ns <-  return $ merge n n1
    grp <- return $ group ns
    html <- render "notesGroup.html" grp
    print html