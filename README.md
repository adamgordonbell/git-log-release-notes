git-log-release-notes
=====================

This is part of our release not generation process.  run it like so:

    runhaskell Main.hs --logs=notes.log --overrideLogs=override_notes.log --groupSize=10  --template=notesGroup.html  --resultFile=releaseNotes.html

where :

 * logs is the git log file
 * overrideLogs is an override file, if you need to replace the text of some commit messages
 * group size specificies how may releases to group together
 * template is a handlebars template used for rendering the html output
 * resultFile is where the output should go
 
It will include git logs tagged with a release number and the expected format is that produced by this command:

    git log --date=short --format="%ad|%d|%s" --simplify-by-decoration > log.txt
    
It uses hastache for handlebars, attoparsec for parsing and CmdArgs for argument passing
