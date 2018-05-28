#!/bin/bash

# syncdirs - a bash script that uses rsync to sync two directories. It also takes a file containing the names of the files/dirs to exclude.
# IMPORTANT: the names in the exclusion file must be relative to the source folder

# EXAMPLE:
# $ syncdirs ~/exclude.txt ~/Dropbox/ ~/backup/Dropbox/
# Note the slash at the end of each folder's name.

rsync -avP --delete --exclude-from=$1 $2 "$3"


