#!/bin/sh

if [ ! -f "$1" ]; then
  touch "$1"
fi

# Remove quarantine bit that may get set for extensions MacOSX doesn't
# recognize, and that may cause an unneeded security dialog to appear. (We
# *know* there are no viruses on this file, 'cause we just created it)
xattr -d com.apple.quarantine "$1" 

set -eu
exec ~/bin/emacsclient -c "$1"
