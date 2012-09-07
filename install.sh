#!/bin/bash

# Creates a symlink to the file, prompting if a file already exists
# there
# $1 = original file location
# $2 = symlink's location
function create_symlink {
    if [ -e $2 ] || [ -L $2 ]
    then
        echo $2 " already exists. Replace file? (y/n)"
        read replace_file
        if [ replace_file == "y" ]
        then
            ln -sf $1 $2
        fi
    fi
}

# emacs
create_symlink `pwd`"/emacs/.emacs" $HOME"/.emacs"
create_symlink `pwd`"/emacs/.emacs.d" $HOME"/.emacs.d"

# terminal
open terminal/TomorrowNight.terminal
create_symlink `pwd`/terminal/.profile $HOME"/.profile"
# no way to set default theme from command line?

echo ""
echo "TODO"
echo "1. Set default terminal style."
