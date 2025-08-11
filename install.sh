#!/bin/bash

os=${OSTYPE//[0-9.]/}
echo $os

# Creates a symlink to the file, prompting if a file already exists
# there
# $1 = original file location
# $2 = symlink's location
function create_symlink {
    should_create_symlink=true
    if [ -e $2 ] || [ -L $2 ]
    then
        echo $2 " already exists. Replace file? (y/n)"
        read replace_file
        if [ "$replace_file" == "y" ] ; then
            rm -rf $2
        else
	          should_create_symlink=false
        fi
    fi

    if $should_create_symlink ; then
	      ln -sf $1 $2
    fi
}

# emacs
create_symlink `pwd`"/emacs/.emacs" $HOME"/.emacs"
create_symlink `pwd`"/emacs/.emacs.d" $HOME"/.emacs.d"

# terminal
create_symlink `pwd`/terminal/.zshrc $HOME"/.zshrc"
create_symlink `pwd`/terminal/.p10k.zsh $HOME"/.p10k.zsh"
create_symlink `pwd`/terminal/.git-prompt.sh $HOME"/.git-prompt.sh"

# git
git config --global user.name "Charlie Andrews"
git config --global core.editor cursor
git config --global credential.helper 'cache --timeout=86400'
create_symlink `pwd`/terminal/.gitignore_global $HOME"/.gitignore_global"
git config --global core.excludesfile $HOME/.gitignore_global
