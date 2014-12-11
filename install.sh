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

# anonymous pro font
curl -O http://www.ms-studio.com/FontSales/AnonymousPro-1.002.zip
unzip AnonymousPro-1.002.zip
if [ "$os" == "linux-gnu" ] ; then
    sudo cp -R AnonymousPro-1.002.001/ /usr/share/fonts/truetype/
elif [ "$os" == "darwin" ] ; then
    open AnonymousPro-1.002.001/*.ttf
fi
rm -rf Anon*
rm -rf ._Anon*


# emacs
create_symlink `pwd`"/emacs/.emacs" $HOME"/.emacs"
create_symlink `pwd`"/emacs/.emacs.d" $HOME"/.emacs.d"

# terminal
create_symlink `pwd`/terminal/.profile $HOME"/.profile"
create_symlink `pwd`/terminal/.bashrc $HOME"/.bashrc"

# no way to set default theme from command line?

echo ""
echo "TODO"
echo "1. Set default terminal style."
