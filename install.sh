#!/bin/bash

# emacs
ln -s `pwd`/emacs/.emacs ~/.emacs
ln -s `pwd`/emacs/.emacs.d ~/.emacs.d

# terminal
open terminal/TomorrowNight.terminal
ln -s `pwd`/terminal/.profile ~/.profile
# no way to set default theme from command line?
