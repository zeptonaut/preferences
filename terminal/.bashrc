source ~/.profile
git() { if [[ $1 == 'merge' ]]; then echo 'Use git5 merge, not git merge. git merge does not understand how to merge the READONLY link and it can corrupt your branch, so stay away from it.'; else command git "$@"; fi; }

function mkcl() {
    if [ "x$1" == "x" ]; then
      echo "usage: mkcl <client name>"
      return 1
    fi
    mkdir $1
    chmod 750 $1
    cd $1
    echo P4CLIENT=charliea-$1 >> .p4config
    g4 -e true client 
    g4 sync
    cd google3
}

source /home/build/nonconf/google3/java/com/google/ads/publisher/scripts/xfp/bashrc.sh

export TERM=xterm-256color

alias ls="ls --color"
