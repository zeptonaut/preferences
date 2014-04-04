# Convenient aliases
alias ked="emacsclient -e '(kill-emacs)'"
alias ed='emacs --daemon'
alias e="emacsclient -c"
alias ka="killall -9"
alias gc="git commit -m"
alias gp="git push origin master"

# dircolors
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# Add bash completion if it exists
if [ hash brew >/dev/null 2>&1 && -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

# Make sure we're using 256 colors
export TERM="xterm-256color"

# Go needs $GOPATH to get new binaries and such
export GOPATH=$HOME/go

# Prefer /usr/local/bin binaries of /usr/bin binaries
export PATH="/usr/local/bin:$PATH"

# Command line prompt
export PS1="\[\e[01;30m\][\u@\h:\[\e[0;31m\]\w\[\e[01;30m\]] \[\e[m\]"




