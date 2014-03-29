# Convenient aliases
alias ed='emacs --daemon'
alias e="emacsclient -c"
alias ka="killall -9"
alias gc="git commit -m"
alias gp="git push origin master"

export TERM=xterm-256color

# dircolors
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

# Bash completion
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

# Go needs $GOPATH to get new binaries and such
export GOPATH=$HOME/go

# Prefer /usr/local/bin binaries of /usr/bin binaries
export PATH="/usr/local/bin:$PATH"

# Command line prompt
export PS1="\[\e[01;30m\][\u@\h:\[\e[0;31m\]\w\[\e[01;30m\]] \[\e[m\]"

# Don't forget about .bashrc!
[[ -r ~/.bashrc ]] && . ~/.bashrc
