# Emacs aliases
alias ed='emacs --daemon'
alias e="emacsclient -c"

export TERM=xterm-256color
alias ls="ls --color"

# dircolors
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

export PS1="\[\e[01;37m\][\u@\h:\[\e[0;31m\]\w\[\e[01;37m\]] \[\e[m\]"

# Don't forget about .bashrc!
[[ -r ~/.bashrc ]] && . ~/.bashrc
