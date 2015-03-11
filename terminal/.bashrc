# Convenient aliases
alias ked="emacsclient -e '(kill-emacs)'"
alias edm='emacs --daemon'
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

# Use xterm-256color as the terminal unless we're using tmux, then use
# screen-256color. tmux requires a screen terminal in order to work
# properly
if [ -n "$TMUX" ]; then
  export TERM="screen-256color"
else
  export TERM="xterm-256color"
fi

# Go needs $GOPATH to get new binaries and such
export GOPATH="$HOME/go"

# Add $GOPATH to $PATH
export PATH="$PATH:$GOPATH/bin"

# Necessary for Chromium development
export CHROME_DEVEL_SANDBOX=/usr/local/sbin/chrome-devel-sandbox

# Add Go appengine to $PATH and $GOPATH
export APPENGINE_SDK="$HOME/bin/go_appengine"
export PATH="$PATH:$APPENGINE_SDK"
alias ae="export GOROOT=$APPENGINE_SDK/goroot"

# Prefer /usr/local/bin binaries of /usr/bin binaries
#export PATH="/usr/local/bin:/usr/local/share/npm/bin:$PATH:$GOPATH/bin"

# Add Chrome's depot_tools to the end of the path
export PATH="$PATH:$HOME/bin/depot_tools"

# Command line prompt
export PS1="\[\e[01;30m\][\u@\h:\[\e[0;31m\]\w\[\e[01;30m\]] \[\e[m\]"

# Don't forget about .google-bashrc!
[[ -r ~/.google-bashrc ]] && . ~/.google-bashrc

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
