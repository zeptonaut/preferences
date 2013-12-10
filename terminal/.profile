source ~/bin/change-directory-until

export P4EDITOR="emacsclient -c"
export EDITOR="emacsclient -c"
export TEST_UNDECLARED_OUTPUTS_DIR="/usr/local/google/tmp"

# Start MySQL
/usr/local/google/mysql_data/startup.sh

# SQL aliases
alias sql.par=/google/data/ro/projects/production/mysql/python/sql.par
alias adsdb-connect-devel="sql.par mystubby:/bns/local/slave/production/mysql/home/xfp-devel-db/primary/shard0:root:e848xUSP:ads0"

# Emacs aliases
alias ed='emacs --daemon'
alias e="emacsclient -c"

# Commonly used shortcuts
alias ka='killall -9'

# Prodaccess aliases
alias pa='prodaccess'
alias pag='prodaccess -g'

# Git aliases
alias gcomm='git commit -a --message="`date +"%m-%d-%Y  %r"` Auto-commit";'
alias ge='git commit --all --message="`date +"%m-%d-%Y  %r"` Auto-commit for export to perforce"; git5 export --no-background-upload'
alias gm='git commit --all --message="`date +"%m-%d-%Y  %r"` Auto-commit for git5 mail "; git5 mail'
alias gs="git5 sync"
alias gl="git log --format=oneline"
alias gb="git branch"
alias gco="git checkout"
alias gc="git changes"
alias gca='git commit --all --message="`date +"%m-%d-%Y %r"` commit all"'
alias gp="git p"
alias g5d="git5 diff"
alias g5o="git5 diff --name-only"

# XFP aliases
alias xfpsdm="gfpdevel-hosted --jobs=CORE_FE,STUBBY_API,TRAFFICKING_SUPERDEVMODE"

export PATH=/usr/local/bin:/usr/local/sbin:$PATH:/usr/local/go/bin:~/bin

# git autocompletion
if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi

# dircolors
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
fi

export PS1="\[\e[01;37m\][\u@\h:\[\e[0;31m\]\w\[\e[01;37m\]] \[\e[m\]"
