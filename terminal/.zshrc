# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Fix issue where vscode can't see output of zsh command
[[ "$TERM_PROGRAM" == "vscode" ]] && . "$(code --locate-shell-integration-path zsh)"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

export JAVA_HOME="/Library/Java/JavaVirtualMachines/openjdk.jdk/Contents/Home"

alias gprune='git checkout master && comm -12 <(git branch | sed "s/ *//g") <(git remote prune origin | sed "s/^.*origin\///g") | xargs -L1 -J % git branch -D %'
alias gw='git webdiff'
alias gwm='git webdiff master'

# Install oh-my-zsh if it's not already installed
[[ -d ~/.oh-my-zsh ]] || sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# TODO(charliea): Install powerlevel 10k.

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
# ZSH_THEME="powerlevel10k/powerlevel10k"
ZSH_THEME="gozilla"
POWERLEVEL9K_MODE="awesome-patched"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f ~/.p10k.zsh ]] && source ~/.p10k.zsh

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


# Convenient aliases
alias ked="emacsclient -e '(kill-emacs)'"
alias edm='emacs --daemon'
alias e="emacsclient -c"
alias ka="killall -9"
alias gc="git commit --allow-empty-message -am ''"
alias gp="git push origin HEAD"
alias grep='grep --color=auto'

export EDITOR="emacs -q"
export GIT_MERGE_AUTOEDIT=no

# Load system-specific load files
# case `uname` in
# Linux)
#   source $HOME/.bashrc-linux
#   ;;
# Darwin)
#   source $HOME/.bashrc-mac
#   ;;
# esac

# Set the dircolors to coordinate with solarized-dark
# test -e "$HOME/github/gnome-terminal-colors-solarized/dircolors" && eval `dircolors /usr/local/google/home/charliea/github/gnome-terminal-colors-solarized/dircolors`

# Add node and npm to path
export PATH="$PATH:$HOME/bin/node/bin"

# Go needs $GOPATH to get new binaries and such
export GOPATH="$HOME/go"

# Add $HOME/bin to path
export PATH="$PATH:$HOME/bin"

# Add $GOPATH to $PATH
export PATH="$PATH:$GOPATH/bin"

# Necessary for Chromium development
export CHROME_DEVEL_SANDBOX=/usr/local/sbin/chrome-devel-sandbox

# Add Go appengine to $PATH and $GOPATH
export APPENGINE_SDK="$HOME/bin/go_appengine"
export PATH="$PATH:$APPENGINE_SDK"
alias ae="export GOROOT=$APPENGINE_SDK/goroot"

# Prefer /usr/local/bin binaries of /usr/bin binaries
export PATH="$HOME/.local/bin:/usr/local/share/npm/bin:$GOPATH/bin:/usr/local/bin:$PATH"

# Set the size of core dumps to unlimited if the command's available
hash ulimit 2>/dev/null && {
  ulimit -c unlimited > /dev/null
}

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Add "git trim" to the path
export PATH="$PATH:$HOME/github/git-trim"

export PATH="$PATH:$HOME/Library/Python/3.7/bin"

export PYTHONPATH="/usr/local/google_appengine"

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Put the Android platform tools into place if they exist
export ANDROID_SDK="$HOME/Library/Android/sdk"
export ANDROID_PLATFORM_TOOLS="$ANDROID_SDK/platform-tools"
export ANDROID_EMULATOR="$ANDROID_SDK/emulator"
[[ -d $ANDROID_PLATFORM_TOOLS ]] && export PATH="$ANDROID_PLATFORM_TOOLS:$PATH"
[[ -d $ANDROID_EMULATOR ]] && export PATH="$ANDROID_EMULATOR:$PATH"
[[ -d $ANDROID_SDK ]] && export PATH="$ANDROID_SDK:$PATH"

export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-17.jdk/Contents/Home

alias h='heroku local:run bundle exec'
eval "$(direnv hook zsh)"

. $(brew --prefix asdf)/libexec/asdf.sh

export PATH="$HOME/.poetry/bin:$PATH"

# export DIRENV_LOG_FORMAT="$(printf "\033[1;30m %%s\033[0m")"
# export DIRENV_LOG_FORMAT="$(printf "\033[1;30m %%s\033[0m")"
export DIRENV_LOG_FORMAT="`tput setaf 3``tput bold`%s`tput sgr0`"

export RUBY_CONFIGURE_OPTS="--with-libyaml-dir=$(brew --prefix libyaml) --with-openssl-dir=$(brew --prefix openssl@3) --disable-install-doc"

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/charlie/.lmstudio/bin"
