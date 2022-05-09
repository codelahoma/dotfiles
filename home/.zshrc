# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
iterm2_print_user_vars() {
    iterm2_set_user_var gitBranch $((git branch 2> /dev/null) | grep \* | cut -c3-)
}
ITERM2_SQUELCH_MARK=1
# export ZSH_THEME="lambda-mod"

if [ -n "$INSIDE_EMACS" ]; then
    export ZSH_THEME="lambda-mod"
else
    export ZSH_THEME="powerlevel10k/powerlevel10k"
    source ~/.p10k.zsh 
fi


# Example aliases
# alias zshconfig="mate ~/.zshrc"
alias ohmyzsh="emacsclient -n ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to  shown in the command execution time stamp
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(asdf brew colored-man-pages common-aliases docker-compose docker iterm2 fasd fzf git github npm  macos wakatime)

source $ZSH/oh-my-zsh.sh

unsetopt nomatch

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

export EDITOR='emacsclient -nw'

autoload zmv
alias mmv='noglob zmv -W'
alias le='open -a /usr/local/opt/emacs-plus/Emacs.app'
alias -g C='| wc -l'
alias hl='highlight -O xterm256'
alias -g HL='|highlight -O xterm256 -'
alias -g F='| fx'
alias xmlf='xmllint --format '
alias susalt='ssh saltmaster-3 sudo salt'
alias ss="script/stop && script/start -d && script/attach --tail=100 &"
alias ssd="script-docker/stop && script-docker/start -d && script-docker/attach --tail=100 &"
alias emc="emacsclient -nw"
alias bsalt="ssh -t saltmaster vim /srv"
alias esalt="ssh -t saltmaster sudo vim /srv"
alias ccat='/bin/cat'
alias cat='/opt/homebrew/bin/bat'
alias org='emacsclient -nw ~/Dropbox/org/gtd/gtd.org'
alias Make=`which make`
alias make="$(which make) --"
autoload edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"
unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/help

export HOMESHICK_DIR=/opt/homebrew/opt/homeshick
source /opt/homebrew/opt/homeshick/homeshick.sh

# initialise completions with ZSH's compinit
autoload -Uz compinit && compinit

if which swiftenv > /dev/null; then eval "$(swiftenv init -)"; fi

NVM_DIR=~/.nvm
alias loadrvm='[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"'
alias loadnvm='[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"'
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

eval "$(asdf exec direnv hook zsh)"
# A shortcut for asdf managed direnv.
direnv() { asdf exec direnv "$@"; }

eval "$(/opt/homebrew/bin/brew shellenv)"

if [[ -f ~/.zshrc.local ]]; then
  source ~/.zshrc.local
fi


# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
# [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export DISPLAY_MAC=`ifconfig en0 | grep "inet " | cut -d " " -f2`:0

function startx() {
	  if [ -z "$(ps -ef|grep XQuartz|grep -v grep)" ] ; then
	      open -a XQuartz
        socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\" &
	  fi
}
alias dgui='docker run -e DISPLAY=$DISPLAY_MAC -it'

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
export CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"

