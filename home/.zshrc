# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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

# if [ -n "$INSIDE_EMACS" ]; then
#     export ZSH_THEME="lambda-mod"
# else
#     export ZSH_THEME="powerlevel10k/powerlevel10k"
#     source ~/.p10k.zsh 
# fi

# history setup
HISTFILE=$HOME/.zhistory
SAVEHIST=1000
HISTSIZE=999
setopt share_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_verify

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
plugins=(asdf brew colored-man-pages common-aliases  iterm2 fasd fzf git github npm  macos wakatime zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

unsetopt nomatch

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

export EDITOR='/opt/homebrew/bin/emacsclient -nw'
export ZSH_WAKATIME_BIN=/opt/homebrew/bin/wakatime-cli
autoload zmv
alias mmv='noglob zmv -W'
alias le='open -a /usr/local/opt/emacs-plus/Emacs.app'
alias -g C='| wc -l'
alias hl='highlight -O xterm256'
alias -g HL='|highlight -O xterm256 -'
alias -g F='| fx'
alias emc="emacsclient -nw"
alias ccat='/bin/cat'
alias cat='/opt/homebrew/bin/bat'
alias Make=`which make`
alias make="$(which make) --"
alias Ls="/bin/ls"
alias ls="eza --icons=always"

autoload edit-command-line
zle -N edit-command-line

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"
# unalias run-help
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
# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

bindkey '^X^E' edit-command-line
bindkey -s ^F "tmux-sessionizer\n"

function rk_autojump {
    targetdir=$(fasd -ld | fzf)
    cd $targetdir
}

# -- Use fd instead of find for fzf --

export FZF_DEFAULT_COMMAND="fd --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd --type=d --hidden --strip-cwd-prefix --exclude .git"

# Use fd (https://github.com/sharkdp/fd) for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
    fd --hidden --exclude .git . "$1"
}

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
    fd --type=d --hidden --exclude .git . "$1"
}


# --- setup fzf theme ---
fg="#CBE0F0"
bg="#011628"
bg_highlight="#143652"
purple="#B388FF"
blue="#06BCE4"
cyan="#2CF9ED"

export FZF_DEFAULT_OPTS="--color=fg:${fg},bg:${bg},hl:${purple},fg+:${fg},bg+:${bg_highlight},hl+:${purple},info:${blue},prompt:${cyan},pointer:${cyan},marker:${cyan},spinner:${cyan},header:${cyan}"

# ----- Bat (better cat) -----

export BAT_THEME="Monokai Extended Bright"

bindkey -s ^T "rk_autojump\n"

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

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
export CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# Generated for envman. Do not edit.
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"

source "${XDG_CONFIG_HOME:-$HOME/.config}/asdf-direnv/zshrc"


if [ -n "$INSIDE_EMACS" ]; then
    direnv reload
fi

# # >>> conda initialize >>>
# # !! Contents within this block are managed by 'conda init' !!
# __conda_setup="$('/Users/rodk/miniforge3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
# if [ $? -eq 0 ]; then
#     eval "$__conda_setup"
# else
#     if [ -f "/Users/rodk/miniforge3/etc/profile.d/conda.sh" ]; then
#         . "/Users/rodk/miniforge3/etc/profile.d/conda.sh"
#     else
#         export PATH="/Users/rodk/miniforge3/bin:$PATH"
#     fi
# fi
# unset __conda_setup
# # <<< conda initialize <<<



# JINA_CLI_BEGIN

## autocomplete
if [[ ! -o interactive ]]; then
    return
fi

compctl -K _jina jina

_jina() {
  local words completions
  read -cA words

  if [ "${#words}" -eq 2 ]; then
    completions="$(jina commands)"
  else
    completions="$(jina completions ${words[2,-2]})"
  fi

  reply=(${(ps:
:)completions})
}

# session-wise fix
ulimit -n 4096
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

# JINA_CLI_END
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
    source "$EAT_SHELL_INTEGRATION_DIR/zsh"


export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib"
export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/sqlite/lib/pkgconfig"
export PATH="/opt/homebrew/opt/sqlite/bin:$PATH"
export PATH="$PATH:/opt/homebrew/opt/sqlite/bin"

source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

source ~/fzf-git.sh/fzf-git.sh

export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"

eval "$(fzf --zsh)"
source ~/.p10k.zsh 

export HISTSIZE=100000
export SAVEHIST=100000
export HISTFILE=~/.zsh_history
export HISTTIMEFORMAT="%d/%m/%y %T "
export HISTCONTROL=ignoredups:erasedups
export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help"


export ZSH_THEME="powerlevel10k/powerlevel10k"
