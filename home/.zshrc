# Powerlevel10k instant prompt (must be first, before any console output)
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Homebrew (must be early, many tools depend on this PATH)
eval "$(/opt/homebrew/bin/brew shellenv)"

# Oh-My-Zsh configuration
ZSH=$HOME/.oh-my-zsh
export ZSH_THEME="powerlevel10k/powerlevel10k"

# Oh-My-Zsh plugins
plugins=(1password asdf brew eza iterm2 fzf git github npm macos wakatime zsh-autosuggestions zoxide)

# Plugin configuration
zstyle ":omz:plugins:eza" 'dirs-first' yes
zstyle ":omz:plugins:eza" 'git-status' yes
zstyle ":omz:plugins:eza" 'header' yes
zstyle ":omz:plugins:eza" 'show-group' no
zstyle ":omz:plugins:eza" 'icons' yes

# Source Oh-My-Zsh
source $ZSH/oh-my-zsh.sh

# History configuration
export HISTSIZE=100000
export SAVEHIST=100000
export HISTFILE=~/.zsh_history
export HISTTIMEFORMAT="%d/%m/%y %T "
setopt share_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_all_dups
setopt hist_verify

# Zsh options
unsetopt nomatch

# Completion setup (fpath must come before compinit)
fpath+=~/.zfunc
autoload -Uz compinit && compinit

# Autoloads
autoload zmv
autoload edit-command-line
autoload run-help
zle -N edit-command-line

# Key bindings
bindkey '^X^E' edit-command-line
bindkey -s ^F "tmux-sessionizer\n"
bindkey -s ^T "rk_autojump\n"

# Aliases
alias mmv='noglob zmv -W'
alias le='open -a /opt/homebrew/opt/emacs-plus/Emacs.app'
alias emc="emacsclient -nw"
alias ccat='/bin/cat'
alias cat='/opt/homebrew/bin/bat'
alias Ls="/bin/ls"
alias rm='rm -i'
alias ohmyzsh="emacsclient -n ~/.oh-my-zsh"
alias orgg='(cd ~/personal/org-files && git-sync && cd .catalyst && git-sync)'

# Global aliases
alias -g C='| wc -l'
alias -g HL='| highlight -O xterm256 -'
alias -g F='| fx'
alias -g G='| grep'
alias -g S='| sort'
alias -g H='| head'

# Functions
function rk_autojump {
    targetdir=$(zoxide query -l | fzf)
    cd "$targetdir"
}

function gi() {
    curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@
}

# FZF configuration
export FZF_DEFAULT_COMMAND="fd --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd --type=d --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"

# FZF theme
fg="#CBE0F0"
bg="#011628"
bg_highlight="#143652"
purple="#B388FF"
blue="#06BCE4"
cyan="#2CF9ED"
export FZF_DEFAULT_OPTS="--color=fg:${fg},bg:${bg},hl:${purple},fg+:${fg},bg+:${bg_highlight},hl+:${purple},info:${blue},prompt:${cyan},pointer:${cyan},marker:${cyan},spinner:${cyan},header:${cyan}"

_fzf_compgen_path() {
    fd --hidden --exclude .git . "$1"
}

_fzf_compgen_dir() {
    fd --type=d --hidden --exclude .git . "$1"
}

eval "$(fzf --zsh)"

# Tool integrations
# asdf with direnv for project-specific environments
eval "$(asdf exec direnv hook zsh)"
direnv() { asdf exec direnv "$@"; }
if [ -n "$INSIDE_EMACS" ]; then
    direnv reload
fi

# Homeshick for dotfiles (HOMESHICK_DIR set in .zshenv)
source /opt/homebrew/opt/homeshick/homeshick.sh

# iTerm2 integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
iterm2_print_user_vars() {
    iterm2_set_user_var gitBranch $((git branch 2> /dev/null) | grep \* | cut -c3-)
}
ITERM2_SQUELCH_MARK=1

# Environment variables
export DISPLAY_MAC=`ifconfig en0 | grep "inet " | cut -d " " -f2`:0
export HELPDIR=/usr/local/share/zsh/help

# System limits
ulimit -n 4096

# EAT shell integration (Emacs)
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# Local customizations
if [[ -f ~/.zshrc.local ]]; then
  source ~/.zshrc.local
fi

# Additional plugins (syntax-highlighting must be last)
source ~/fzf-git.sh/fzf-git.sh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Powerlevel10k configuration (after oh-my-zsh)
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
