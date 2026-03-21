# Powerlevel10k instant prompt (must be first, before any console output)
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Homebrew (macOS)
if [[ -f /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Oh-My-Zsh configuration
ZSH=$HOME/.oh-my-zsh
export ZSH_THEME="powerlevel10k/powerlevel10k"

# Core plugins (cross-platform)
plugins=(eza fzf git github npm zsh-autosuggestions zoxide)

# macOS-only plugins
if [[ "$OSTYPE" == darwin* ]]; then
  plugins+=(1password brew macos wakatime)
fi

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
alias emc="emacsclient -nw"
alias ccat='/bin/cat'
alias Ls="/bin/ls"

if [[ "$OSTYPE" == darwin* ]]; then
  alias le='open -a /opt/homebrew/opt/emacs-plus/Emacs.app'
  alias cat='/opt/homebrew/bin/bat'
else
  # On Ubuntu/Debian, bat is installed as batcat
  if command -v batcat &>/dev/null; then
    alias cat='batcat'
  elif command -v bat &>/dev/null; then
    alias cat='bat'
  fi
fi
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
# Determine fd and bat binary names (Ubuntu uses fdfind/batcat)
if command -v fd &>/dev/null; then
  _fd_cmd="fd"
elif command -v fdfind &>/dev/null; then
  _fd_cmd="fdfind"
fi

if command -v bat &>/dev/null; then
  _bat_cmd="bat"
elif command -v batcat &>/dev/null; then
  _bat_cmd="batcat"
fi

if [[ -n "${_fd_cmd:-}" ]]; then
  export FZF_DEFAULT_COMMAND="$_fd_cmd --hidden --strip-cwd-prefix --exclude .git"
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  export FZF_ALT_C_COMMAND="$_fd_cmd --type=d --hidden --strip-cwd-prefix --exclude .git"
fi

if [[ -n "${_bat_cmd:-}" ]]; then
  export FZF_CTRL_T_OPTS="--preview '$_bat_cmd -n --color=always --line-range :500 {}'"
fi

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
    ${_fd_cmd:-fd} --hidden --exclude .git . "$1"
}

_fzf_compgen_dir() {
    ${_fd_cmd:-fd} --type=d --hidden --exclude .git . "$1"
}

if command -v fzf &>/dev/null; then
  eval "$(fzf --zsh)"
fi

# Tool integrations
# mise for runtime version management
if command -v mise &>/dev/null; then
  eval "$(mise activate zsh)"
fi
if [ -n "$INSIDE_EMACS" ]; then
    direnv reload
fi

# Ansible vault password file for secret decryption
export ANSIBLE_VAULT_PASSWORD_FILE="$HOME/.vault_pass"

# Homeshick for dotfiles (HOMESHICK_DIR set in .zshenv)
# Homeshick (HOMESHICK_DIR set in .zshenv)
if [[ -f /opt/homebrew/opt/homeshick/homeshick.sh ]]; then
  source /opt/homebrew/opt/homeshick/homeshick.sh
elif [[ -f "$HOME/.homesick/repos/homeshick/homeshick.sh" ]]; then
  source "$HOME/.homesick/repos/homeshick/homeshick.sh"
fi

# Environment variables
if [[ "$OSTYPE" == darwin* ]]; then
  export DISPLAY_MAC=$(ifconfig en0 2>/dev/null | grep "inet " | cut -d " " -f2):0
  export HELPDIR=/usr/local/share/zsh/help
fi

# System limits
ulimit -n 4096

# EAT shell integration (Emacs)
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# Local customizations
if [[ -f ~/.zshrc.local ]]; then
  source ~/.zshrc.local
fi

# Additional plugins (syntax-highlighting must be last)
[[ -f ~/fzf-git.sh/fzf-git.sh ]] && source ~/fzf-git.sh/fzf-git.sh

# zsh-syntax-highlighting (must be last)
if [[ -f /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
  source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
elif [[ -f /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]]; then
  source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# Powerlevel10k configuration (after oh-my-zsh)
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
