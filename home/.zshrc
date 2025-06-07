# Powerlevel10k instant prompt
source ~/.p10k.zsh
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

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

# History configuration
export HISTSIZE=100000
export SAVEHIST=100000
export HISTFILE=~/.zsh_history
export HISTTIMEFORMAT="%d/%m/%y %T "
export HISTCONTROL=ignoredups:erasedups
export HISTIGNORE="ls:cd:cd -:pwd:exit:date:* --help"
setopt share_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_verify

# Source Oh-My-Zsh
source $ZSH/oh-my-zsh.sh

# Zsh options
unsetopt nomatch

# Editor and basic utilities
export EDITOR='/opt/homebrew/bin/emacsclient -nw'

# Key bindings
bindkey '^X^E' edit-command-line
bindkey -s ^F "tmux-sessionizer\n"
bindkey -s ^T "rk_autojump\n"

# Autoloads
autoload -Uz compinit && compinit
autoload zmv
autoload edit-command-line
autoload run-help
zle -N edit-command-line

# Aliases
alias mmv='noglob zmv -W'
alias le='open -a /usr/local/opt/emacs-plus/Emacs.app'
alias emc="emacsclient -nw"
alias ccat='/bin/cat'
alias cat='/opt/homebrew/bin/bat'
alias Make=`which make`
alias make="$(which make) --"
alias Ls="/bin/ls"
alias rm='rm -i'
alias ohmyzsh="emacsclient -n ~/.oh-my-zsh"

# Global aliases
alias -g C='| wc -l'
alias -g HL='|highlight -O xterm256 -'
alias -g F='| fx'
alias -g G='| grep'
alias -g S='| sort'
alias -g H='| head'

# Functions
function rk_autojump {
    targetdir=$(fasd -ld | fzf)
    cd $targetdir
}

function gi() { 
    curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@ ;
}

function startx() {
    if [ -z "$(ps -ef|grep XQuartz|grep -v grep)" ] ; then
        open -a XQuartz
        socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:\"$DISPLAY\" &
    fi
}

# FZF Configuration
export FZF_DEFAULT_COMMAND="fd --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd --type=d --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"

# FZF custom theme
fg="#CBE0F0"
bg="#011628"
bg_highlight="#143652"
purple="#B388FF"
blue="#06BCE4"
cyan="#2CF9ED"
export FZF_DEFAULT_OPTS="--color=fg:${fg},bg:${bg},hl:${purple},fg+:${fg},bg+:${bg_highlight},hl+:${purple},info:${blue},prompt:${cyan},pointer:${cyan},marker:${cyan},spinner:${cyan},header:${cyan}"

# FZF completion functions
_fzf_compgen_path() {
    fd --hidden --exclude .git . "$1"
}

_fzf_compgen_dir() {
    fd --type=d --hidden --exclude .git . "$1"
}
eval "$(fzf --zsh)"

# Tool integrations
# iTerm2 integration
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
iterm2_print_user_vars() {
    iterm2_set_user_var gitBranch $((git branch 2> /dev/null) | grep \* | cut -c3-)
}
ITERM2_SQUELCH_MARK=1

# Bat configuration
export BAT_THEME="Monokai Extended Bright"

# Package managers
# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# asdf 
# Note: loads direnv for managing project-specific environments
eval "$(asdf exec direnv hook zsh)"
direnv() { asdf exec direnv "$@"; }
if [ -n "$INSIDE_EMACS" ]; then
    direnv reload
fi

# Homeshick for dotfiles
export HOMESHICK_DIR=/opt/homebrew/opt/homeshick
source /opt/homebrew/opt/homeshick/homeshick.sh

# NVM (Node Version Manager)
NVM_DIR=~/.nvm
alias loadnvm='[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"'
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# RVM (Ruby Version Manager) - only add to PATH
export PATH="$PATH:$HOME/.rvm/bin"
alias loadrvm='[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"'

# swiftenv
if which swiftenv > /dev/null; then eval "$(swiftenv init -)"; fi

# Zoxide (better cd)
eval "$(zoxide init zsh)"

# Environment variables
export DISPLAY_MAC=`ifconfig en0 | grep "inet " | cut -d " " -f2`:0
export HELPDIR=/usr/local/share/zsh/help
export ZSH_WAKATIME_BIN=/opt/homebrew/bin/wakatime-cli

# System flags
ulimit -n 4096  # Increase file descriptor limit
export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

# Library paths
export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib -L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include -I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/sqlite/lib/pkgconfig"
export PATH="/opt/homebrew/opt/sqlite/bin:$PATH"

# Additional plugins
source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/fzf-git.sh/fzf-git.sh

# envman
[ -s "$HOME/.config/envman/load.sh" ] && source "$HOME/.config/envman/load.sh"
source "${XDG_CONFIG_HOME:-$HOME/.config}/asdf-direnv/zshrc"

# EAT shell integration (Emacs)
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"

# Local customizations
if [[ -f ~/.zshrc.local ]]; then
  source ~/.zshrc.local
fi

# p10k configuration
[[ ! -f ~/.homesick/repos/dotfiles/home/.p10k.zsh ]] || source ~/.homesick/repos/dotfiles/home/.p10k.zsh
# FlowLoom
export PATH="/Users/rodk/.homesick/repos/dotfiles/.flowloom/bin:$PATH"

# Additional aliases and functions
function gi() { curl -sLw "\n" https://www.toptal.com/developers/gitignore/api/$@ ;}
alias cl="SHELL=/bin/bash claude"

# FlowLoom
export PATH="/Users/rodk/demo/flowloom/packages/flowloom_installer/atlas-up-ai/bin:$PATH"

# FlowLoom
export PATH="/Users/rodk/demo/atlas-up-ai/.flowloom/bin:$PATH"

# FlowLoom
export PATH="/Users/rodk/github/flowloom/test-flowloom-install/.flowloom/bin:$PATH"

# FlowLoom
export PATH="/Users/rodk/demo/flowloom/packages/flowloom_installer/.flowloom/bin:$PATH"

# FlowLoom
export PATH="/Users/rodk/github/flowloom/test-final-installer/.flowloom/bin:$PATH"

# FlowLoom
export PATH="/Users/rodk/github/flowloom/test-easter-egg-default/.flowloom/bin:$PATH"

# Claude-generated shell enhancements for notifications and development
source ~/zshrc_additions.sh

# FlowLoom
export PATH="/Users/rodk/.homesick/repos/dotfiles/.flowloom/bin:$PATH"
