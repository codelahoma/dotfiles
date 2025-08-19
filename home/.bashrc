# Bash RC - Clean environment for Claude Code scripts and automation
# This provides a minimal, predictable shell environment

# Basic shell options for script compatibility
set -o emacs          # Emacs-style command line editing
shopt -s checkwinsize # Update LINES and COLUMNS after each command
set +H                # Disable history expansion (prevents ! issues in scripts)

# Environment variables
export EDITOR=${EDITOR:-vim}
export PAGER=${PAGER:-less}
export LANG=${LANG:-en_US.UTF-8}

# Clean PATH - start with system defaults
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Homebrew (if available)
if [[ -x /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# asdf version manager (if available)
if [[ -f "$HOME/.asdf/asdf.sh" ]]; then
    source "$HOME/.asdf/asdf.sh"
    # Add asdf completions for bash
    [[ -f "$HOME/.asdf/completions/asdf.bash" ]] && source "$HOME/.asdf/completions/asdf.bash"
fi

# User directories
[[ -d "$HOME/.local/bin" ]] && export PATH="$HOME/.local/bin:$PATH"
[[ -d "$HOME/bin" ]] && export PATH="$HOME/bin:$PATH"

# Node.js global packages
[[ -d "$HOME/.npm-global/bin" ]] && export PATH="$HOME/.npm-global/bin:$PATH"

# Python environment
export PYTHONDONTWRITEBYTECODE=1
export PYTHONUNBUFFERED=1

# Git configuration for scripts
export GIT_EDITOR="$EDITOR"

# CRITICAL: Prevent any claude alias from being set
# This ensures Claude Code can execute properly in subshells
unset CLAUDE_ALIAS 2>/dev/null || true
unalias claude 2>/dev/null || true

# Essential utilities for script safety
alias rm='rm -i'
alias cp='cp -i' 
alias mv='mv -i'

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Minimal prompt for non-interactive use
export PS1='$ '
# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/rodk/.lmstudio/bin"
# End of LM Studio CLI section

