PATH="~/bin:/usr/local/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin:~/usr/local/opt/coreutils/libexec/gnubin:/Library/Android/sdk/tools:~/Library/Android/sdk/platform-tools:$PATH"
export PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:/usr/local/man:$MANPATH"

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
# Locale
export LANG="en_US.UTF-8"
export LC_COLLATE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_MONETARY="en_US.UTF-8"
export LC_NUMERIC="en_US.UTF-8"
export LC_TIME="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
# User configuration

export TERM=xterm-256color

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# Set NVM_DIR if it isn't already defined
[[ -z "$NVM_DIR" ]] && export NVM_DIR="$HOME/.nvm"

# Load nvm if it exists
[[ -f "$NVM_DIR/nvm.sh" ]] && source "$NVM_DIR/nvm.sh"

