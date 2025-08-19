PATH="/opt/homebrew/bin:/Users/rodk/.local/bin:~/bin:/opt/local/bin:/opt/local/sbin:~/usr/local/opt/coreutils/libexec/gnubin:/Library/Android/sdk/tools:~/Library/Android/sdk/platform-tools:$PATH"
export PATH="/Users/rodk/bin:/usr/local/opt/qt@5.5/bin:$PATH"
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:/usr/local/man:$MANPATH"
export PATH=/Users/rodk/Library/Python/3.9/bin:$PATH

# # for virutalenvwrapper
# export WORKON_HOME=~/Envs

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
export SAVEHIST=50000
export KALEIDOSCOPE_DIR=/Users/rodk/github/Kaleidoscope
export ZSH_WAKATIME_BIN=/opt/homebrew/bin/wakatime-cli
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"

# key bindings
bindkey -s ^F "tmux-sessionizer\n"

function rk_autojump {
                     targetdir=$(fasd -ld | fzf)
                     cd $targetdir
                     }

bindkey -s ^T "rk_autojump\n"


if [[ "$(uname -s)"  == "Darwin" ]] ; then

    # Adopted SDK workaround from @marcosgomesborges
    [[ -n "$MACOSX_DEPLOYMENT_TARGET"  ]]  || export MACOSX_DEPLOYMENT_TARGET="$(sw_vers -productVersion | tr -d '\n')"    # e.g.: 10.14
    [[ -n "$SDKROOT" ]]                    || export SDKROOT="$(xcrun --show-sdk-path)"

    # Workaround for OpenSSL header/library paths (for GCC & LINKER)
    pfx_openssl="/usr/local/opt/openssl@1.1"  # Change this if openssl was not installed via homebrew 
    if [[ -d "$pfx_openssl" ]]  ; then
        export CPATH="${pfx_openssl}/include:${CPATH}"                # Headers for C pre-processor
        export LIBRARY_PATH="${pfx_openssl}/lib:${LIBRARY_PATH}"      # libraries (for the linker)
    fi

    ulimit -n unlimited

fi
