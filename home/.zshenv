# PATH configuration
PATH="/opt/homebrew/bin:/Users/rodk/.local/bin:~/bin:/opt/local/bin:/opt/local/sbin:~/usr/local/opt/coreutils/libexec/gnubin:/Library/Android/sdk/tools:~/Library/Android/sdk/platform-tools:$PATH"
export PATH="/Users/rodk/bin:/usr/local/opt/qt@5.5/bin:$PATH"
export PATH="/opt/homebrew/opt/sqlite/bin:$PATH"
export PATH="$PATH:/Users/rodk/.lmstudio/bin"
export PATH=/Users/rodk/Library/Python/3.9/bin:$PATH
export MANPATH="/usr/local/opt/coreutils/libexec/gnuman:/usr/local/man:$MANPATH"

# Default editor
export EDITOR="$HOME/.local/bin/ec"
export VISUAL="$HOME/.local/bin/ec"
export FCEDIT="$HOME/.local/bin/ec"

# Tool configuration
export BAT_THEME="Monokai Extended Bright"
export HOMESHICK_DIR=/opt/homebrew/opt/homeshick

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
export KALEIDOSCOPE_DIR=/Users/rodk/github/Kaleidoscope
export ZSH_WAKATIME_BIN=/opt/homebrew/bin/wakatime-cli
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview'"

# macOS-specific configuration
if [[ "$(uname -s)" == "Darwin" ]]; then

    # SDK workaround from @marcosgomesborges
    [[ -n "$MACOSX_DEPLOYMENT_TARGET" ]] || export MACOSX_DEPLOYMENT_TARGET="$(sw_vers -productVersion | tr -d '\n')"
    [[ -n "$SDKROOT" ]] || export SDKROOT="$(xcrun --show-sdk-path)"

    # Workaround for OpenSSL header/library paths (for GCC & LINKER)
    pfx_openssl="/usr/local/opt/openssl@1.1"
    if [[ -d "$pfx_openssl" ]]; then
        export CPATH="${pfx_openssl}/include:${CPATH}"
        export LIBRARY_PATH="${pfx_openssl}/lib:${LIBRARY_PATH}"
    fi

    # GCC/libgccjit for Emacs native compilation
    if [[ -d "/opt/homebrew/lib/gcc/15" ]]; then
        export LIBRARY_PATH="/opt/homebrew/lib/gcc/15:${LIBRARY_PATH}"
    fi

    # Compilation flags for Homebrew packages
    export LDFLAGS="-L/opt/homebrew/opt/sqlite/lib -L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
    export CPPFLAGS="-I/opt/homebrew/opt/sqlite/include -I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"
    export PKG_CONFIG_PATH="/opt/homebrew/opt/sqlite/lib/pkgconfig"

    # macOS system flags
    export OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES

fi
