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
    # export ZSH_THEME="cdimascio-lambda"
    # export ZSH_THEME="wedisagree"
    # export POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(vcs newline pyenv context dir)
    # export POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator background_jobs history time)
    POWERLEVEL9K_MODE='nerdfont-complete'

    # Please only use this battery segment if you have material icons in your nerd font (or font)
    # Otherwise, use the font awesome one in "User Segments"
    # prompt_zsh_battery_level() {
    #     local percentage1=`pmset -g ps  |  sed -n 's/.*[[:blank:]]+*\(.*%\).*/\1/p'`
    #     local percentage=`echo "${percentage1//\%}"`
    #     local color='%F{red}'
    #     local symbol="\uf00d"
    #     pmset -g ps | grep "discharging" > /dev/null
    #     if [ $? -eq 0 ]; then
    #         local charging="false";
    #     else
    #         local charging="true";
    #     fi
    #     if [ $percentage -le 20 ]
    #     then symbol='\uf579' ; color='%F{red}' ;
    #          #10%
    #     elif [ $percentage -gt 19 ] && [ $percentage -le 30 ]
    #     then symbol="\uf57a" ; color='%F{red}' ;
    #          #20%
    #     elif [ $percentage -gt 29 ] && [ $percentage -le 40 ]
    #     then symbol="\uf57b" ; color='%F{yellow}' ;
    #          #35%
    #     elif [ $percentage -gt 39 ] && [ $percentage -le 50 ]
    #     then symbol="\uf57c" ; color='%F{yellow}' ;
    #          #45%
    #     elif [ $percentage -gt 49 ] && [ $percentage -le 60 ]
    #     then symbol="\uf57d" ; color='%F{blue}' ;
    #          #55%
    #     elif [ $percentage -gt 59 ] && [ $percentage -le 70 ]
    #     then symbol="\uf57e" ; color='%F{blue}' ;
    #          #65%
    #     elif [ $percentage -gt 69 ] && [ $percentage -le 80 ]
    #     then symbol="\uf57f" ; color='%F{blue}' ;
    #          #75%
    #     elif [ $percentage -gt 79 ] && [ $percentage -le 90 ]
    #     then symbol="\uf580" ; color='%F{blue}' ;
    #          #85%
    #     elif [ $percentage -gt 89 ] && [ $percentage -le 99 ]
    #     then symbol="\uf581" ; color='%F{blue}' ;
    #          #85%
    #     elif [ $percentage -gt 98 ]
    #     then symbol="\uf578" ; color='%F{green}' ;
    #          #100%
    #     fi
    #     if [ $charging = "true" ];
    #     then color='%F{green}'; if [ $percentage -gt 98 ]; then symbol='\uf584'; fi
    #     fi
    #     echo -n "%{$color%}$symbol" ;
    # }

    # zsh_internet_signal(){
    #     local color
    #     local symbol="\uf7ba"
    #     if ifconfig en0 | grep inactive &> /dev/null; then
    #         color="%F{red}"
    #     else
    #         color="%F{blue}"
    #     fi
    #     echo -n "%{$color%}$symbol "
    # }

    POWERLEVEL9K_PROMPT_ON_NEWLINE=true
    POWERLEVEL9K_PROMPT_ADD_NEWLINE=true
    POWERLEVEL9K_RPROMPT_ON_NEWLINE=true
    POWERLEVEL9K_SHORTEN_DIR_LENGTH=2
    POWERLEVEL9K_SHORTEN_STRATEGY="truncate_beginning"
    POWERLEVEL9K_RVM_BACKGROUND="black"
    POWERLEVEL9K_RVM_FOREGROUND="249"
    POWERLEVEL9K_RVM_VISUAL_IDENTIFIER_COLOR="red"
    POWERLEVEL9K_TIME_BACKGROUND="black"
    POWERLEVEL9K_TIME_FOREGROUND="249"
    POWERLEVEL9K_TIME_FORMAT="\UF43A %D{%I:%M   %m.%d.%y}"
    POWERLEVEL9K_RVM_BACKGROUND="black"
    POWERLEVEL9K_RVM_FOREGROUND="249"
    POWERLEVEL9K_RVM_VISUAL_IDENTIFIER_COLOR="red"
    POWERLEVEL9K_STATUS_VERBOSE=false
    POWERLEVEL9K_VCS_CLEAN_FOREGROUND='black'
    POWERLEVEL9K_VCS_CLEAN_BACKGROUND='green'
    POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND='black'
    POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND='yellow'
    POWERLEVEL9K_VCS_MODIFIED_FOREGROUND='white'
    POWERLEVEL9K_VCS_MODIFIED_BACKGROUND='black'
    POWERLEVEL9K_COMMAND_EXECUTION_TIME_BACKGROUND='black'
    POWERLEVEL9K_COMMAND_EXECUTION_TIME_FOREGROUND='blue'
    POWERLEVEL9K_FOLDER_ICON=''
    POWERLEVEL9K_STATUS_OK_IN_NON_VERBOSE=true
    POWERLEVEL9K_STATUS_VERBOSE=false
    POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=0
    POWERLEVEL9K_VCS_UNTRACKED_ICON='\u25CF'
    POWERLEVEL9K_VCS_UNSTAGED_ICON='\u00b1'
    POWERLEVEL9K_VCS_INCOMING_CHANGES_ICON='\u2193'
    POWERLEVEL9K_VCS_OUTGOING_CHANGES_ICON='\u2191'
    POWERLEVEL9K_VCS_COMMIT_ICON="\uf417"
    POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX="%F{blue}\u256D\u2500%f"
    POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX="%F{blue}\u2570\uf460%f "
    # POWERLEVEL9K_CUSTOM_BATTERY_STATUS="prompt_zsh_battery_level"
    # POWERLEVEL9K_CUSTOM_INTERNET_STATUS="zsh_internet_signal"
    # POWERLEVEL9K_CUSTOM_INTERNET_STATUS_BACKGROUND="gray"
    POWERLEVEL9K_CUSTOM_ITERM_MARK="iterm2_prompt_mark"

    POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(custom_iterm_mark time newline context os_icon custom_battery_status ssh root_indicator dir vcs)
    POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(command_execution_time pyenv custom_internet_status)
    HIST_STAMPS="mm/dd/yyyy"
    DISABLE_UPDATE_PROMPT=true
fi

# ZSH_THEME="robbyrussell"

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
plugins=(brew colored-man-pages common-aliases docker-compose docker iterm2 fasd git github npm  macos pyenv wakatime)

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
alias penv='eval "$(pyenv init -)"'
alias ccat='/bin/cat'
alias cat='/opt/homebrew/bin/bat'
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

eval "$(direnv hook zsh)"
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

eval "$(pyenv init -)"
export LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/bzip2/lib"
export CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/bzip2/include"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

