## Umask
umask 022

## Keybindings
bindkey -e

## Paths
typeset -U cdpath CDPATH

typeset -U fpath FPATH
case $OSTYPE in
    darwin* )
        if type brew > /dev/null 2>&1; then
            fpath=($(brew --prefix)/share/zsh/site-functions $fpath)
        fi
        ;;
    * )
        ;;
esac
fpath=(
    /usr/local/share/zsh-completions(N-/)
    $fpath
)

typeset -U manpath MANPATH
manpath=(
    # for coreutils
    /usr/local/opt/coreutils/libexec/gnuman(N-/)
    # for system
    /usr/local/man(N-/)
    /usr/local/share/man(N-/)
    /usr/share/man(N-/)
    $manpath
)

# Sets the tty erase character to whatever is specified as the backspace code
# in the terminfo database
if [ -x /usr/bin/tput ]; then
    # We can't do this with "dumb" terminal
    if [ "x$(tput kbs)" != "x" ]; then
        stty erase $(tput kbs)
    elif [ -x /usr/bin/wc ]; then
        # We can't do this with "dumb" terminal
        if [ "$(tput kbs | wc -c)" -gt 0 ]; then
            stty erase $(tput kbs)
        fi
    fi
fi

## Color settings
if [ -f ~/.dircolors ]; then
    if type gdircolors > /dev/null 2>&1; then
        eval $(gdircolors ~/.dircolors)
    elif type dircolors > /dev/null 2>&1; then
        eval $(dircolors ~/.dircolors)
    fi
fi

## Completion
autoload -Uz compinit; compinit
setopt always_to_end
setopt auto_list
setopt auto_menu
setopt list_packed
setopt list_types
if [ -n "$LS_COLORS" ]; then
    zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
fi

## History
HISTFILE=$ZDOTDIR/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt append_history
setopt extended_history
setopt inc_append_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt share_history

# History search
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

## Prompt
setopt prompt_subst
autoload -Uz vcs_info
autoload -Uz colors; colors;
zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' formats \
"%{$fg[yellow]%}%c%{$fg[green]%}%u%{$reset_color%}\
[%{$fg[cyan]%}%b%{$reset_color%}]%{$fg[yellow]%}%s%{$reset_color%}"
zstyle ':vcs_info:*' actionformats \
"%{$fg[yellow]%}%c%{$fg[green]%}%u%{$reset_color%}\
[%{$fg[cyan]%}%b|%a%f%{$reset_color%}]%{$fg[yellow]%}%s%{$reset_color%}"
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b:%r'
vcs_info_wrapper() {
    LANG=en_US.UTF-8 vcs_info
    if [ -n "$vcs_info_msg_0_" ]; then
        echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
    fi
}
PROMPT='%n@%m:$(vcs_info_wrapper)%# '
RPROMPT='%~'

## Aliases
# Safety
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -i"
set -o noclobber

# Listing, directories, and motion
alias cl="clear"
alias du="du -ch --max-depth=1"

maybe_ls() {
    if type /usr/local/opt/coreutils/libexec/gnubin/ls > /dev/null 2>&1; then
        /usr/local/opt/coreutils/libexec/gnubin/ls $@
    else
        ls $@
    fi
}
alias l="maybe_ls -CF --color=auto"
alias la="maybe_ls -AF --color=auto"
alias ll="maybe_ls -alrtF --color=auto"
alias ls="maybe_ls -CF --color=auto"

alias m="less"

alias ..="cd .."
alias ...="cd ..;cd .."

alias md="mkdir"

alias treeacl="tree -A -C -L 2"

# find
if type /usr/local/opt/findutils/libexec/gnubin/find > /dev/null 2>&1; then
    alias find="/usr/local/opt/findutils/libexec/gnubin/find"
fi

# grep
if type ggrep > /dev/null 2>&1; then
    alias grep="ggrep --color=auto"
else
    alias grep="grep --color=auto"
fi
export GREP_COLORS="1;31" # green for matches

# sort
if type /usr/local/opt/coreutils/libexec/gnubin/sort > /dev/null 2>&1; then
    alias sort="/usr/local/opt/coreutils/libexec/gnubin/sort"
fi

# tar
if type /usr/local/opt/gnu-tar/libexec/gnubin/tar > /dev/null 2>&1; then
    alias tar="/usr/local/opt/gnu-tar/libexec/gnubin/tar"
fi

# which
if type /usr/local/opt/gnu-which/bin/gwhich > /dev/null 2>&1; then
    alias which="/usr/local/opt/gnu-which/bin/gwhich"
fi

# xargs
if type /usr/local/opt/findutils/libexec/gnubin/xargs -h > /dev/null 2>&1; then
    alias xargs="/usr/local/opt/findutils/libexec/gnubin/xargs"
fi

# Text and editor commands
alias em="emacs -nw"
alias eqq="emacs -nw -Q"
export EDITOR="emacs -nw"
export VISUAL="emacs -nw"

# less options
export LESS="-iMRS --no-init --quit-if-one-screen --RAW-CONTROL-CHARS"
case $OSTYPE in
    darwin* )
        if [ -x /usr/local/bin/src-hilite-lesspipe.sh ]; then
            lessopen="| /usr/local/bin/src-hilite-lesspipe.sh %s"
        fi
        ;;
    linux* )
        if [ -x /usr/share/source-highlight/src-hilite-lesspipe.sh ]; then
            lessopen="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"
        fi
        ;;
esac
export LESSOPEN=$lessopen

# sort options
# Ensures cross-platform sorting behavior of GNU sort.
# http://www.gnu.org/software/coreutils/faq/coreutils-faq.html#Sort-does-not-sort-in-normal-order_0021
unset LANG
export LC_ALL=POSIX

# pyenv
if [ -d $HOME/.pyenv ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH=$PYENV_ROOT/bin:$PATH
fi
if type pyenv > /dev/null 2>&1; then
    eval "$(pyenv init -)"
fi
