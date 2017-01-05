## Path
# prevent dupulication
typeset -U path PATH

case $OSTYPE in
    darwin* )
        # path helper
        if [ -x /usr/libexec/path_helper ]; then
            eval $(/usr/libexec/path_helper -s)
        fi
        ;;

    * )
        # cargo
        if [ -d ~/.cargo/bin ]; then
            path=(~/.cargo/bin(N-/) $path)
        fi

        # pyenv
        if [ -d $HOME/.pyenv ]; then
            export PYENV_ROOT="$HOME/.pyenv"
            path=($PYENV_ROOT/bin(N-/) $path)
        fi
        if type pyenv > /dev/null 2>&1; then
            eval "$(pyenv init -)"
        fi
        ;;
esac
