## Path
# prevent dupulication in paths
typeset -U path PATH
typeset -U cdpath CDPATH
typeset -U fpath FPATH
typeset -U manpath MANPATH

case ${OSTYPE} in
    darwin* )
        # path helper
        if [ -x /usr/libexec/path_helper ]; then
            eval $(/usr/libexec/path_helper -s)
        fi

        # for coreutils
        if [ -x /usr/local/opt/coreutils/libexec/gnubin ]; then
            path=(/usr/local/opt/coreutils/libexec/gnubin $path)
        fi
        if [ -x /usr/local/opt/coreutils/libexec/gnuman ]; then
            manpath=(/usr/local/opt/coreutils/libexec/gnuman $manpath)
        fi
        ;;

    linux* )
        ;;
esac
