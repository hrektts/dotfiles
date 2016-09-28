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
        ;;
esac
