# Zsh custom completions (migrated from old compctl file)

echo "Loading $ZUSRDIR/custom_completions.zsh"

#-----------------------------------------------------------------
# screen
#-----------------------------------------------------------------
# Completes detached screen sessions
_screens() {
    reply=(`screen -ls | grep 'tached'  | sed -e 's/\t//' | sed -e 's/\t.*//' `)
}
compdef _screens screen

#-----------------------------------------------------------------
# ncftp
#-----------------------------------------------------------------
# Completes bookmarks from ~/.ncftp/bookmarks
_ncftphosts () {
  reply=( ` tail +3 ~/.ncftp/bookmarks | cut -d',' -f1 `)
}
compdef _ncftphosts ncftp

#-----------------------------------------------------------------
# X Font clients
#-----------------------------------------------------------------
# Helper function to get font list
_getfontlist () {
    local grephead
    local greplist
    local a
    read -cA a
    if [ -z "$_fontlist" ] ; then
        OIFS=$IFS; IFS=":"
        xlsfonts $xfdopt | awk \
            'BEGIN{FS="-";OFS="-"}{\
        if (/^-/) { \
          print "-" $2,$3,$4,$5,$6,$7,"*","*","*","*",$12,"*",$14,$15} \
        else { print } }' \
            | uniq | tr '\n' ':' | read -A _fontlist
        IFS=$OIFS
    fi
    if [ -z "$a[-1]" ] ; then
        reply=($_fontlist)
    else
        grephead=$a[-1]
        greplist=( $_fontlist[(r)$grephead*,(R)$grephead*] )
        reply=($greplist)
    fi
}

# Bind the font list completion to various X clients.
# This is a simplified binding. The original was more context-sensitive.
compdef _getfontlist rxvt kterm mule emacs xfd

#-----------------------------------------------------------------
# OS Specific completions
#-----------------------------------------------------------------
case `uname -s` in
    FreeBSD)
        # for pkg_* (FreeBSD)
        compctl -g '*(-/) *.tgz' pkg_add
        compctl -g '/var/db/pkg/*(/:t)' pkg_info pkg_delete
        ;;
    Linux)
        # for rpm (RedHat-based Linux)
        if type rpm &>/dev/null; then
            compctl -s '$(rpm -qa)' \
                -x 's[--]' -s 'oldpackage percent replacefiles replacepkgs noscripts \
                       root excludedocs includedocs test upgrade test clean \
                       short-circuit sign recompile rebuild resign querytags \
                       queryformat version help quiet rcfile force hash' - \
                's[ftp:]' -P '//' -s '$(</u/zsh/ftphosts)' -S '/' - \
                'c[-1,--root]' -g '*(-/)' - \
                'c[-1,--rcfile]' -f - \
                'p[1] s[-b]' -k '(p l c i b a)' - \
                'c[-1,--queryformat] N[-1,{]' \
                    -s '"${${(f)$(rpm --querytags)}#RPMTAG_}"' -S '}' - \
                'W[1,-q*] C[-1,-([^-]*|)f*]' -f - \
                'W[1,-i*], W[1,-q*] C[-1,-([^-]*|)p*]' \
                    -g '*.rpm' + -g '*(-/)' + -f -- rpm
        fi
        ;;
esac
