# $ZDOTDIR/.zprofile
#
echo "Loading $ZDOTDIR/.zprofile"

### Select OS type

# NFS $B$G%[!<%`$r6&M-$7$F$$$k>l9g$J$I!"(BOS $BKh$K0[$J$k4D6-@_Dj$r;H$&(B

case $OSTYPE {
    sunos*)	export SYSTEM=sun ;;
    solaris*)	export SYSTEM=sol ;;
    irix*)	export SYSTEM=sgi ;;
    osf*)	export SYSTEM=dec ;;
    linux*)	export SYSTEM=gnu ;;
    freebsd*)	export SYSTEM=bsd ;;
    darwin*)	export SYSTEM=mac ;;
}

# ZDOTDIR $B$O(B zsh $B$N8D?MMQ@_Dj%U%!%$%k$rC5$9%G%#%l%/%H%j$r;XDj$9$k(B

if [ -z $ZDOTDIR ]; then
  export ZDOTDIR=$HOME
fi

# $B@Z$jJ,$1$?@_Dj%U%!%$%k$rFI$_9~$`%G%#%l%/%H%j$r;XDj$9$k(B

#export ZUSERDIR=$ZDOTDIR/.zsh


### System specific environment

# $B4D6-JQ?t!J(BPATH $B$J$I!K$N(B OS $BJL@_Dj%U%!%$%k$rFI$_9~$`(B

if [ -f $ZUSERDIR/zshrc.$SYSTEM ]; then
  source $ZUSERDIR/zshrc.$SYSTEM
fi

# OS $BKh$K(B locale $B$r@Z$jBX$($k!J>e5-(B OS $BJL%U%!%$%k$K=q$$$F$bNI$$!K(B

case $SYSTEM {
  sun)	export LANG=japanese    ;;
  sol)	export LANG=japanese    ;;
  sgi)	export LANG=ja_JP.EUC   ;;
  dec)	export LANG=japanese    ;;
  gnu)	export LANG=ja_JP.utf8  ;;
#  bsd)	export LANG=ja_JP.EUC    ;;
  mac)	export LANG=ja_JP.UTF-8 ;;
}

export LANGUAGE=$LANG
export LC_ALL=$LANG

# $B8D?MMQ$N(B PATH $B$rDI2C$9$k(B

# dont include . include $PATH !!!!! dangerous !
#export PATH="$HOME/bin/$SYSTEM:$HOME/bin:$PATH:."


# man path

#export MANPATH="/usr/share/man:/usr/X11R6/man:/usr/local/man:$HOME/man:."


### environment variables

# $B6&DL$9$k4D6-JQ?t$r@_Dj$9$k(B

# export EDITOR=vim
# export LANG=ja_JP.UTF-8
export EDITOR=vim
#export EDITOR='vim'
#export LANG=ja_JP.ujis
export PAGER="less"
#export LESSCHARSET=japanese-sjis
#export LESSCHARSET=japanese
# #export LESS='-irqMM'
# export LESS='-iqMM'
# unset  LESSOPEN
#export LV='-z -Ia -Oe'
#export RUBYLIB=.:/usr/lib/ruby/1.6
#export PAGER=lv
export GZIP='-v9N'
#export XMODIFIERS=@im=kinput2
export XMODIFIERS="@im=skkinput"
#export XMODIFIERS=@im=_XWNMO

# gtkconv $B$r;H$C$F(B GIMP/Gtk+ $B$J$I$N%a%K%e!<$rF|K\8l2=$7$F$$$k>l9g(B

#export GDK_CONV=''

# Applixware $B$r9XF~$7$FF|K\8l4D6-$GMxMQ$7$F$$$k>l9g(B

#export AXLANG=17

