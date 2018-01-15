# $ZDOTDIR/.zprofile
#
echo "Loading $ZDOTDIR/.zprofile"

### Select OS type

# NFS でホームを共有している場合など、OS 毎に異なる環境設定を使う

case $OSTYPE {
    sunos*)	export SYSTEM=sun ;;
    solaris*)	export SYSTEM=sol ;;
    irix*)	export SYSTEM=sgi ;;
    osf*)	export SYSTEM=dec ;;
    linux*)	export SYSTEM=gnu ;;
    freebsd*)	export SYSTEM=bsd ;;
    darwin*)	export SYSTEM=mac ;;
}

# ZDOTDIR は zsh の個人用設定ファイルを探すディレクトリを指定する

if [ -z $ZDOTDIR ]; then
  export ZDOTDIR=$HOME
fi

# 切り分けた設定ファイルを読み込むディレクトリを指定する

#export ZUSERDIR=$ZDOTDIR/.zsh


### System specific environment

# 環境変数（PATH など）の OS 別設定ファイルを読み込む

if [ -f $ZUSERDIR/zshrc.$SYSTEM ]; then
  source $ZUSERDIR/zshrc.$SYSTEM
fi

# OS 毎に locale を切り替える（上記 OS 別ファイルに書いても良い）

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

# 個人用の PATH を追加する

# dont include . include $PATH !!!!! dangerous !
#export PATH="$HOME/bin/$SYSTEM:$HOME/bin:$PATH:."


# man path

#export MANPATH="/usr/share/man:/usr/X11R6/man:/usr/local/man:$HOME/man:."


### environment variables

# 共通する環境変数を設定する

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

# gtkconv を使って GIMP/Gtk+ などのメニューを日本語化している場合

#export GDK_CONV=''

# Applixware を購入して日本語環境で利用している場合

#export AXLANG=17

