# $ZDOTDIR/.zshrc
#
echo "Loading $ZDOTDIR/.zshrc"

#Setup ssh-agent
if [ -f ~/.ssh-agent ]; then
    . ~/.ssh-agent
fi
if [ -z "$SSH_AGENT_PID" ] || ! kill -0 $SSH_AGENT_PID; then
    ssh-agent > ~/.ssh-agent
    . ~/.ssh-agent
fi
ssh-add -l >& /dev/null || ssh-add

### shell variables

# zsh が使うシェル変数のうちヒストリ（履歴機能）に関するもの

HISTFILE=$ZDOTDIR/.zsh_history
HISTSIZE=10000
SAVEHIST=20000
if [ $UID = 0 ]; then
  unset HISTFILE
  SAVEHIST=0
fi

# 自分以外のユーザのログイン・ログアウトを表示するようになる

WATCH=notme

# core ファイルのサイズを 0 に抑制する
unlimit
#limit core 0
limit -s

# ファイル作成時のデフォルトのモードを指定する
umask 022

# 端末の設定：Ctrl+H に 1 文字削除、Ctrl+C に割り込み、Ctrl+Z にサスペンド
stty erase '^H'
stty intr '^C'
stty susp '^Z'


### key bindings

# zsh のキーバインドを環境変数 EDITOR に関わらず emacs 風にする

bindkey -e				# EDITOR=vi -> bindkey -v
#bindkey -v                              # EDITOR=vi -> bindkey -v

# ・行全てではなく、カーソル位置から前方だけを削除するように変更
# ・Ctrl+Space によるマーク位置からカーソル位置までを消すように変更
# ・Esc+H で、カーソル前の単語を削除（backward-kill-word より多めに消す）
# ・Esc+. で、コマンドラインの最後の引数を繰り返し挿入する

bindkey '^U' backward-kill-line		# override kill-whole-line
#bindkey '^W' kill-region		# override backward-kill-word
#bindkey '^[h' vi-backward-kill-word	# override run-help
#bindkey '^[.' copy-prev-word		# override insert-last-word


# # esc は使いにくいので強引に ctrl に割り振る。
# bindkey '^B' backward-word
# bindkey '^F' forward-word


## 以下、見通しを良くするために複数ファイルに切り分けて include している ##
## （$ZUSRDIR は .zprofile で指定）                                     ##

### zsh options

# zsh そのものの動作を指定するオプションの設定

if [ -f $ZUSRDIR/zshoptions ]; then
    source $ZUSRDIR/zshoptions
fi


### completions

# 補完の設定を行う compctl の設定ファイルを読み込む

if [ -f $ZUSRDIR/completions ]; then
    source $ZUSRDIR/completions
fi


### aliases

# コマンドに別名をつける alias の設定ファイルを読み込む

if [ -f $ZUSRDIR/aliases ]; then
    source $ZUSRDIR/aliases
fi


### functions

# 複雑な機能を実現する関数 function の設定ファイルを読み込む

if [ -f $ZUSRDIR/functions ]; then
    source $ZUSRDIR/functions
fi


### color ls

# 色つき ls の設定ファイルを読み込む

if [ -f $ZUSRDIR/lscolors ]; then
    source $ZUSRDIR/lscolors
#  alias ll='ls -lAF --color=tty'
fi


# mor_tool
if [ -d $HOME/Tool/mor_tool ]; then
    export PATH=$HOME/Tool/mor_tool:$PATH
fi

### User environment
if [ -d $HOME/.rbenv ]; then
    export PATH=$HOME/.rbenv/bin:$PATH
    eval "$(rbenv init - zsh)"
    #export CC=/usr/bin/gcc
fi

if [ -d $HOME/bin ]; then
    export PATH=$HOME/bin:$PATH
fi

if [ -f $HOME/Qualcomm/Hexagon_SDK/3.3.1/setup_sdk_env.source ]; then
    source $HOME/Qualcomm/Hexagon_SDK/3.3.1/setup_sdk_env.source
    export PATH="$HEXAGON_SDK_ROOT/tools/HEXAGON_Tools/8.1.04/Tools/bin:$PATH"
    export HEXAGON_HALIDE_ROOT=$HOME/Qualcomm/HALIDE_Tools/2.0/Halide
fi

if [ -d /opt/Halide ]; then
    export HALIDE_SDK_ROOT=/opt/Halide
elif [ -d /usr/local/Cellar/halide/2017.10.30 ]; then
    export HALIDE_SDK_ROOT=/usr/local/Cellar/halide/2017.10.30
fi  

if [ -d /opt/silexica ]; then
    export LM_LICENSE_FILE=/opt/silexica/license/silexica_nodelock.lic:$LM_LICENSE_FILE
    source /opt/silexica/exports
fi

net_tools_deprecated_message () {
  echo -n 'net-tools コマンドはもう非推奨ですよ？おじさんなんじゃないですか？ '
}

arp () {
  net_tools_deprecated_message
  echo 'Use `ip n`'
}
ifconfig () {
  net_tools_deprecated_message
  echo 'Use `ip a`, `ip link`, `ip -s link`'
}
iptunnel () {
  net_tools_deprecated_message
  echo 'Use `ip tunnel`'
}
iwconfig () {
  echo -n 'iwconfig コマンドはもう非推奨ですよ？おじさんなんじゃないですか？ '
  echo 'Use `iw`'
}
nameif () {
  net_tools_deprecated_message
  echo 'Use `ip link`, `ifrename`'
}
netstat () {
  net_tools_deprecated_message
  echo 'Use `ss`, `ip route` (for netstat -r), `ip -s link` (for netstat -i), `ip maddr` (for netstat -g)'
}
route () {
  net_tools_deprecated_message
  echo 'Use `ip r`'
}

# ユーザ独自の設定ファイルがあれば読み込む

if [ -f $ZUSRDIR/zshrc.user ]; then
    source $ZUSRDIR/zshrc.user
fi

