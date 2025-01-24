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

if [ -f $ZUSRDIR/token ]; then
    source $ZUSRDIR/token
fi

### color ls

# 色つき ls の設定ファイルを読み込む

if [ -f $ZUSRDIR/lscolors ]; then
    source $ZUSRDIR/lscolors
#  alias ll='ls -lAF --color=tty'
fi

# Android NDK
case $SYSTEM {
    mac) export ANDROID_NDK_PATH=$HOME/Library/Android/sdk/ndk/21.0.6113669 ;;
    gnu) export ANDROID_NDK_PATH=$HOME/Android/Sdk/ndk/21.0.6113669 ;;
}

if [ -d $ANDROID_NDK_PATH ]; then
    export PATH=$ANDROID_NDK_PATH:$PATH
fi

case $SYSTEM {
    mac) export ADB_PATH=$HOME/Library/Android/Sdk/platform-tools ;;
#    gnu) export ADB_PATH=$HOME/Android/Sdk/platform-tools ;;
}

if [ -d $ADB_PATH ]; then
    export PATH=$ADB_PATH:$PATH
fi

# mor_tool
if [ -d $HOME/Tool/mor_tool ]; then
    export PATH=$HOME/Tool/mor_tool:$PATH
fi

if [ -d $HOME/Tool/local_tool ]; then
    export PATH=$HOME/Tool/local_tool:$PATH
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

# cargo for Rust
if [ -d $HOME/.cargo ]; then
    source $HOME/.cargo/env
fi

if [ -d /opt/Halide ]; then
    export HALIDE_SDK_ROOT=/opt/Halide
elif [ -d /usr/local/Cellar/halide/2017.10.30 ]; then
    export HALIDE_SDK_ROOT=/usr/local/Cellar/halide/2017.10.30
fi  

case $SYSTEM in
    mac)
        if [ -d /opt/homebrew/opt/llvm ]; then
            export PATH=/opt/homebrew/opt/llvm/bin:$PATH
            export DYLD_LIBRARY_PATH=/opt/homebrew/opt/llvm/lib:$DYLD_LIBRARY_PATH
            export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
            export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
        elif [ -d /usr/local/opt/llvm/ ]; then
            export PATH=/usr/local/opt/llvm/bin:$PATH
            export DYLD_LIBRARY_PATH=/usr/local/opt/llvm/lib:$DYLD_LIBRARY_PATH
            export LDFLAGS="-L/usr/local/opt/llvm/lib"
            export CPPFLAGS="-I/usr/local/opt/llvm/include"
        fi
        ;;
esac


# snap
if [ -d /snap/ ]; then
    export PATH=/snap/bin:$PATH
fi

if [ -n "$LINUX_ON_WINDOWS" ]; then
    export PATH=/mnt/c/Android/android-sdk/platform-tools:/mnt/c/Android/android-sdk/ndk-bundle/build:$PATH
    alias adb=adb.exe
fi

if [ -f $HOME/.bc ]; then
    export BC_ENV_ARGS="-l $HOME/.bc"
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

# pager
type lv > /dev/null 2>&1
if [ $? -eq 0 ]; then
    export PAGER=lv
else
    export PAGER=less
fi
export BAT_PAGER=less

if [ -f $ZUSRDIR/zshrc.user ]; then
    source $ZUSRDIR/zshrc.user
fi

# PYENV
if [ -d $HOME/Tool/pyenv ]; then
    export PYENV_ROOT="$HOME/Tool/pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
elif [ -d $HOME/Tool/.pyenv ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
elif [ -d $HOME/.pyenv ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
fi

type pyenv > /dev/null 2>&1
if [ $? -eq 0 ]; then
    eval "$(pyenv init -)"
    eval "$(pyenv init --path)"
    eval "$(pyenv virtualenv-init -)"
fi



type pipenv > /dev/null 2>&1
if [ $? -eq 0 ]; then
    eval "$(_PIPENV_COMPLETE=zsh_source pipenv)"
#    eval "$(pipenv --completion)"
fi

type pip > /dev/null 2>&1
if [ $? -eq 0 ]; then
    eval "`pip completion --zsh`"
fi

# poetry
if [ -d $HOME/.poetry ]; then
    export PATH=$HOME/.poetry/bin:$PATH
fi
export SYSTEM_VERSION_COMPAT=1

export SCREENDIR=$HOME/.screen

# opam configuration
test -r $HOME/.opam/opam-init/init.zsh && . $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
type opam > /dev/null 2>&1
if [ $? -eq 0 ]; then
    eval `opam config env`
fi

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# iterm2 shell integration
[ -s "~/.iterm2_shell_integration.zsh" ] && source "~/.iterm2_shell_integration.zsh"
