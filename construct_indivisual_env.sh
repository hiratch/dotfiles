#!/bin/sh
set -e

# install_user.sh の先頭に入れる逆のチェック
if [ "$(id -u)" -eq 0 ]; then
    echo "エラー: ユーザー環境のセットアップです。sudoを使わずに実行してください。"
    exit 1
fi

echo "=== ユーザー環境のセットアップを開始します (sudo不要) ==="

# 1. Homebrewのインストール (Mac/Linux共通)
if ! command -v brew >/dev/null 2>&1; then
    echo "Homebrewが見つかりません。インストールを開始します..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    
    # インストール直後にbrewコマンドを使えるようにPATHを通す
    OS="$(uname -s)"
    if [ "$OS" = "Linux" ] && [ -x "/home/linuxbrew/.linuxbrew/bin/brew" ]; then
        eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    elif [ "$OS" = "Darwin" ] && [ -x "/opt/homebrew/bin/brew" ]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
fi

# 2. Brewfileによるユーザー用ツールのインストール
echo "Homebrewによるパッケージのインストールを実行します..."
if [ -f "$HOME/dotfiles/Brewfile" ]; then
    # fzf, ripgrep, bat などの個人用ツールは Brewfile に記載しておきます
    brew bundle --file="$HOME/dotfiles/Brewfile"
else
    echo "警告: $HOME/dotfiles/Brewfile が見つかりません。スキップします。"
fi

# 3. Byobuなどの設定ファイルのシンボリックリンク展開
echo "設定ファイルのシンボリックリンクを作成しています..."
mkdir -p "$HOME/.byobu"

# Byobuのスクロール設定を強制上書き展開
ln -sf "$HOME/dotfiles/.byobu/.tmux.conf" "$HOME/.byobu/.tmux.conf"

# 他の設定ファイルもここで展開
ln -sfn "$HOME/dotfiles/.zshenv" "$HOME/.zshenv"
ln -sfn "$HOME/dotfiles/.zsh" "$HOME/.zsh"
ln -sfn "$HOME/dotfiles/.bashrc" "$HOME/.bashrc"
ln -sfn "$HOME/dotfiles/.vimrc" "$HOME/.vimrc"
ln -sfn "$HOME/dotfiles/.emacs.d" "$HOME/.emacs.d"
ln -sfn "$HOME/dotfiles/.bc" "$HOME/.bc"
ln -sfn "$HOME/dotfiles/.gitconfig" "$HOME/.gitconfig"
ln -sfn "$HOME/dotfiles/.gitignore" "$HOME/.gitignore"
ln -sfn "$HOME/dotfiles/.screenrc" "$HOME/.screenrc"
ln -sfn "$HOME/dotfiles/Brewfile" "$HOME/Brewfile"

# .claudeディレクトリとkeybindings.jsonのシンボリックリンク展開
#mkdir -p "$HOME/.claude"
#ln -sf "$HOME/dotfiles/.claude/keybindings.json" "$HOME/.claude/keybindings.json"

echo "ユーザー環境のセットアップが完了しました！"
