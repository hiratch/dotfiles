# ==========================================
# Mac / Linux 共通のパッケージ (CLI・開発ツール)
# ==========================================
tap "genkiroid/cert"

brew "ast-grep"
brew "bat"
brew "btop"
brew "byobu"
brew "cmake"
brew "cmigemo"
brew "curl"
brew "direnv"
brew "duckdb"
brew "fd"
brew "ffmpeg"
brew "gcc"
brew "gh"
brew "git"
brew "git-delta"
brew "git-lfs"
brew "imagemagick"
brew "libgccjit"
brew "llvm"
brew "lv"
brew "pwgen"
brew "ripgrep"
brew "uv"
brew "vim"
brew "zsh"
brew "zsh-completions"
brew "genkiroid/cert/cert"

brew "awscli"

# ==========================================
# macOS 専用のパッケージ (GUI・Mac特有ツール)
# ==========================================
if OS.mac?
  tap "railwaycat/emacsmacport"
  
  # Mac特有のコマンドラインツール
  brew "iproute2mac" # Linuxは標準のipコマンドがあるため不要
  brew "git-gui"     # GUIを含むためMac限定に配置
  brew "tkdiff"      # GUI(Tcl/Tk)を含むためMac限定に配置
  brew "coreutils"
  brew "gnu-tar"
  
  # GUIアプリケーション (Cask)
  cask "railwaycat/emacsmacport/emacs-mac"
  cask "docker-desktop"
  cask "factor"
  cask "meld"
  cask "notion"
  cask "slack"
  cask "obsidian"
  cask "claude-code"
  cask "iterm2"
  cask "google-chrome"
  cask "google-japanese-ime"
end
