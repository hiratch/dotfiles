;;; init.el --- A Modern Emacs Configuration with Quelpa and use-package

;;; ==========================================================================
;;; Section 1: Bootstrap - パッケージ管理システムの準備
;;; ==========================================================================
;; Emacs 27.1以降のLisp readerのバグを回避
(setq read-process-output-max (* 1024 1024))

;; パッケージ取得先リポジトリを設定
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))

;; package.elを初期化
(package-initialize)

;; Quelpaのインストール (なければ)
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(require 'quelpa)

;; use-packageのインストール (なければ)
;; quelpa-use-packageもここで導入する
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; quelpaをuse-packageで使えるようにする
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)


;;; ==========================================================================
;;; Section 2: Core Emacs Settings - Emacsの基本設定
;;; ==========================================================================

;; UI設定 --------------------------------------------------------------------
(setq-default
 inhibit-startup-message t      ; スタートアップメッセージを非表示
 line-number-mode t             ; 行番号をモードラインに表示
 column-number-mode t           ; 桁番号をモードラインに表示
 indent-tabs-mode nil           ; タブの代わりにスペースを使用
 kill-whole-line t              ; C-kで行全体をキル
 visible-bell t                 ; ビープ音の代わりに画面をフラッシュ
 truncate-lines t               ; 行を折り返さない
 find-file-visit-truename t     ; ファイルアクセス時にシンボリックリンクを解決
 read-file-name-completion-ignore-case t) ; ファイル名補完で大文字小文字を無視

;; ツールバー、メニューバー、スクロールバーを非表示
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; 対応する括弧をハイライト
(show-paren-mode t)

;; バックアップファイルを作成しない
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; 初期フレーム（ウィンドウ）の設定
(setq default-frame-alist
      '((foreground-color . "gray85")
        (background-color . "gray2")
        (border-color . "black")
        (mouse-color . "white")
        (vertical-scroll-bars . right)
        (width . 120)
        (height . 55)
        (top . 15)
        (left . 290)
        (cursor-type . box)
        (cursor-color . "red")
        (alpha . (90 70))))

;; リージョン（選択範囲）の色
(set-face-background 'region "forest green")

;; キーバインド --------------------------------------------------------------
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s") 'shell)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; windmove (Shift + 矢印でウィンドウ間を移動)
(windmove-default-keybindings)
(setq windmove-wrap-around t)


;;; ==========================================================================
;;; Section 3: Package Configurations - 各パッケージの設定
;;; ==========================================================================

;; 汎用ユーティリティ ----------------------------------------------------
(use-package popwin
  :quelpa t
  :config
  (popwin-mode 1)
  (setq popwin:popup-window-position 'bottom))

(use-package rainbow-delimiters
  :quelpa t
  :hook (prog-mode . rainbow-delimiters-mode))

;; CUA-mode (矩形選択)
(use-package cua-base
  :quelpa t
  :config
  (cua-mode 1)
  (setq cua-enable-cua-keys nil)) ; C-x, C-cなどを上書きしない

;; Magit (Gitクライアント)
(use-package magit
  :quelpa t
  :bind (("C-x g" . magit-status)))

(use-package compat
  :quelpa (compat :fetcher github :repo "emacs-compat/compat"))

;; コード補完 & スニペット -----------------------------------------------
(use-package yasnippet
  :quelpa t
  :hook (after-init . yas-global-mode)
  :config
  ;; スニペットディレクトリの指定
  (setq yas-snippet-dirs (list "~/.emacs.d/snippets" "~/.emacs.d/mysnippets"))
  ;; 以前のinit.elにあったキーバインドを復元
  (define-key yas-minor-mode-map (kbd "C-c C-s") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c C-n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-c C-v") 'yas-visit-snippet-file)
  ;; companyとの連携のため、TABキーのバインドはここでは無効化しておく
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

(use-package yasnippet-snippets
  :quelpa t
  :after yasnippet)

(use-package company
  :quelpa t
  :after yasnippet ; yasnippetの読み込み後に設定する
  :hook (after-init . global-company-mode)
  :bind (("C-M-i" . company-complete))
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  ;; companyの設定ブロックに移動
  (add-to-list 'company-backends 'company-yasnippet))

;; 静的解析 (Flycheck) ----------------------------------------------------
(use-package flycheck
  :quelpa t
  :hook (after-init . global-flycheck-mode))


;; C / C++ 開発環境 -------------------------------------------------------
(use-package irony
  :quelpa t
  :hook ((c-mode . irony-mode)
         (c++-mode . irony-mode))
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :quelpa t
  :after (company irony)
  :config (add-to-list 'company-backends 'company-irony))

(use-package rtags
  :quelpa t
  :hook (c-mode-common-hook .
          (lambda ()
            (when (rtags-is-indexed)
              (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
              (local-set-key (kbd "M-,") 'rtags-location-stack-back)))))

(use-package flycheck-rtags
  :quelpa t
  :after (flycheck rtags)
  :hook ((c-mode . flycheck-rtags-setup)
         (c++-mode . flycheck-rtags-setup)))


;; Rust 開発環境 ----------------------------------------------------------
(use-package rust-mode
  :quelpa t
  :config
  (setq rust-format-on-save t))

(use-package racer
  :quelpa t
  :hook (rust-mode . racer-mode)
  (rust-mode . eldoc-mode)
  (rust-mode . (lambda () (company-mode))))

(use-package flycheck-rust
  :quelpa t
  :hook (rust-mode . flycheck-rust-setup))


;; その他モード設定 -------------------------------------------------------
(use-package csharp-mode
  :quelpa t
  :mode "\\.cs\\'")

(use-package typescript-mode
  :quelpa t
  :mode "\\.tsx\\'")

;;; ==========================================================================
;;; Section 4: Custom Set Variables - カスタマイズUIによる設定
;;; ==========================================================================
;; このセクションはcustomize UIによって自動的に編集されます。
;; 手動で編集せず、設定は各use-packageブロックに移動していくのが理想です。
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-display-errors-function
   (lambda (errors)
     (let ((messages (mapcar #'flycheck-error-message errors)))
       (popup-tip (mapconcat 'identity messages "\12")))))
 '(irony-additional-clang-options '("-std=c++11"))
 '(package-selected-packages nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; OPAM (OCaml Package Manager) の設定はそのまま残します
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(when (file-exists-p "~/.emacs.d/opam-user-setup.el")
  (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
