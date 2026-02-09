;;; init.el --- A Modern Emacs Configuration with Quelpa and use-package

;;; ==========================================================================
;;; Section 1: Bootstrap - パッケージ管理システムの準備
;;; ==========================================================================
(setq read-process-output-max (* 1024 1024)) ;; LSPの通信パフォーマンス向上のため重要
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; 基本パッケージのインストール
(dolist (pkg '(quelpa use-package))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'quelpa)
(require 'use-package)

;; quelpa-use-package のセットアップ
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)
(add-to-list 'load-path (expand-file-name "quelpa/packages" user-emacs-directory))

;;; ==========================================================================
;;; Section 2: Core Emacs Settings - Emacsの基本設定
;;; ==========================================================================
(setq-default
 inhibit-startup-message t
 line-number-mode t
 column-number-mode t
 indent-tabs-mode nil
 kill-whole-line t
 visible-bell t
 truncate-lines t
 find-file-visit-truename t
 read-file-name-completion-ignore-case t)

;; GUI周りの設定
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)

;; バックアップファイルの抑止
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

;; フレーム設定
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
(set-face-background 'region "forest green")

;; キーバインド調整
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s") 'shell)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Mac用設定
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'meta))

;; ファイル末尾のマーク設定
(defun set-buffer-end-mark ()
  (let ((overlay (make-overlay (point-max) (point-max))))
    (overlay-put overlay 'before-string #("[EOF]" 0 5 (face highlight)))
    (overlay-put overlay 'insert-behind-hooks
                 '((lambda (overlay after beg end &optional len)
                     (when after
                       (move-overlay overlay (point-max) (point-max))))))))
(add-hook 'find-file-hooks 'set-buffer-end-mark)

;;; ==========================================================================
;;; Section 3: UI & Utility Packages - UI・便利機能
;;; ==========================================================================

(use-package bind-key :quelpa t)

(use-package popwin
  :quelpa t
  :config
  (popwin-mode 1)
  (setq popwin:popup-window-position 'bottom))

(use-package ebuff-menu
  :quelpa t
  :bind (("C-x C-b" . electric-buffer-list))
  :config
  (define-key electric-buffer-menu-mode-map "x" 'Buffer-menu-execute))

(use-package rainbow-delimiters
  :quelpa t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (defun rainbow-delimiters-using-stronger-colors ()
    "Make the rainbow-delimiters faces more saturated."
    (interactive)
    (require 'color)
    (cl-loop
     for index from 1 to rainbow-delimiters-max-face-count
     do
     (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
      (cl-callf color-saturate-name (face-foreground face) 30))))
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))

(use-package cua-base
  :quelpa t
  :config
  (cua-mode 1)
  (setq cua-enable-cua-keys nil)
  ;; 矩形編集時の連番挿入機能の拡張
  (defadvice cua-sequence-rectangle (around my-cua-sequence-rectangle activate)
    (interactive
     (list (if current-prefix-arg
               (prefix-numeric-value current-prefix-arg)
             (string-to-number
              (read-string "Start value: (0) " nil nil "0")))
           (string-to-number
            (read-string "Increment: (1) " nil nil "1"))
           (read-string (concat "Format: (" cua--rectangle-seq-format ") "))))
    (if (= (length format) 0)
        (setq format cua--rectangle-seq-format)
      (setq cua--rectangle-seq-format format))
    (cua--rectangle-operation 'clear nil t 1 nil
       '(lambda (s e l r)
           (kill-region s e)
           (insert (format format first))
           (yank)
           (setq first (+ first incr))))))

(use-package magit
  :quelpa t
  :bind (("C-x g" . magit-status)))

(use-package compat
  :quelpa (compat :fetcher github :repo "emacs-compat/compat"))

(use-package topsy
  :quelpa (topsy :fetcher github :repo "alphapapa/topsy.el")
  :hook ((prog-mode . topsy-mode)
         (magit-section-mode . topsy-mode))
  :config
  (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
  (setq-default header-line-format '(which-func-mode ("" which-func-format))))

;;; ==========================================================================
;;; Section 4: Completion & Snippets - 補完・スニペット (Corfu + Yasnippet)
;;; ==========================================================================

;; スニペット設定
(use-package yasnippet
  :quelpa t
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-snippet-dirs (list "~/.emacs.d/snippets" "~/.emacs.d/mysnippets"))
  (define-key yas-minor-mode-map (kbd "C-c C-s") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c C-n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-c C-v") 'yas-visit-snippet-file)
  ;; TABキーの競合を防ぐ設定（必要に応じて調整）
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

(use-package yasnippet-snippets
  :quelpa t
  :after yasnippet)

;; モダンな補完UI (Corfu) - Companyの代わりに採用
;; Eglotとの相性が非常に良いため、こちらに統一します
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ; 補完候補を循環選択
  (corfu-auto t)                 ; 自動で補完開始
  (corfu-separator ?\s)          ; スペースで絞り込み
  (corfu-quit-at-boundary 'separator)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))        ; ドキュメントのポップアップ表示

;; TABキーの挙動設定 (インデント or 補完)
(setq tab-always-indent 'complete)

;;; ==========================================================================
;;; Section 5: LSP & Lang Settings - 言語設定とEglot
;;; ==========================================================================

;; Tree-sitter (シンタックスハイライト)
(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt) ;; 未インストールの言語を開いた時にインストールするか尋ねる
  :config
  (treesit-auto-add-to-auto-mode-alist 'all) ;; すべての言語でTree-sitterモードを有効にする
  (global-treesit-auto-mode))


(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

;; デフォルトモードのリマップ (標準モード -> Tree-sitter版)
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        ;; 必要に応じて他の言語も追加可能 (Emacs 29以降)
        ;; (c-mode . c-ts-mode) 
        ;; (c++-mode . c++-ts-mode)
        ))

;; LSPクライアント (Eglot)
(use-package eglot
  :ensure nil ; Emacs 29+ 標準搭載
  :hook 
  ;; 以下のモード起動時に自動でLSPサーバーに接続
  ((python-ts-mode . eglot-ensure)
   (python-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (c-ts-mode . eglot-ensure)
   (c++-ts-mode . eglot-ensure)
   (rust-mode . eglot-ensure)      ; Rust用追加
   (rust-ts-mode . eglot-ensure)   ; Rust (Tree-sitter)用追加
   (csharp-mode . eglot-ensure)    ; C#用追加
   (typescript-mode . eglot-ensure)); TS用追加
  :config
  (setq eglot-connect-timeout 120)
  
  ;; 言語サーバーの明示的指定
  ;; Python (pylsp)
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp")))
  ;; C/C++ (clangd)
  (add-to-list 'eglot-server-programs '((c-mode c++-mode c-ts-mode c++-ts-mode) . ("clangd")))
  ;; Rust (rust-analyzer) - 通常は自動検出されますが、念の為指定可能
  ;; (add-to-list 'eglot-server-programs '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  )

;; TRAMP/Docker用のヘルパー関数
(defun my-eglot-dynamic-docker-project-root-connect (server)
  "When in a TRAMP docker buffer, dynamically find the container-local
project root and pass it to eglot."
  (let* ((project-tramp-root (project-root (project-current)))
         (container-project-root (tramp-file-name-localname
                                  (tramp-dissect-file-name project-tramp-root))))
    (message "Eglot: Forcing project root to '%s' inside container" container-project-root)
    (eglot-connect server :project-root container-project-root)))

;; --- 個別の言語モード設定 (LSP以外の部分) ---

(use-package cc-mode
  :ensure nil
  :hook (c-mode-common-hook . (lambda () (setq show-trailing-whitespace t))))

;; Rust設定
(use-package rust-mode
  :quelpa t
  :config
  (setq rust-format-on-save t)) ; 保存時にrustfmtを実行

;; C#設定
(use-package csharp-mode
  :quelpa t
  :mode "\\.cs\\'")

;; TypeScript設定
(use-package typescript-mode
  :quelpa t
  :mode "\\.tsx\\'")

;;; ==========================================================================
;;; Section 6: Debugger & Remote - デバッガ・TRAMP
;;; ==========================================================================

(use-package gud
  :ensure nil
  :init
  (bind-keys :map mode-specific-map
             ("C-c C-SPC" . gud-break))
  (add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
  :config
  (setq g-many-windows t)
  (setq gdb-use-serapate-io-buffer t)
  (setq gud-tooltip-echo-area nil))

(use-package tramp
  :ensure nil
  :config
  (setq tramp-persistency-file-name "~/.emacs.d/tramp"))

;;; ==========================================================================
;;; Section 7: Custom Variables - 自動生成された設定
;;; ==========================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((eglot-project-connect-function . my-eglot-dynamic-docker-project-root-connect)))
 '(package-selected-packages
   '(corfu csharp-mode magit matlab-mode popwin quelpa-use-package rainbow-delimiters topsy tree-sitter-langs typescript-mode yasnippet-snippets)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (file-exists-p "~/.emacs.d/opam-user-setup.el")
  (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))

;;; init.el ends here
