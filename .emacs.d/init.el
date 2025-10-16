;;; init.el --- A Modern Emacs Configuration with Quelpa and use-package

;;; ==========================================================================
;;; Section 1: Bootstrap - パッケージ管理システムの準備
;;; ==========================================================================
(setq read-process-output-max (* 1024 1024))

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))
(require 'quelpa)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

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
 inhibit-startup-message t
 line-number-mode t
 column-number-mode t
 indent-tabs-mode nil
 kill-whole-line t
 visible-bell t
 truncate-lines t
 find-file-visit-truename t
 read-file-name-completion-ignore-case t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)

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

;; キーバインド --------------------------------------------------------------
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-s") 'shell)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; 復元機能: [EOF]マークをバッファ末尾に表示 --------------------------------
(defun set-buffer-end-mark ()
  (let ((overlay (make-overlay (point-max) (point-max))))
    (overlay-put overlay 'before-string #("[EOF]" 0 5 (face highlight)))
    (overlay-put overlay 'insert-behind-hooks
                 '((lambda (overlay after beg end &optional len)
                     (when after
                       (move-overlay overlay (point-max) (point-max))))))))
(add-hook 'find-file-hooks 'set-buffer-end-mark)


;;; ==========================================================================
;;; Section 3: Package Configurations - 各パッケージの設定
;;; ==========================================================================

;; 汎用ユーティリティ ----------------------------------------------------
(use-package bind-key ;; GDB設定で利用
  :quelpa t)

(use-package popwin
  :quelpa t
  :config
  (popwin-mode 1)
  (setq popwin:popup-window-position 'bottom))

;; 復元機能: rainbow-delimitersの色を強調
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
  ;; 起動時に一度だけ実行
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))

;; 復元機能: cua-modeの連番挿入機能を拡張
(use-package cua-base
  :quelpa t
  :config
  (cua-mode 1)
  (setq cua-enable-cua-keys nil) ; C-x, C-cなどを上書きしない
  ;; 連番挿入の拡張
  (defadvice cua-sequence-rectangle (around my-cua-sequence-rectangle activate)
    "連番を挿入するとき、紫のところの文字を上書きしないで左にずらす."
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

;; Magit (Gitクライアント)
(use-package magit
  :quelpa t
  :bind (("C-x g" . magit-status)))

(use-package compat
  :quelpa (compat :fetcher github :repo "emacs-compat/compat"))

;; 復元機能: topsy (which-functionの代替)
(use-package topsy
  :quelpa (topsy :fetcher github :repo "alphapapa/topsy.el")
  :hook ((prog-mode . topsy-mode)
         (magit-section-mode . topsy-mode))
  :config
  ;; topsyをヘッダーラインに表示するための設定
  (delete (assoc 'which-func-mode mode-line-format) mode-line-format)
  (setq-default header-line-format '(which-func-mode ("" which-func-format))))


;; コード補完 & スニペット -----------------------------------------------
(use-package yasnippet
  :quelpa t
  :hook (after-init . yas-global-mode)
  :config
  (setq yas-snippet-dirs (list "~/.emacs.d/snippets" "~/.emacs.d/mysnippets"))
  (define-key yas-minor-mode-map (kbd "C-c C-s") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c C-n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-c C-v") 'yas-visit-snippet-file)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<tab>") nil))

(use-package company
  :quelpa t
  :after yasnippet
  :hook (after-init . global-company-mode)
  :bind (("C-M-i" . company-complete))
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (add-to-list 'company-backends 'company-yasnippet))

(use-package yasnippet-snippets
  :quelpa t
  :after yasnippet)


;; 静的解析 (Flycheck) ----------------------------------------------------
(use-package flycheck
  :quelpa t
  :hook (after-init . global-flycheck-mode))


;; 言語別設定 -------------------------------------------------------------

;; 復元機能: C言語モードで末尾の空白を表示
(use-package cc-mode
  :ensure nil ; ビルトインパッケージ
  :hook (c-mode-common-hook . (lambda () (setq show-trailing-whitespace t))))

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

(use-package csharp-mode
  :quelpa t
  :mode "\\.cs\\'")

(use-package typescript-mode
  :quelpa t
  :mode "\\.tsx\\'")

;; 復元機能: GDB関連の詳細な設定
(use-package gud
  :ensure nil ; ビルトインパッケージ
  :init
  (bind-keys :map mode-specific-map
             ("C-c C-SPC" . gud-break))
  (add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
  :config
  (setq gdb-many-windows t)
  (setq gdb-use-serapate-io-buffer t)
  (setq gud-tooltip-echo-area nil))


;;; ==========================================================================
;;; Section 4: Custom Set Variables - カスタマイズUIによる設定
;;; ==========================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-display-errors-function
   (lambda
     (errors)
     (let
         ((messages
           (mapcar #'flycheck-error-message errors)))
       (popup-tip
        (mapconcat 'identity messages "\12")))))
 '(irony-additional-clang-options '("-std=c++11")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; OPAM (OCaml Package Manager) の設定はそのまま残します
(when (file-exists-p "~/.emacs.d/opam-user-setup.el")
  (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))
