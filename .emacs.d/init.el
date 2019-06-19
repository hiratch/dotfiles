(package-initialize)
(setq package-archives
      '(("gnu". "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; Quelpa
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq quelpa-update-melpa-p nil)
(require 'quelpa)
(if (require 'quelpa nil t)
    (quelpa-self-upgrade)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; quelpa install package list start
(quelpa 'rainbow-delimiters)
(quelpa 'popwin)
(quelpa 'magit)
(quelpa 'yasnippet)
(quelpa 'yasnippet-snippets)
(quelpa 'company)
(quelpa 'company-irony)
(quelpa 'company-rtags)
(quelpa 'irony)
(quelpa 'flycheck)
(quelpa 'flycheck-rtags)
(quelpa 'rtags)
;; quelpa install package list end

;;; font-lockの設定
(global-font-lock-mode nil)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines t)

;; 初期フレームの設定
(setq default-frame-alist
      (append (list '(foreground-color . "gray85")
		    '(background-color . "gray2") ;"LemonChiffon")
		    '(border-color . "black")
		    '(mouse-color . "white")
		    '(vertical-scroll-bars . right)
		    '(width . 95)
		    '(height . 55)
		    '(top . 30)
		    '(left . 590)
		    '(cursor-type . box)
		    '(cursor-color . "red")
		    '(cursor-height . 4)
		    '(alpha . (90 70 50 30))
		    )
	      default-frame-alist))


;;; 行番号・桁番号をモードラインに表示する・しない設定 
(line-number-mode t)			; 行番号 
(column-number-mode t)			; 桁番号 

;; バッファの最初の行で previous-line しても、
;; "beginning-of-buffer" と注意されないようにする。
(defun previous-line (arg)
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
	  (line-move (- arg))
	((beginning-of-buffer end-of-buffer)))
    (line-move (- arg)))
  nil)

;;; C-k(kill-line) で行末の改行も含めて kill する
(setq kill-whole-line t)

;;; 警告音のかわりに画面フラッシュ
(setq visible-bell t)

(setq max-specpdl-size 60000)
(setq max-lisp-eval-depth 60000)

;; 行番号入れるやつ
(line-number-mode t)
(column-number-mode t)

;; ツールバー消す
(tool-bar-mode 0)

;; maximize-window
(defun my-fullscreen ()
  (interactive)
  (let ((fullscreen (frame-parameter (selected-frame) 'fullscreen)))
    (cond
     ((null fullscreen)
      (set-frame-parameter (selected-frame) 'fullscreen 'fullboth))
     (t
      (set-frame-parameter (selected-frame) 'fullscreen 'nil))))
  (redisplay))

(setq-default indent-tabs-mode nil)

;; region color
(set-face-background 'region "forest green")

(defun set-buffer-end-mark()
  (let ((overlay (make-overlay (point-max) (point-max))))
    (overlay-put overlay 'before-string #("[EOF]" 0 5 (face highlight)))
    (overlay-put overlay 'insert-behind-hooks
		 '((lambda (ovelay after beg end &optional len)
		     (when after
		       (move-overlay overlay (point-max) (point-max))))))))
(add-hook 'find-file-hooks 'set-buffer-end-mark)

;; shift + 矢印
(windmove-default-keybindings)
(setq windmove-wrap-around t)


(setq minibuffer-max-depth nil)

(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; ;;; 初期ディレクトリの設定
(cd "~/")

;; スクロールバー出す
;(set-scroll-bar-mode 'right)

;; 対応する括弧をハイライト
(show-paren-mode t)

;; 行の表示を折り返さない
(setq-default truncate-lines t)
;; 折り返し表示ON/OFF
(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します．"
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(global-set-key "\C-c\C-l" 'toggle-truncate-lines)

;; C-x C-f で大小文字を区別しない
(setq read-file-name-completion-ignore-case t)

(auto-fill-mode nil)
(auto-fill-mode t)
(set-fill-column 120)

;;; バインド変更など
;;(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)
(setq visible-bell t)
(global-set-key "\M-?" 'help-for-help)
(global-set-key "\C-ci" 'info)

(global-set-key "\C-x\C-b" 'electric-buffer-list)
(eval-after-load "ebuff-menu"
  '(progn
     (define-key
       electric-buffer-menu-mode-map
       "x" 'Buffer-menu-execute)))
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key [(control shift l)] '(lambda () (interactive)(recenter 0)))
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-s" 'shell)
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key
 [C-mouse-wheel1]
 '(lambda (event) (interactive "e")
    (let* ((position (event-start event))
	   (lines (w32-get-mouse-wheel-scroll-lines (nth 4 position))))
      (if (> lines 0) (scroll-up) (scroll-down)))))
(global-set-key
 [S-mouse-wheel1]
 '(lambda (event) (interactive "e")
    (let* ((position (event-start event))
	   (lines (w32-get-mouse-wheel-scroll-lines (nth 4 position))))
      (if (> lines 0) (scroll-up 1) (scroll-down 1)))))


(defun set-background-dark-color ()
  "Change foregrand-color and background-color."
  (interactive)
  ;;  (assoc 'background-color default-frame-alist)
  (set-background-color "gray15")
  (set-foreground-color "gray85")
  (add-hook 'mw32-ime-on-hook
	    (lambda () (set-cursor-color "lawn green")))
  (add-hook 'mw32-ime-off-hook
	    (lambda () (set-cursor-color "LemonChiffon")))
  )

(defun set-background-bright-color ()
  "Change foregrand-color and background-color."
  (interactive)
  (set-background-color "azure")
  (set-foreground-color "black")
  (add-hook 'mw32-ime-on-hook
	    (lambda () (set-cursor-color "orange")))
  (add-hook 'mw32-ime-off-hook
	    (lambda () (set-cursor-color "RoyalBlue1")))
  )

;; shell
(add-hook 'shell-mode-hook
	  (local-set-key "\C-l" '(lambda () (interactive)(recenter 0))))


;; popwin.el
(use-package popwin
  :config
  ;; おまじない（よく分かってない、、）
  (setq display-buffer-function 'popwin:display-buffer)
  ;; ポップアップを画面下に表示
  (setq popwin:popup-window-position 'bottom))
             
;;;;;;;;;;;;;;;;;;;;;;;;;

;; c#
(autoload 'csharp-mode "csharp-mode"
  "Major mode for editing C# code." t)
(setq auto-mode-alist (cons '( "\\.cs\\'" . csharp-mode ) auto-mode-alist ))

;; cperl
(setq auto-mode-alist (cons '( "\\.pl\\'" . cperl-mode ) auto-mode-alist ))
(setq auto-mode-alist (cons '( "\\.cgi\\'" . cperl-mode ) auto-mode-alist ))

(load "~/.emacs.d/cc-mode-set.el")
(setq-default indent-tabs-mode nil)

;; which-func-mode
(which-func-mode 1)
(setq which-func-mode t)
;; 画面上部に表示
(delete (assoc 'which-func-mode mode-line-format) mode-line-format)
(setq-default header-line-format '(which-func-mode ("" which-func-format)))


;; GDB
(use-package gud
  :init
  (bind-keys :map mode-specific-map
             ("\C-c\C-SPC" . gud-break))
  (add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
  :config
  (setq gdb-many-windows t)
  (setq gdb-use-serapate-io-buffer t)
  (setq gud-tooltip-echo-area nil))

;; cua-mode (Common User Acess Mode) 矩形選択用
(use-package cua-base
  :init (cua-mode 1)
  :config
  (progn
    (setq cua-enable-cua-keys nil)))


(defadvice cua-sequence-rectangle (around my-cua-sequence-rectangle activate)
  "連番を挿入するとき、紫のところの文字を上書きしないで左にずらす"
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
         (setq first (+ first incr)))))

;; rainbow-delimiters を使うための設定
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors))

;; 括弧の色を強調する設定
(use-package cl-lib)
(use-package color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))


(use-package magit)

;; c-mode
(add-hook 'c-mode-common-hook
          (lambda()
            (setq show-trailing-whitespace t)))


(use-package yasnippet
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "C-c C-s") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c C-n") 'yas-new-snippet)
  (define-key yas-minor-mode-map (kbd "C-c C-v") 'yas-visit-snippet-file)
  (define-key yas-minor-mode-map (kbd "C-c C-x") 'yas-expand)
  ;; 何故かTABの無効化は両方とも必要・・・何故なのか
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map [(tab)] nil))
 
;rtags
(use-package rtags
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (rtags-is-indexed)
                (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
                (local-set-key (kbd "M-;") 'rtags-find-symbol)
                (local-set-key (kbd "M-@") 'rtags-find-references)
                (local-set-key (kbd "M-,") 'rtags-location-stack-back)))))
 
(use-package flycheck
  :ensure t
  :init(global-flycheck-mode)
  :commands
  (flycheck-irony-setup)
  
  :config
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-syntax-check-automatically nil)
  (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
  (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
  (custom-set-variables
   '(flycheck-display-errors-function
     (lambda (errors)
       (let ((messages (mapcar #'flycheck-error-message errors)))
         (popup-tip (mapconcat 'identity messages "\n")))))
   '(flycheck-display-errors-delay 0.5))
  (define-key flycheck-mode-map (kbd "C-M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "C-M-p") 'flycheck-previous-error)
  (add-hook 'c-mode-common-hook 'flycheck-mode))

(use-package flycheck-rtags
  :ensure t
  :after flycheck rtags
  :config
  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil)
    )
  (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
  (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
  (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)
  )

(use-package company
  :config
  (global-company-mode 1)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection))
 
(defun my-irony-mode-on ()
  (when (member major-mode irony-supported-major-modes)
    (irony-mode 1)))
 
(use-package irony
  :config
  (unless (irony--find-server-executable) (call-interactively #'irony-install-server))
  (custom-set-variables '(irony-additional-clang-options '("-std=c++11")))
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'c-mode-common-hook 'my-irony-mode-on)
  (add-hook 'c-mode-hook 'my-irony-mode-on)
  (add-hook 'c++-mode-hook 'my-irony-mode-on))
 
 
;; yasnippetとの連携
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


; coding
(use-package yasnippet
             :commands
             (yas-insert-snippet yas-new-snippet yas-visit-snippet-file yas-expand)
             :config
             (setq yas-snippet-dirs
                   '("~/.emacs.d/mysnippets"
                     "~/.emacs.d/elpa/yasnippet-snippets-20190316.1919/snippets"
                     )))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit transient git-commit with-editor dash async popwin rainbow-delimiters quelpa-use-package use-package bind-key quelpa))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
