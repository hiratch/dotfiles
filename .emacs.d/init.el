(package-initialize)
(setq package-archives
      '(("gnu". "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (package-utils quelpa))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;; スクロールバー出す
(set-scroll-bar-mode 'right)

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

