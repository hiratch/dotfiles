

;; C/C++ 用のカスタマイズ
(defconst my-c-style
  '(
    ;; 基本オフセット量の設定
    (c-basic-offset		. 4)

    ;; tab キーでインデントを実行
    (c-tab-always-indent	. t)

    ;; コメント行のオフセット量の設定
    (c-comment-only-line-offset . 0)

    ;; カッコ前後の自動改行処理の設定
    ;; 基本的に'{'はぶら下げ、'}'は一行取る
    (c-hanging-braces-alist
     . (
	(class-open after)		; クラス宣言の'{'の前後
	(class-close after)		; クラス宣言の'}'の後
	(defun-open after)		; 関数宣言の'{'の前後
	(defun-close before after)	; 関数宣言の'}'の後
	(inline-open after)		; クラス内のインライン関数宣言の'{'の後
	(inline-close before after)	; クラス内のインライン関数宣言の'}'の後
	(brace-list-open after)		; 列挙型、配列宣言の'{'の後
	(brace-list-close before after) ; 列挙型、配列宣言の'}'の前後
	(block-open after)		; ステートメントの'{'の後
;;	  (block-close after)		  ; ステートメントの'}'前後
	(block-close . c-snug-do-while) ;
	(substatement-open after)	; サブステートメント(if 文等)の'{'の後
	(statement-case-open after)	; case 文の'{'の後
	(extern-lang-open before after) ; 他言語へのリンケージ宣言の'{'の前後
	(extern-lang-close before)	; 他言語へのリンケージ宣言の'}'の前
	))

    ;; コロン前後の自動改行処理の設定
    (c-hanging-colons-alist
     . (
	(case-label after)	  ; case ラベルの':'の後
	(label after)		  ; ラベルの':'の後
	(access-label after)	  ; アクセスラベル(public等)の':'の後
	(member-init-intro)	  ; コンストラクタでのメンバー初期化リストの先頭の':'では改行しない
	(inher-intro before)	  ; クラス宣言での継承リストの先頭の':'では改行しない
	))

    ;; 挿入された余計な空白文字のキャンセル条件の設定
    ;; 下記の*を削除する
    (c-cleanup-list
     . (
	brace-else-brace		; else の直前
					; "} * else {"	->  "} else {"
	brace-elseif-brace		; else if の直前
					; "} * else if (.*) {"
					; ->  } "else if (.*) {"
	brace-catch-brace		; } * catch (...) * { のところ
	empty-defun-braces		; 空のクラス・関数定義の'}' の直前
					;；"{ * }"  ->	"{}"
	defun-close-semi		; クラス・関数定義後の';' の直前
					; "} * ;"  ->  "};"
	list-close-comma		; 配列初期化時の'},'の直前
					; "} * ,"  ->  "},"
	scope-operator			; スコープ演算子'::' の間
					; ": * :"  ->  "::"
	))

    ;; オフセット量の設定
    ;; 必要部分のみ抜粋(他の設定に付いては info 参照)
    ;; オフセット量は下記で指定
    ;; +  c-basic-offsetの 1倍, ++ c-basic-offsetの 2倍
    ;; -  c-basic-offsetの-1倍, -- c-basic-offsetの-2倍
    (c-offsets-alist
     . (
;;;	(arglist-intro	      . ++)		  ; 引数リストの開始行
	(arglist-intro	      . +)		  ; 引数リストの開始行
;;;	(arglist-close	      . c-lineup-arglist) ; 引数リストの終了行
	(arglist-close	      . 0)		  ; 引数リストの終了行
	(substatement-open    . 0)		  ; サブステートメントの開始行
	(statement-cont	      . +)		  ; ステートメントの継続行
	(case-label	      . 0)		  ; case 文のラベル行
	(statement-case-intro . 4)		  ; case 文のステートメント行
	(access-label	      . /)		  ; アクセスラベル行
	(label		      . 0)		  ; ラベル行
	(block-open	      . 0)		  ; ブロックの開始行
;;;	(member-init-intro    . ++)		  ; メンバオブジェクトの初期化リスト
	(member-init-intro    . +)		  ; メンバオブジェクトの初期化リスト
        (inextern-lang        . 0)
	))

    ;; インデント時に構文解析情報を表示する
    (c-echo-syntactic-information-p . t)
    )
  "My C Programming Style")

;; hook 用の関数の定義
(defun my-c-mode-common-hook ()
  ;; my-c-stye を登録して有効にする
  (c-add-style "PERSONAL" my-c-style t)

  ;; 次のスタイルがデフォルトで用意されているので選択してもよい
  ;;  (c-set-style "gnu")
  ;;  (c-set-style "cc-mode")
  ;;  (c-set-style "stroustrup")
  ;;  (c-set-style "ellemtel")
  ;; 既存のスタイルを変更する場合は次のようにする
  ;;  (c-set-offset 'member-init-intro '++)

  ;; タブ長の設定
  (setq tab-width 4)

  ;; タブの代わりにスペースを使う
  (setq indent-tabs-mode nil)

  ;; 自動改行(auto-newline)を有効にする
  (c-toggle-auto-state t)

  ;; 連続する空白の一括削除(hungry-delete)を有効にする
  (c-toggle-hungry-state t)

  ;;  ;; セミコロンで自動改行しない
  (setq c-hanging-semi&comma-criteria
	'(c-semi&comma-no-newlines-before-nonblanks
	  c-semi&comma-inside-parenlist
	  c-semi&comma-no-newlines-for-oneline-inliners))

  ;; キーバインドの追加
  ;; ------------------
  ;; C-m	改行＋インデント
  ;; C-c c	コンパイルコマンドの起動
  ;; C-h	空白の一括削除
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map "\C-cc" 'compile)
;;  (define-key c-mode-base-map "\C-c\C-c" 'comment-or-uncomment-region)
  (define-key c-mode-base-map "\C-h" 'c-electric-backspace)
  (define-key c-mode-base-map [(control shift h)] help-map)
  (define-key c-mode-base-map [(shift return)] 'newline)
  (define-key c-mode-base-map [(meta right)] 'dabbrev-expand)
  (define-key c-mode-base-map [(meta left)]  'dabbrev-completion)

  ;; コンパイルコマンドの設定
  (setq compile-command "make -k" )   ; Cygwin の make
  ;; (setq compile-command "nmake /NOLOGO /S") ; VC++ の nmake
  )
;; モードに入るときに呼び出す hook の設定
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; ヘッダファイルもc++モードで開く 
(setq auto-mode-alist (append
		       '(("\\.h$"    . c++-mode))
		       auto-mode-alist))

