;; language
;; pyim
(use-package pyim
  :defer t
  :after (orderless)
  :diminish (pyim-isearch-mode)
  :init
  ;; 拼音检索字符串
  (progn
    (defun zh-orderless-regexp (orig_func component)
      (call-interactively #'pyim-activate)
      (call-interactively #'pyim-deactivate)
      (let ((result (funcall orig_func component)))
        (pyim-cregexp-build result)))
    (advice-add 'orderless-regexp :around #'zh-orderless-regexp))
  :config
  (progn
    (setq default-input-method "pyim")
    (setq pyim-dcache-directory "~/.emacs.d/.cache/pyim/dcache/")
    ;; ;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
    ;; (global-set-key (kbd "M-j") 'pyim-convert-string-at-point)
    ;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
    (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)
    ;; 我使用全拼
    (setq pyim-default-scheme 'quanpin)
    ;; 开启代码搜索中文功能（比如拼音，五笔码等）
    (pyim-isearch-mode 1)
    ;; 设置选词框的绘制方式
    (if (posframe-workable-p)
        (setq pyim-page-tooltip 'posframe)
      (setq pyim-page-tooltip 'popup))
    ;; 显示5个候选词。
    (setq pyim-page-length 5)
    ;; Basedict
    (use-package pyim-basedict
      :config
      (pyim-basedict-enable))
    ))

;; ddskk
(use-package ddskk
  :bind (("C-x j" . skk-mode))
  :defer t
  :init
  (progn
    (setq skk-server-prog "/usr/local/bin/google-ime-skk") ; google-ime-skkの場所
    (setq skk-large-jisyo "~/Library/Application Support/AquaSKK/SKK-JISYO.L")
    (setq skk-server-inhibit-startup-server nil) ; 辞書サーバが起動していなかったときに Emacs からプロセスを立ち上げる
    (setq skk-server-host "localhost")           ; サーバー機能を利用
    (setq skk-server-portnum 55100)              ; ポートはgoogle-ime-skk
    (setq skk-share-private-jisyo t)             ; 複数 skk 辞書を共有

    ;; 候補表示
    (setq skk-show-inline t)   ; 変換候補の表示位置
    (setq skk-show-tooltip t) ; 変換候補の表示位置
    (setq skk-show-candidates-always-pop-to-buffer t) ; 変換候補の表示位置
    (setq skk-henkan-show-candidates-rows 2)          ; 候補表示件数を2列に

    ;; 動作
    (setq skk-egg-like-newline t) ; Enterで改行しない
    (setq skk-delete-implies-kakutei nil) ; ▼モードで一つ前の候補を表示
    (setq skk-use-look t) ; 英語補完
    (setq skk-auto-insert-paren t) ; 閉じカッコを自動的に
    (setq skk-henkan-strict-okuri-precedence t) ; 送り仮名が厳密に正しい候補を優先して表示
    (require 'skk-hint) ; ヒント
    (add-hook 'skk-load-hook ; 自動的に入力モードを切り替え
              (lambda ()
                (require 'context-skk)))

    ;; カタカナを変換候補に入れる
    (setq skk-search-katakana 'jisx0201-kana)))

;; (use-package migemo
;;   :init
;;   (defun jp-orderless-regexp (orig_func component)
;;       (let ((result (funcall orig_func component)))
;;         (migemo-get-pattern result)))
;;   (advice-add 'orderless-regexp :around #'jp-orderless-regexp)
;;   :config
;;   (setq migemo-directory "/opt/homebrew/bin/cmigemo/utf-8/migemo-dict")
;;   (setq migemo-command (executable-find "cmigemo"))
;;   (setq migemo-options '("-q" "--emacs" "--nonewline"))
;;   (setq migemo-coding-system 'utf-8-unix) ;; この指定が極めて重要
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil)
;;   (migemo-init)
;;   )

(provide 'idiig-cjk)
