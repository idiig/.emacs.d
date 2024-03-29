;; 语言环境
(set-language-environment "UTF-8")
(setenv "LANG"  "ja_JP.UTF-8")
(prefer-coding-system 'utf-8-unix)
(setq-default bidi-display-reordering nil) ;; 无视从右到左的语言

;; 不自动生成保存文件
(setq make-backup-files nil)

;; 选中文字能被整体替换（与其他文本编辑器相同）
(delete-selection-mode 1)

;; 文件最后添加新行
(setq require-final-newline t)

;; 文件在外部更新时buffer更新
(global-auto-revert-mode 1)

;; 窄光标样式
(setq-default cursor-type 'bar)

;; 关闭警告声
(setq ring-bell-function 'ignore)

;; yes no 2 y n
(defalias 'yes-or-no-p 'y-or-n-p)

;; 禁止多个窗口
(setq ns-pop-up-frames nil)

;; 不折行
(setq-default truncate-lines t)
(setq truncate-partial-width-windows t)

;; 用tab进行缩进或补全
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; 添加ag，tex等的路径
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
(setenv "PATH" "~/.pyenv/shims/python")
(use-package exec-path-from-shell
  :defer t
  :custom
  (exec-path-from-shell-arguments (quote ("-l")))
  :init
  (exec-path-from-shell-initialize))

;; 最近打开记录设置
(use-package recentf
  :defer t
  :commands (recentf-open-files consult-recent-file)
  :init
  (setq recentf-save-file (expand-file-name "recentf" idiig-cache-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 25
        ;; recentf-auto-cleanup 'never
        )
  (setq recentf-exclude
        '("COMMIT_MSG"
          "COMMIT_EDITMSG"
          "github.*txt$"
          "/tmp/"
          "/ssh:"
          "/sudo:"
          "/TAGS$"
          "/GTAGS$"
          "/GRAGS$"
          "/GPATH$"
          "\\.mkv$"
          "\\.mp[34]$"
          "\\.avi$"
          "\\.pdf$"
          "\\.sub$"
          "\\.srt$"
          "\\.ass$"
          ".*png$"
          "Nutstore/org-files/"
          "bookmarks"))
  (setq recentf-max-saved-items 2048)
  ;; :config
  (recentf-mode 1))

;; cleanup recent files
(defun idiig/cleanup-recentf ()
  (progn
    (and (fboundp 'recentf-cleanup)
         (recentf-cleanup))))
(add-hook 'kill-emacs-hook #'idiig/cleanup-recentf)

;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
;; 使用鼠标时关闭minibuffer
(defun idiig/stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
(add-hook 'mouse-leave-buffer-hook 'idiig/stop-using-minibuffer)

;; 不存在文档时询问是否新建
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; 找文件时若无母文档则新建 
(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (when dir
        (unless (file-exists-p dir)
          (make-directory dir t))))))

;; 自动保存文件设置
(use-package savehist
  :init
  (progn
    (setq savehist-additional-variables
          ;; search entries
          '(search-ring regexp-search-ring)
          ;; 每一分钟保存一次
          savehist-autosave-interval 60
          ;; keep the home clean
          savehist-file (expand-file-name "savehist" idiig-savefile-dir))
    (savehist-mode t)))

;; 书签功能，打开时自动到原先编辑的位置
(use-package bookmark
  :config
  (setq bookmark-default-file (expand-file-name "bookmarks" idiig-savefile-dir)
        bookmark-save-flag 1))

;; 保存文件的编辑位置
(use-package saveplace
  :config
  (progn
    (setq save-place-file (expand-file-name "place" idiig-savefile-dir))
    (save-place-mode 1)))

;; Message buffer为motion state
(evil-set-initial-state 'messages-buffer-mode 'motion)
(evil-set-initial-state 'special-mode 'motion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; 打开配置文件
;; (defun open-init-file()
;;   (interactive)
;;   (find-file "~/.emacs.d/init.el"))

;; C-w删除词或选中区域
(defun backward-kill-word-or-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))

;; 自动在后面加一行
(defun idiig/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; occur弹出编辑 
(defun occur-dwim ()
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur)
  (call-interactively 'other-window))

;; 寻找non-ascii字符
(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

;; 去匹配括号的末尾
(defun idiig/goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

;; 在点号后面加入SPC 
(defun idiig/insert-space-after-point ()
  (interactive)
  (save-excursion (insert " ")))

;; 大小写转换
(defmacro dakra-define-up/downcase-dwim (case)
  (let ((func (intern (concat "dakra-" case "-dwim")))
        (doc (format "Like `%s-dwim' but %s from beginning when no region is active." case case))
        (case-region (intern (concat case "-region")))
        (case-word (intern (concat case "-word"))))
    `(defun ,func (arg)
       ,doc
       (interactive "*p")
       (save-excursion
         (if (use-region-p)
             (,case-region (region-beginning) (region-end))
           (beginning-of-thing 'symbol)
           (,case-word arg))))))

(dakra-define-up/downcase-dwim "upcase")
(dakra-define-up/downcase-dwim "downcase")
(dakra-define-up/downcase-dwim "capitalize")

;; 全局缩进
(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end)))
      (progn
        (indent-buffer)))))

;; window function
(defun idiig/shrink-window (delta)
  (interactive "p")
  (shrink-window delta))

(defun idiig/shrink-window-horizontally (delta)
  (interactive "p")
  (shrink-window delta t))

(defun idiig/enlarge-window (delta)
  (interactive "p")
  (enlarge-window delta))

(defun idiig/enlarge-window-horizontally (delta)
  (interactive "p")
  (enlarge-window delta t))

(defvar idiig-layouts-restrict-spc-tab nil)

(defun idiig/alternate-buffer (&optional window)
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (if (bound-and-true-p idiig-layouts-restrict-spc-tab)
          (let ((buffer-list (persp-buffer-list))
                (my-buffer (window-buffer window)))
            ;; find buffer of the same persp in window
            (seq-find (lambda (it) ;; predicate
                        (and (not (eq (car it) my-buffer))
                             (member (car it) buffer-list)))
                      (window-prev-buffers)
                      ;; default if found none
                      (list nil nil nil)))
        (or (cl-find (window-buffer window) (window-prev-buffers)
                     :key #'car :test-not #'eq)
            (list (other-buffer) nil nil)))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))

(defun idiig/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)))

;; 重命名当前buffer的文件名
(defun idiig/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; run current file
(defun idiig/run-current-file ()
  "Compile and/or Execute the current file."
  (interactive)
  ;; (call-interactively #'compile-dwim-compile)
  (call-interactively #'compile-dwim-run))

;; 光标跳到新窗口
(use-package popwin
  :defer t
  :custom
  (popup-window-position 'bottom)
  (popup-window-width 100)
  :config
  (popwin-mode 1))

;; 窗口移动
(use-package windmove
  :init
  (windmove-default-keybindings))

;; hungry delete: 连续删除空白
(use-package hungry-delete
  :diminish hungry-delete-mode
  :init (setq hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
  :config
  (progn
    (setq-default hungry-delete-chars-to-skip " \t\f\v\n")  ;; 删除的空白符号 
    (global-hungry-delete-mode t)))

;; 跳到代码之前而非最前
(use-package mwim
  :defer t
  :commands (mwim-beginning-of-code-or-line mwim-end-of-code-or-line))

;; 自动折行
(use-package unfill
  :defer t
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :init
  (global-set-key [remap fill-paragraph] #'unfill-toggle))

(use-package emacs
  :init
  (electric-pair-mode t)
  :config
  (setq electric-pair-preserve-balance nil)
  ;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  ;; mode-specific local-electric pairs
  (defconst idiig/default-electric-pairs electric-pair-pairs)
  (defun idiig/add-local-electric-pairs (pairs)
    "Example usage: 
    (add-hook 'jupyter-org-interaction-mode '(lambda () (set-local-electric-pairs '())))
    "
    (setq-local electric-pair-pairs (append idiig/default-electric-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))
  ;; 禁止 <>
  (add-function :before-until electric-pair-inhibit-predicate
                (lambda (c) (eq c ?<   ;; >
                                )))
  "在括号内也可以高亮括号"
  (define-advice show-paren-function (:around (fn) fix-show-paren-function)
    (cond ((looking-at-p "\\s(") (funcall fn))
	  (t (save-excursion
	       (ignore-errors (backward-up-list))
	       (funcall fn)))))
  (show-paren-mode t))

;; posframe: 弹窗设置
(use-package posframe)

;; 数字键选择窗口
(use-package winum
  :diminish winum-mode
  :custom (winum-auto-setup-mode-line nil)
  :init
  (winum-mode))

;; 窗口变化与选择
(use-package window-numbering
  :config
  (window-numbering-mode 1))

;; hydra: 持续按键
(use-package hydra
  :defer t
  :init
  (defun idiig//hydra-key-doc-function (key key-width doc doc-width)
    (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
            key doc))
  :config
  (setq hydra-key-doc-function 'idiig//hydra-key-doc-function
        hydra-head-format "[%s] "))

;; 撤销树
(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode))

(provide 'idiig-better-default)
