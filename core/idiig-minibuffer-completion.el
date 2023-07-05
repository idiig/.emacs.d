;; 一些minibuffer相关的有用设置。。
(use-package emacs
  :init
  (progn
    ;; 为`completing-read-multiple'添加提示，比如[CRM<separator>]
    (defun crm-indicator (args)
      (cons (format "[CRM%s] %s"
                    (replace-regexp-in-string
                     "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                     crm-separator)
                    (car args))
            (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; 不允许鼠标出现在minibuffer的提示中
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; 在emacs 28以后，非当前mode的指令都会被隐藏，vertico的指令也会隐藏
    (setq read-extended-command-predicate
          #'command-completion-default-include-p)

    ;; minibuffer可循环
    (setq enable-recursive-minibuffers t)
    ))

;; Vertico：提供minibuffer补全UI
(use-package vertico
  :after consult
  :bind (:map vertico-map
              ("C-j" . #'vertico-next)
              ("C-k" . #'vertico-previous))
  :init
  (vertico-mode)
  :config
  (progn
    ;; 表示行数
    (setq vertico-count 15)
    ;; 可循环
    (setq vertico-cycle t)))

;; Orderless：提供补全格式选择
(use-package orderless
  :after (consult)
  :config
  (setq search-default-mode nil)
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)
      ))

  (defun +orderless--suffix-regexp ()
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))
  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--suffix-regexp))))
     ;; Ignore single !
     ((equal "!" word) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 1))
        (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
          (cons (cdr x) (substring word 0 -1)))))))
  
  ;; Define orderless style with initialism by default ; add migemo feature for japanese
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp
                                 )))
  
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
                                        (buffer (styles +orderless-with-initialism))
                                        (consult-location (styles +orderless-with-initialism))
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)))

;; Maginalia：增强minubuffer的annotation
(use-package marginalia
  :after vertico
  ;; 只在minibuffer启用快捷键
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (progn
    (marginalia-mode)
    (setq marginalia-align-offset 5)))

;; Embark：minibufferaction 和自适应的context menu
(use-package embark
  :after vertico
  :bind
  (("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
   (:map minibuffer-local-map
         ("C-;" . embark-act)         ;; 对函数进行设置操作 
         ("M-." . embark-dwim)        ;; 实施 
         ("C-c C-e" . embark-export)  ;; occur
         )) 
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (progn
    ;; 关闭modeline
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))
    ;; 设定embark-collect-mode
    (evil-set-initial-state 'embark-collect-mode 'normal)
    ;; help-key
    ;; (embark-help-key "?")
    ))

(use-package consult
  :hook (after-init . (lambda () (require 'consult)))
  :bind (([remap M-x] . execute-extended-command)
         ([remap goto-line] . consult-goto-line)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap find-file] . find-file)
         ([remap open-recent-file] . consult-recent-file)
         ([remap evil-yank] . consult-yank-pop)
         ("C-c y" . consult-yasnippet)
         ("C-c f" . consult-find)
         ("C-s" . consult-line)
         ("C-c i" . consult-imenu)
         ("C-c o" . consult-file-externally)
         ("C-x p f" . consult-ripgrep)
         (:map minibuffer-local-map
               ("C-c h" . consult-history)))
  :init
  (defun idiig/consult-buffer-region-or-symbol ()
    "consult-line当前字符或选中区域."
    (interactive)
    (let ((input (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))
      (consult-line input)))
  (defun idiig/consult-project-region-or-symbol (&optional default-inputp)
    "consult-ripgrep 当前字符或选中区域."
    (interactive)
    (let ((input (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))
      (consult-ripgrep default-inputp input)))
  :config
  (progn
    ;; C-s C-s 检索历史记录
    (defvar my-consult-line-map
      (let ((map (make-sparse-keymap)))
        (define-key map "C-s" #'previous-history-element)
        map))
    (consult-customize consult-line :keymap my-consult-line-map)
    ;; ;; 禁止自动显示consult文件的内容
    (setq consult-preview-key "C-v")
    ;; 应用 Orderless 的正则解析到 consult-grep/ripgrep/find
    (defun consult--orderless-regexp-compiler (input type &rest _config)
      (setq input (orderless-pattern-compiler input))
      (cons
       (mapcar (lambda (r) (consult--convert-regexp r type)) input)
       (lambda (str) (orderless--highlight input str))))
    ;; 表示的buffer种类
    (defcustom consult-buffer-sources
      '(consult--source-hidden-buffer
        consult--source-buffer
        consult--source-file
        consult--source-bookmark
        consult--source-project-buffer
        consult--source-project-file)
      "Sources used by `consult-buffer'. See `consult--multi' for a description of the source values."
      :type '(repeat symbol))
    ;; ？提示检索buffer类型；f<SPC>=file, p<SPC>=project, etc..
    (define-key consult-narrow-map
      (vconcat consult-narrow-key "?") #'consult-narrow-help)))

;; embark-export弹出occur和grep mode的buffer
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode))

;; grep搜索+集体修改
(use-package wgrep
  :after embark-consult
  :bind ((:map grep-mode-map ("i" . idiig/wgrep-change-to-wgrep-mode))
         (:map wgrep-mode-map ("C-c C-c" . wgrep-save-all-buffers)))
  :init
  (progn
    (defun idiig/wgrep-change-to-wgrep-mode ()
      (interactive)
      (wgrep-change-to-wgrep-mode)
      (evil-normal-state)))
  :config
  (progn
    (evil-define-key 'normal wgrep-mode-map ",," 'wgrep-finish-edit)
    (evil-define-key 'normal wgrep-mode-map ",c" 'wgrep-finish-edit)
    (evil-define-key 'normal wgrep-mode-map ",a" 'wgrep-abort-changes)
    (evil-define-key 'normal wgrep-mode-map ",k" 'wgrep-abort-changes)
    (evil-define-key 'normal wgrep-mode-map "q" #'(lambda () (interactive) (call-interactively #'wgrep-exit)
                                                    (call-interactively #'quit-window)))
    (evil-define-key 'normal grep-mode-map "i" 'idiig/wgrep-change-to-wgrep-mode)
    (evil-define-key 'normal grep-mode-map "q" 'quit-window)))

;; occur 调整
(use-package emacs
  :bind (:map occur-mode-map ("i" . idiig/line-change-to-occur-mode))
  :init
  (evil-set-initial-state 'occur-mode 'motion)
  (defun idiig/line-change-to-occur-mode ()
    (interactive)
    (occur-edit-mode)
    (evil-normal-state))
  :config
  (progn
    (evil-define-key 'motion occur-mode-map ",," 'occur-cease-edit)
    (evil-define-key 'normal occur-mode-map "q" 'quit-window)))

(provide 'idiig-minibuffer-completion)
