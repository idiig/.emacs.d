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
    (setq enable-recursive-minibuffers t)))

;; Vertico：提供minibuffer补全UI
(use-package vertico
  :after consult
  :bind (:map vertico-map
              ("C-j" . #'vertico-next)
              ("C-k" . #'vertico-previous))
  :init
  (vertico-mode)
  :config
  (setq vertico-count 15))

;; Orderless：提供补全格式选择
(use-package orderless
  :after consult
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Maginalia：增强minubuffer的annotation
(use-package marginalia
  ;; :after consult
  ;; 只在minibuffer启用快捷键
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (progn
    (marginalia-mode)
    (setq marginalia-align-offset 5)))

;; Embark：minibufferaction 和自适应的context menu
(use-package embark
  :after consult
  :bind
  (:map minibuffer-local-map
        ("C-." . embark-act)         ;; pick some comfortable binding
        ("C-;" . embark-dwim)        ;; good alternative: M-.
        ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; 关闭modeline
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package consult
  ;; :defer t
  :hook (after-init . (lambda () (require 'consult)))
  :bind (("C-s" . consult-line)
         ("M-x" . execute-extended-command)
         ("C-c C-l" . consult-goto-line)
         ("C-c i" . consult-imenu)
         ("C-c o" . consult-file-externally)
         ("C-x C-f" . find-file)
         ("C-x C-r" . consult-recent-file)
         ("C-x p f" . consult-ripgrep)
         (:map minibuffer-local-map
               ("C-c h" . consult-history)))
  :init
  (defun idiig/consult-region-or-symbol ()
    "consult当前字符或选中区域."
    (interactive)
    (let ((input (if (region-active-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end))
                   (thing-at-point 'symbol t))))
      (consult-line input)))
  :config
  (progn
    ;; C-s C-s 检索历史记录
    (defvar my-consult-line-map
      (let ((map (make-sparse-keymap)))
        (define-key map "\C-s" #'previous-history-element)
        map))
    (consult-customize consult-line :keymap my-consult-line-map)
    ))

;; (use-package selectrum
;;   :init
;;   (selectrum-mode +1))

(provide 'idiig-minibuffer-completion)
