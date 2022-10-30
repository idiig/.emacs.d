;; run-prog-mode-hook
(defun idiig/run-prog-mode-hooks ()
  "Runs `prog-mode-hook'. 针对一些本该为编程语言又没自动加载prog mode的语言hook.
如：(add-hook 'python-hook 'idiig/run-prog-mode-hooks)
"
  (run-hooks 'prog-mode-hook))

;; 加入REPL列表
(defvar idiig-repl-list '()
  "REPL语言列表.")

(defun idiig/register-repl (feature repl-func &optional tag)
  "将对象语言加入REPL列表用于."
  (push `(,(or tag (symbol-name repl-func))
          . (,feature . ,repl-func))
        idiig-repl-list))

;; yasnippet
(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode yas-active-extra-mode)
  :diminish (yas-global-mode yas-minor-mode yas-active-extra-mode)
  :init
  (progn
    ;; 没有undefine error
    (defvar yas-global-mode nil)
    ;; 多键位时给出补全项
    (setq yas-prompt-functions '(yas-completing-promptq))
    ;; minor mode键位重制
    (setq yas-minor-mode-map (make-sparse-keymap))
    ;; yas的一些函数
    (defun idiig/load-yasnippet ()
      (unless yas-global-mode (yas-global-mode 1))
      (yas-minor-mode 1))
    (defun idiig/force-yasnippet-off ()
      (yas-minor-mode -1)
      (setq yas-dont-activate t))
    ;; hooks
    (add-hook 'term-mode-hook 'idiig/force-yasnippet-off)
    (add-hook 'shell-mode-hook 'idiig/force-yasnippet-off)
    (add-hook 'eshell-mode-hook 'idiig/force-yasnippet-off)
    (add-hook 'org-mode-hook 'idiig/load-yasnippet)
    (add-hook 'prog-mode-hook 'idiig/load-yasnippet)
    (add-hook 'markdown-mode-hook 'idiig/load-yasnippet)
    (add-hook 'bibtex-mode-hook 'idiig/load-yasnippet))
  :config
  (progn
    (use-package yasnippet-snippets)))

;; Use lsp-bridge
(use-package lsp-bridge
  :load-path "~/.emacs.d/dependencies/lsp-bridge"
  :bind (:map acm-mode-map
              ("C-j" . acm-select-next)
              ("C-k" . acm-select-prev))
  ;; :custom
  ;; (acm-enable-yas . nil)
  ;; (acm-enable-doc . nil)
  ;; (acm-enable-icon . nil)
  :hook (prog-mode . (lambda ()
                       ;; hook任意语言
                       ;; (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                       (lsp-bridge-mode)))
  :init
  (require 'lsp-bridge)
  :config
  (progn
    (setq acm-enable-tabnine t)
    (setq lsp-bridge-enable-auto-format-code t)
    (setq lsp-bridge-python-command "/opt/homebrew/bin/python3")))

(provide 'idiig-prog)
