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

;; prog-mode


;; Use lsp-bridge
(use-package lsp-bridge
  :after corfu
  :load-path "~/.emacs.d/dependencies/lsp-bridge"
  :bind (:map acm-mode-map
              ("C-j" . acm-select-next)
              ("C-k" . acm-select-prev))
  :custom
  (acm-enable-yas . nil)
  (acm-enable-doc . nil)
  ;; (acm-enable-icon . nil)
  :hook (prog-mode . (lambda ()
                       ;; hook任意语言
                       ;; (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                         (lsp-bridge-mode)))
  :init
  (require 'lsp-bridge)
  (run-hooks 'corfu-mode-off-hook)
  (setq acm-enable-tabnine t)
  (setq lsp-bridge-enable-auto-format-code t)
  (setq lsp-bridge-python-command "/opt/homebrew/bin/python3")
  )

;; (use-package flymake
;;   :hook (prog-mode-hook . flymake-mode-off-hook))

(provide 'idiig-prog-bridge)
