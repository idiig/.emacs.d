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

;; 自动括号补齐等括号，括号高亮设定
(use-package tree-sitter
  ;; :load-path "/path-to/emacs-tree-sitter/core"
  ;; :hook (prog-mode . tree-sitter-mode)
  :init
  (use-package tree-sitter-langs
    :load-path "~/.emacs.d/dependencies/tree-sitter-langs")
  (tree-sitter-load 'elisp "elisp")
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  :config
  (global-tree-sitter-mode)
  ;; 开启语法高亮
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  ;; 文本描述（doc）高亮设定
  (add-function :before-until (local 'tree-sitter-hl-face-mapping-function)
                (lambda (capture-name)
	          (pcase capture-name
	            ("doc" 'font-lock-comment-face)))))

(use-package grammatical-edit
  :load-path "~/.emacs.d/dependencies/grammatical-edit"
  :hook (prog-mode . (lambda () (grammatical-edit-mode 1)))
  :config
  (define-key grammatical-edit-mode-map (kbd "(") 'grammatical-edit-open-round)
  (define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
  (define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
  (define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
  (define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
  (define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
  (define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)
  
  (define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
  (define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)
  (define-key grammatical-edit-mode-map (kbd "'") 'grammatical-edit-single-quote)

  (define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
  (define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)

  (define-key grammatical-edit-mode-map (kbd "M-o") 'grammatical-edit-backward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-forward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

  (define-key grammatical-edit-mode-map (kbd "M-\"") 'grammatical-edit-wrap-double-quote)
  (define-key grammatical-edit-mode-map (kbd "M-'") 'grammatical-edit-wrap-single-quote)
  (define-key grammatical-edit-mode-map (kbd "M-[") 'grammatical-edit-wrap-bracket)
  (define-key grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)
  (define-key grammatical-edit-mode-map (kbd "M-(") 'grammatical-edit-wrap-round)
  (define-key grammatical-edit-mode-map (kbd "M-)") 'grammatical-edit-unwrap)

  (define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-right)
  (define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-left)
  (define-key grammatical-edit-mode-map (kbd "M-:") 'grammatical-edit-jump-out-pair-and-newline)
  
  ;; (define-key grammatical-edit-mode-map (kbd "C-j") 'grammatical-edit-jump-up)
  )

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
  :config
  (progn
    (setq acm-enable-tabnine t)
    ;; (setq lsp-bridge-enable-log t)
    ;; (setq lsp-bridge-enable-auto-format-code t)
    (setq lsp-bridge-python-command "/Users/idiig/.pyenv/shims/python")
    ;; (setq lsp-bridge-python-command "/opt/homebrew/opt/python@3.10/bin/python3.10")
    ;; 增加org-babel语言
    (setq lsp-bridge-org-babel-lang-list '("clojure" 
                                           "latex" 
                                           "python"
                                           "R"
                                           "hmtl"
                                           "sh"
                                           "emacs-lisp"
                                           "org"
                                           "css"))))

(provide 'idiig-prog)
