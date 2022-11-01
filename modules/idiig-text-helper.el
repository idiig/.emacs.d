;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; TAB cycle if there are only few candidates
;;   (setq completion-cycle-threshold 3)
;;   (setq tab-always-indent 'complete))

;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-quit-no-match 'separator)  ;; Enable auto completion and configure quitting
;;   (kind-icon-use-icons nil)         ;; Disable icons
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin
;;   :bind (:map corfu-map 
;;               ("TAB" . corfu-next)
;;               ("<tab>" . corfu-next)
;;               ("S-TAB" . corfu-previous)
;;               ("<backtab>" . corfu-previous)
;; 	      ("C-j" . corfu-next)
;; 	      ("C-n" . corfu-next)
;; 	      ("C-p" . corfu-previous)
;; 	      ("C-k" . corfu-previous)
;;               ([remap move-beginning-of-line] . #'corfu-beginning-of-prompt)
;;               ([remap move-end-of-line] . #'corfu-end-of-prompt)) 
;;   :init
;;   (progn
;;     (defun corfu-beginning-of-prompt ()
;;       "Move to beginning of completion input."
;;       (interactive)
;;       (corfu--goto -1)
;;       (goto-char (car completion-in-region--data)))
;;     (defun corfu-end-of-prompt ()
;;       "Move to end of completion input."
;;       (interactive)
;;       (corfu--goto -1)
;;       (goto-char (cadr completion-in-region--data)))
;;     ;; (global-corfu-mode)
;;     ;; (corfu-excluded-modes '(lsp-bridge-mode))
;;     ))

;; ;; Use Dabbrev with Corfu!
;; (use-package dabbrev
;;   ;; Swap M-/ and C-M-/
;;   :bind (("M-/" . dabbrev-completion)
;;          ("C-M-/" . dabbrev-expand))
;;   ;; Other useful Dabbrev configurations.
;;   :custom
;;   (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\|svg\\|eps\\)\\'")))

;; ;; 英语补全
;; (use-package corfu-english-helper
;;   :after corfu
;;   :defer t
;;   :load-path "~/.emacs.d/dependencies/corfu-english-helper/"
;;   :commands toggle-corfu-english-helper)

;; hippie-expand补齐
(use-package hippie-exp
  :init
  (progn
    (global-set-key (kbd "M-/") 'hippie-expand)
    (define-key evil-insert-state-map [remap evil-complete-previous] 'hippie-expand)
    (setq hippie-expand-try-functions-list
          '(
            ;; Try to expand word "dynamically", searching the current buffer.
            try-expand-dabbrev
            ;; Try to expand word "dynamically", searching all other buffers.
            try-expand-dabbrev-all-buffers
            ;; Try to expand word "dynamically", searching the kill ring.
            try-expand-dabbrev-from-kill
            ;; Try to complete text as a file name, as many characters as unique.
            try-complete-file-name-partially
            ;; Try to complete text as a file name.
            try-complete-file-name
            ;; Try to expand word before point according to all abbrev tables.
            try-expand-all-abbrevs
            ;; Try to complete the current line to an entire line in the buffer.
            try-expand-list
            ;; Try to complete the current line to an entire line in the buffer.
            try-expand-line
            ;; Try to complete as an Emacs Lisp symbol, as many characters as
            ;; unique.
            try-complete-lisp-symbol-partially
            ;; Try to complete word as an Emacs Lisp symbol.
            try-complete-lisp-symbol))
    ;; Try to expand yasnippet snippets based on prefix
    (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    ))

(use-package ispell
  :defer t
  :init
  ;; ispell不检查部分
  (defun idiig/org-ispell ()
    "Configure `ispell-skip-region-alist' for `org-mode'."
    (make-local-variable 'ispell-skip-region-alist)
    (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
    (add-to-list 'ispell-skip-region-alist '("~" "~"))
    (add-to-list 'ispell-skip-region-alist '("=" "="))
    (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
  (add-hook 'org-mode-hook #'idiig/org-ispell)
  :config
  (setq ispell-program-name "/opt/homebrew/bin/aspell"))

(use-package lsp-grammarly
  :defer t)

(provide 'idiig-text-helper)
