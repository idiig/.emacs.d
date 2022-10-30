;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match 'separator)  ;; Enable auto completion and configure quitting
  (kind-icon-use-icons nil)         ;; Disable icons
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :bind (:map corfu-map 
              ("TAB" . corfu-next)
              ("<tab>" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("<backtab>" . corfu-previous)
	      ("C-j" . corfu-next)
	      ("C-n" . corfu-next)
	      ("C-p" . corfu-previous)
	      ("C-k" . corfu-previous)
              ([remap move-beginning-of-line] . #'corfu-beginning-of-prompt)
              ([remap move-end-of-line] . #'corfu-end-of-prompt)) 
  :init
  (progn
    (defun corfu-beginning-of-prompt ()
      "Move to beginning of completion input."
      (interactive)
      (corfu--goto -1)
      (goto-char (car completion-in-region--data)))
    (defun corfu-end-of-prompt ()
      "Move to end of completion input."
      (interactive)
      (corfu--goto -1)
      (goto-char (cadr completion-in-region--data)))
    (global-corfu-mode)))

;; Use Dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\|svg\\|eps\\)\\'")))

;; yasnippet
(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode yas-active-extra-mode)
  :diminish
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

;; consult-yasnippet 依赖 corfu
(use-package consult-yasnippet
  :after (consult corfu)
  :bind ("C-c y" . consult-yasnippet))

;; 英语补全
(use-package corfu-english-helper
  :after corfu
  :defer t
  :load-path "~/.emacs.d/dependencies/corfu-english-helper/"
  :commands toggle-corfu-english-helper)

(provide 'idiig-text-completion)
