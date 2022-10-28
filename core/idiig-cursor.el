;; avy 转跳
(use-package avy
  :defer t
  :commands (idiig/avy-open-url
             idiig/avy-goto-url
             avy-pop-mark
             avy-with
             avy-goto-char-2
             avy-jump)
  :bind (("C-s-'" . avy-goto-char-2)
         ("M-'" . avy-goto-char-2)
         ())
  :init
  (progn
    (setq avy-all-windows 'all-frames)
    (setq avy-background t)
    (defun idiig/avy-goto-url()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy-jump "https?://"))
    (defun idiig/avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (idiig/avy-goto-url)
        (browse-url-at-point)))
    ;; (evil-leader/set-key
    ;;  "jb" 'avy-pop-mark
    ;;  "jj" 'evil-avy-goto-char-timer
    ;;  "jl" 'evil-avy-goto-line
    ;;  "ju" 'idiig/avy-goto-url
    ;;  "jU" 'idiig/avy-open-url
    ;;  "jw" 'evil-avy-goto-word-or-subword-1
    ;;  "jo" 'idiig/avy-open-url)
    ;; (which-key-declare-prefixes "SPC j" "avy-jump")
    ))


;; 搜索修改当前选中区域高亮
(use-package iedit
  :defer t
  :commands (iedit-mode)
  :bind ("C-;" . iedit-mode))

;; 扩大选中
(use-package expand-region
  :defer t
  :commands er/expand-region
  :config
  (progn
    (with-eval-after-load 'expand-region
      (defadvice er/prepare-for-more-expansions-internal
          (around helm-ag/prepare-for-more-expansions-internal activate)
        ad-do-it
        (let ((new-msg (concat (car ad-return-value)
                               ", / to search in project, "
                               "b to search in buffer, "
                               "cs to change quote"))
              (new-bindings (cdr ad-return-value)))
          (cl-pushnew
           '("/" (lambda ()
                   (call-interactively
                    'idiig/consult-project-region-or-symbol)))
           new-bindings)
          (cl-pushnew
           '("b" (lambda ()
                   (call-interactively
                    'idiig/consult-buffer-region-or-symbol)))
           new-bindings)
          (setq ad-return-value (cons new-msg new-bindings)))))
    (setq expand-region-contract-fast-key "V"
          expand-region-reset-fast-key "r")))


;; multi cursor
(use-package multiple-cursors
  :custom
  (mc/list-file "~/.emacs.d/.cache/.mc-lists.el")
  :init
  (progn
    (bind-key* "C-s-l" 'mc/edit-lines)
    (bind-key* "C-s-f" 'mc/mark-all-dwim)
    (bind-key* "C-s-." 'mc/mark-next-like-this)
    (bind-key* "s-." 'mc/mark-next-like-this)
    (bind-key* "C-s-," 'mc/mark-previous-like-this)
    (bind-key* "s->" 'mc/unmark-next-like-this)
    (bind-key* "s-<" 'mc/unmark-previous-like-this)
    (bind-key* "C-c C-s-." 'mc/mark-all-like-this)

    ;; http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
    (define-prefix-command 'endless/mc-map)
    ;; C-x m is usually `compose-mail'. Bind it to something
    ;; else if you use this command.
    (define-key ctl-x-map "m" 'endless/mc-map)
    ;; Really really nice!
    (define-key endless/mc-map "i" #'mc/insert-numbers)
    (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
    (define-key endless/mc-map "a" #'mc/mark-all-like-this)

    ;; Occasionally useful
    (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
    (define-key endless/mc-map "r" #'mc/reverse-regions)
    (define-key endless/mc-map "s" #'mc/sort-regions)
    (define-key endless/mc-map "l" #'mc/edit-lines)
    (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
    (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines)
    )
  :config
  (setq mc/cmds-to-run-once
        '(
          counsel-M-x
          idiig/my-mc-mark-next-like-this))
  ;; (setq mc/cmds-to-run-for-all
  ;;       '(
  ;;         electric-newline-and-maybe-indent
  ;;         hungry-delete-backward
  ;;         spacemacs/backward-kill-word-or-region
  ;;         spacemacs/smart-move-beginning-of-line
  ;;         evil-substitute
  ;;         lispy-move-beginning-of-line
  ;;         lispy-move-end-of-line
  ;;         lispy-space
  ;;         lispy-delete-backward
  ;;         evil-exit-visual-state
  ;;         evil-backward-char
  ;;         evil-delete-char
  ;;         evil-escape-emacs-state
  ;;         evil-escape-insert-state
  ;;         mwim-beginning-of-code-or-line
  ;;         mwim-end-of-line-or-code
  ;;         evil-exit-emacs-state
  ;;         evil-previous-visual-line
  ;;         evil-next-visual-line
  ;;         evil-forward-char
  ;;         evil-insert
  ;;         evil-next-line
  ;;         evil-normal-state
  ;;         evil-previous-line
  ;;         evil-append
  ;;         evil-append-line
  ;;         forward-sentence
  ;;         kill-sentence
  ;;         org-self-insert-command
  ;;         sp-backward-delete-char
  ;;         sp-delete-char
  ;;         sp-remove-active-pair-overlay
  ;;         orgtbl-hijacker-command-109))
  )

(provide 'idiig-cursor)
