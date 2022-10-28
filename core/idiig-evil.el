;; 开启evil模式
(use-package evil
  :init
  (setq evil-want-integration t) 
  (setq evil-want-keybinding nil)
  
  ;; isert的时候到句末 
  (defun idiig/yank-to-end-of-line ()
    "Yank to end of line."
    (interactive)
    (evil-yank (point) (point-at-eol)))

  :config
  (progn 
    (global-evil-leader-mode)
    (evil-mode 1)
    ;; 所在字符串外添加parens
    (use-package evil-surround
      :config
      (global-evil-surround-mode 1))

    ;; 选中区域注释
    (use-package evil-nerd-commenter
      :defer t
      ;; :hook (prog-mode . evil-nerd-commenter)
      :commands evilnc-comment-or-uncomment-lines
      :init
      (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
      (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
      ;; :config
      ;; (evilnc-default-hotkeys)
      )

    ;; 群体替换
    (use-package evil-iedit-state
      :commands (evil-iedit-state evil-iedit-state/iedit-mode)
      :init
      (progn
        (setq iedit-current-symbol-default t
              iedit-only-at-symbol-boundaries t
              iedit-toggle-key-default nil)
        ;; (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
        ))
    
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    ;; disable highlight when use swiper or evil ex search, this option won't effect evil-ex-search-next command
    (setq-default evil-ex-search-persistent-highlight nil)

    ;; (adjust-major-mode-keymap-with-evil "git-timemachine")
    ;; (adjust-major-mode-keymap-with-evil "tabulated-list")

    (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)

    ;; ;; change evil initial mode state
    ;; (cl-loop for (mode . state) in
    ;;       '((shell-mode . normal)
    ;;         (minibuffer-inactive-mode . emacs))
    ;;       do (evil-set-initial-state mode state))

    ;;mimic "nzz" behaviou in vim
    (defadvice evil-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    ;; (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

    (defun my-evil-yank ()
      (interactive)
      (save-excursion
        (call-interactively 'evil-yank))
      (backward-char))

    (define-key evil-visual-state-map (kbd "y") 'my-evil-yank)

    (define-key evil-normal-state-map
      (kbd "Y") 'idiig/yank-to-end-of-line)

    ;; rebind g,k to gj and gk
    ;; (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    ;; (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))

    (define-key evil-normal-state-map (kbd "g[")
      (lambda () (interactive) (beginning-of-defun)))

    (define-key evil-normal-state-map (kbd "g]")
      (lambda () (interactive) (end-of-defun)))

    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
    (define-key evil-normal-state-map (kbd "M-y") 'consult-yank-pop)

    (define-key evil-insert-state-map "\C-e" 'end-of-line)
    (define-key evil-insert-state-map "\C-n" 'next-line)
    (define-key evil-insert-state-map "\C-k" 'kill-line)
    (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
    (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)
    (define-key evil-insert-state-map (kbd "s-b") 'backward-word)

    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)
    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map (kbd "C-r") 'idiig/evil-quick-replace)
    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "mp") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    (define-key evil-visual-state-map (kbd "mf") 'mc/mark-all-like-this-in-defun)

    ;; in spacemacs, we always use evilify miscro state
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
    ;; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)

    ;; (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
    ;; (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
    ;; (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
    ;; (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)

    ;; for emacs shell mode
    ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
    ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
    (evil-define-key 'emacs term-raw-map (kbd "C-w")
      'evil-delete-backward-word)

    ;; ;; color
    ;; (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
    ;;       evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
    ;;       evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
    ;;       evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
    ;;       evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
    ;;       evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))
    ;; (setq evil-insert-state-cursor '("chartreuse3" box))
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)))

;; evil leader key
(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (setq evil-leader/in-all-states t)
  (evil-leader/set-leader "<SPC>"))

;; Evil bindings 比如 evil-magit, agenda, etc.
(use-package evil-collection
  :after evil
  :custom
  (evil-emacs-state-cursor '("#663311" box)) 
  (evil-normal-state-cursor '("#005500" box)) 
  (evil-visual-state-cursor '("#663311" box))
  (evil-motion-state-cursor '("#005500" box))
  ;; 具体可以看文档
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init
   '(magit calendar ibuffer custom dired)))

(provide 'idiig-evil)
