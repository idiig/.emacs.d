;; magit function
(defun idiig/magit-toggle-whitespace ()
  "Toggle whitespace in `magit-diff-mode'."
  (interactive)
  (if (member "-w" (if (derived-mode-p 'magit-diff-mode)
                       magit-refresh-args
                     magit-diff-section-arguments))
      (idiig//magit-dont-ignore-whitespace)
    (idiig//magit-ignore-whitespace)))

(defun idiig//magit-ignore-whitespace ()
  "Ignore whitespace in `magit-diff-mode'"
  (add-to-list (if (derived-mode-p 'magit-diff-mode)
                   'magit-refresh-args 'magit-diff-section-arguments) "-w")
  (magit-refresh))

(defun idiig//magit-dont-ignore-whitespace ()
  "Don't ignore whitespace in `magit-diff-mode'"
  (setq magit-diff-options
        (remove "-w"
                (if (derived-mode-p 'magit-diff-mode)
                    magit-refresh-args
                  magit-diff-section-arguments)))
  (magit-refresh))

;; git link function
(defun idiig/git-permalink ()
  "Allow the user to get a permalink via git-link in a git-timemachine buffer."
  (interactive)
  (let ((git-link-use-commit t))
    (call-interactively 'git-link-commit)))

(defun idiig//git-permalink-copy-url-only ()
  "Allow the user to get a permalink via git-link in a git-timemachine buffer."
  (interactive)
  (let (git-link-open-in-browser
        (git-link-use-commit t))
    (call-interactively 'git-link-commit)))

(defun idiig//git-link-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link)))

(defun idiig//git-link-commit-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link-commit)))

(defun idiig//git-stage-commit-current ()
  "git 的一系列常用组合"
  (interactive)
  (call-interactively 'magit-stage-file)
  (call-interactively 'magit-commit-create))

;; magit
(use-package magit
  :defer t
  :diminish (with-editor-mode)
  :init
  (progn
    (setq magit-completing-read-function 'magit-builtin-completing-read)
    (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
    ;; key bindings
    (which-key-declare-prefixes "SPC gf" "git-file")
    (which-key-declare-prefixes "SPC g" "git")
    (which-key-declare-prefixes "C-SPC gf" "git-file")
    (which-key-declare-prefixes "C-SPC g" "git")
    (evil-leader/set-key 
      "ga"  'magit-remote-add
      "gp"  'magit-push-current-to-upstream
      "gP"  'idiig//git-stage-commit-current
      "gb"  'magit-blame-addition/body
      "gc"  'magit-commit-create
      "gC"  'magit-clone
      "gfF" 'magit-find-file
      "gfl" 'magit-log-buffer-file
      "gfd" 'magit-diff
      "gi"  'magit-init
      "gL"  'magit-list-repositories
      "gm"  'magit-dispatch
      "gs"  'magit-status
      "gS"  'magit-stage-file
      "gU"  'magit-unstage-file)
    (defhydra magit-blame-addition (:color red
                                           :hint nil)
      "
Press [_b_] again to blame further in the history, [_q_] to go up or quit."
      ("b" magit-blame-addition)
      ;; here we use the :exit keyword because we should exit the
      ;; micro-state only if the magit-blame-quit effectively disable
      ;; the magit-blame mode.
      ("q" nil :exit (progn (when (bound-and-true-p magit-blame-mode)
                              (magit-blame-quit))
                            (not (bound-and-true-p magit-blame-mode))))
      )
    :config
    (progn
      ;; set repositories
      (setq magit-repository-directories
            '(("~/Nutstore/works/" . 2)
              ))
      ;; seems to be necessary at the time of release
      (require 'git-rebase)
      ;; bind function keys
      ;; (define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)
      (define-key magit-repolist-mode-map (kbd "gr") 'magit-list-repositories)
      (define-key magit-repolist-mode-map (kbd "RET") 'magit-repolist-status)
      ;; confirm/abort
      (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
      (let ((mm-key idiig-major-mode-leader-key))
        (dolist (state '(normal motion))
          (evil-define-key state with-editor-mode-map
            (concat mm-key mm-key) 'with-editor-finish
            (concat mm-key "a")    'with-editor-cancel
            (concat mm-key "c")    'with-editor-finish
            (concat mm-key "k")    'with-editor-cancel)
          (evil-define-key state magit-log-select-mode-map
            (concat mm-key mm-key) 'magit-log-select-pick
            (concat mm-key "a")    'magit-log-select-quit
            (concat mm-key "c")    'magit-log-select-pick
            (concat mm-key "k")    'magit-log-select-quit)))
      ;; whitespace
      (define-key magit-status-mode-map (kbd "C-S-w")
        'idiig/magit-toggle-whitespace)
      ;; full screen magit-status
      ;; (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
      ;; Workaround for #12747 - org-mode
      (evil-define-key 'normal magit-blame-read-only-mode-map (kbd "RET") 'magit-show-commit))))

;; ;; gitignore
;; (use-package helm-gitignore
;;   :defer t
;;   :init (evil-leader/set-key "gI" 'helm-gitignore))

;; (use-package gitconfig-mode
;;   :defer t)

;; (use-package gitignore-templates
;;   :defer t
;;   :init
;;   (evil-leader/set-key-for-mode 'gitignore-mode
;;     "i" 'gitignore-templates-insert)
;;   (evil-leader/set-key
;;     "gfi" 'gitignore-templates-new-file))

;; git with org
(use-package orgit
  :defer t)

(defun idiig/org-reveal-advice (&rest _args)
  (when (derived-mode-p 'org-mode)
    (org-reveal)))

(advice-add 'magit-blame-addition :after #'idiig/org-reveal-advice)
(advice-add 'magit-diff-visit-file :after #'idiig/org-reveal-advice)
(advice-add 'magit-diff-visit-worktree-file
            :after #'idiig/org-reveal-advice)

;; highlight for git
(use-package smeargle
  :defer t
  :init
  (progn
    (which-key-declare-prefixes "SPC gH" "highlight")
    (which-key-declare-prefixes "C-SPC gH" "highlight")
    ;; TODO abstract this to a function
    (let ((descr
           '(("smeargle" . "highlight by last update time")
             ("smeargle-commits" . "highlight by age of changes")
             ("smeargle-clear" . "clear"))))
      (dolist (nd descr)
        ;; ensure the target matches the whole string
        (push (cons (cons nil (concat "\\`" (car nd) "\\'"))
                    (cons nil (cdr nd)))
              which-key-replacement-alist)))
    (evil-leader/set-key
      "gHc" 'smeargle-clear
      "gHh" 'smeargle-commits
      "gHt" 'smeargle)))

;; git link
(use-package git-link
  :defer t
  :init
  (progn
    (which-key-declare-prefixes "SPC gl" "links")
    (which-key-declare-prefixes "C-SPC gl" "links")
    (evil-leader/set-key
      "glc" 'git-link-commit
      "glC" 'idiig//git-link-commit-copy-url-only
      "gll" 'git-link
      "glL" 'idiig//git-link-copy-url-only
      "glp" 'idiig//git-permalink
      "glP" 'idiig//git-permalink-copy-url-only)
    ;; default is to open the generated link
    (setq git-link-open-in-browser t)))

;; git message
(use-package git-messenger
  :defer t
  :init (evil-leader/set-key "gM" 'git-messenger:popup-message)
  :config (define-key git-messenger-map [escape] 'git-messenger:popup-close))

;; git 时光机
(use-package git-timemachine
  :defer t
  :commands git-time-machine/body
  :init
  (evil-leader/set-key "gt" 'git-time-machine/body)
  (evil-leader/set-key "gt" 'git-time-machine/body)
  :config
  (progn
    (defhydra git-time-machine (:color red
                                       :hint nil)
      "
[_p_/_N_] previous [_n_] next [_m_] select [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit"
      ("c" git-timemachine-show-current-revision)
      ("g" git-timemachine-show-nth-revision)
      ("p" git-timemachine-show-previous-revision)
      ("n" git-timemachine-show-next-revision)
      ("N" git-timemachine-show-previous-revision)
      ("Y" git-timemachine-kill-revision)
      ("m" my-git-timemachine :exit t)
      ("q" nil :exit t))
    
    (defun my-unwind-git-timemachine ()
      (if (not (eq last-command-event 13))
          (git-timemachine-quit)))

    ;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
    (defun my-git-timemachine-show-selected-revision ()
      "Show last (current) revision of file."
      (interactive)
      (let (collection)
        (setq collection
              (mapcar (lambda (rev)
                        ;; re-shape list for the ivy-read
                        (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                      (git-timemachine--revisions)))
        (ivy-read "commits:"
                  collection
                  :unwind #'my-unwind-git-timemachine
                  :action (lambda (rev)
                            (git-timemachine-show-revision (cdr rev))))))

    (defun my-git-timemachine ()
      "Open git snapshot with the selected version.  Based on ivy-mode."
      (interactive)
      (unless (featurep 'git-timemachine)
        (require 'git-timemachine))
      (git-timemachine--start #'my-git-timemachine-show-selected-revision))))

(use-package transient
  :defer t
  :init
  (setq
   transient-levels-file
   (expand-file-name "transient/levels.el" idiig-cache-dir)
   transient-values-file
   (expand-file-name "transient/values.el" idiig-cache-dir)
   transient-history-file
   (expand-file-name "transient/history.el" idiig-cache-dir)))

;; ;; evil-magit 已被并入 evil－collection
;; (use-package evil-magit)

;; ;; git 检索 但better search已经有类似功能
;; (use-package helm-git-grep
;;   :defer t
;;   :after magit
;;   :init (evil-leader/set-key
;;          "g/" 'helm-git-grep
;;          "g*" 'helm-git-grep-at-point))

(provide 'idiig-git)
