;;  main keybindings
(defun idiig//org-set-keys ()
  (evil-leader/set-key-for-mode 'org-mode
    "'" 'org-edit-special
    "c" 'org-capture

    ;; Clock
    ;; These keybindings should match those under the "oC" prefix (below)
    "Cc" 'org-clock-cancel
    "Cd" 'org-clock-display
    "Ce" 'org-evaluate-time-range
    "Cg" 'org-clock-goto
    "Ci" 'org-clock-in
    "CI" 'org-clock-in-last
    "Cj" 'org-clock-jump-to-current-clock
    "Co" 'org-clock-out
    "CR" 'org-clock-report
    "Cr" 'org-resolve-clocks

    "dd" 'org-deadline
    "ds" 'org-schedule
    "dt" 'org-time-stamp
    "dT" 'org-time-stamp-inactive
    ;; "ee" 'org-export-dispatch
    "e" 'org-export-dispatch
    "fi" 'org-feed-goto-inbox
    "fu" 'org-feed-update-all

    "a" 'org-agenda

    "p" 'org-priority

    "Tc" 'org-toggle-checkbox
    "Te" 'org-toggle-pretty-entities
    "Ti" 'org-toggle-inline-images
    "Tn" 'org-num-mode
    "Tl" 'org-toggle-link-display
    "Tt" 'org-show-todo-tree
    "TT" 'org-todo
    "TV" 'space-doc-mode
    "Tx" 'org-latex-preview

    ;; More cycling options (timestamps, headlines, items, properties)
    "L" 'org-shiftright
    "H" 'org-shiftleft
    "J" 'org-shiftdown
    "K" 'org-shiftup

    ;; Change between TODO sets
    "C-S-l" 'org-shiftcontrolright
    "C-S-h" 'org-shiftcontrolleft
    "C-S-j" 'org-shiftcontroldown
    "C-S-k" 'org-shiftcontrolup

    ;; Subtree editing
    "sa" 'org-toggle-archive-tag
    "sA" 'org-archive-subtree-default
    "sb" 'org-tree-to-indirect-buffer
    "sd" 'org-cut-subtree
    "sy" 'org-copy-subtree
    "sh" 'org-promote-subtree
    "sj" 'org-move-subtree-down
    "sk" 'org-move-subtree-up
    "sl" 'org-demote-subtree
    "sn" 'org-narrow-to-subtree
    "sw" 'widen
    "sr" 'org-refile
    "ss" 'org-sparse-tree
    "sS" 'org-sort

    ;; tables
    "ta" 'org-table-align
    "tb" 'org-table-blank-field
    "tc" 'org-table-convert
    "tdc" 'org-table-delete-column
    "tdr" 'org-table-kill-row
    "te" 'org-table-eval-formula
    "tE" 'org-table-export
    "tf" 'org-table-field-info
    "th" 'org-table-previous-field
    "tH" 'org-table-move-column-left
    "tic" 'org-table-insert-column
    "tih" 'org-table-insert-hline
    "tiH" 'org-table-hline-and-move
    "tir" 'org-table-insert-row
    "tI" 'org-table-import
    "tj" 'org-table-next-row
    "tJ" 'org-table-move-row-down
    "tK" 'org-table-move-row-up
    "tl" 'org-table-next-field
    "tL" 'org-table-move-column-right
    "tn" 'org-table-create
    "tN" 'org-table-create-with-table.el
    "tr" 'org-table-recalculate
    "tR" 'org-table-recalculate-buffer-tables
    "ts" 'org-table-sort-lines
    "ttf" 'org-table-toggle-formula-debugger
    "tto" 'org-table-toggle-coordinate-overlays
    "tw" 'org-table-wrap-region

    ;; Source blocks / org-babel
    "bp"     'org-babel-previous-src-block
    "bn"     'org-babel-next-src-block
    "be"     'org-babel-execute-maybe
    "bo"     'org-babel-open-src-block-result
    "bv"     'org-babel-expand-src-block
    "bu"     'org-babel-goto-src-block-head
    "bg"     'org-babel-goto-named-src-block
    "br"     'org-babel-goto-named-result
    "bb"     'org-babel-execute-buffer
    "bs"     'org-babel-execute-subtree
    "bd"     'org-babel-demarcate-block
    "bt"     'org-babel-tangle
    "bf"     'org-babel-tangle-file
    "bc"     'org-babel-check-src-block
    "bj"     'org-babel-insert-header-arg
    "bl"     'org-babel-load-in-session
    "bi"     'org-babel-lob-ingest
    "bI"     'org-babel-view-src-block-info
    "bz"     'org-babel-switch-to-session
    "bZ"     'org-babel-switch-to-session-with-code
    "ba"     'org-babel-sha1-hash
    "bx"     'org-babel-do-key-sequence-in-edit-buffer
    ;; "b."     'idiig/org-babel-transient-state/body
    ;; Multi-purpose keys
    "*" 'org-ctrl-c-star
    "-" 'org-ctrl-c-minus
    "#" 'org-update-statistics-cookies
    "RET"   'org-ctrl-c-ret
    "M-RET" 'org-meta-return
    ;; attachments
    "A" 'org-attach
    ;; insertion
    "ib" 'org-insert-structure-template
    "id" 'org-insert-drawer
    "ie" 'org-set-effort
    "if" 'org-footnote-new
    "ih" 'org-insert-heading
    "iH" 'org-insert-heading-after-current
    "ii" 'org-insert-item
    ;; "iK" 'idiig/insert-keybinding-org
    "il" 'org-insert-link
    "in" 'org-add-note
    "ip" 'org-set-property
    "is" 'org-insert-subheading
    "it" 'org-set-tags-command
    ;; region manipulation
    "xb" (idiig|org-emphasize idiig/org-bold ?*)
    "xc" (idiig|org-emphasize idiig/org-code ?~)
    "xi" (idiig|org-emphasize idiig/org-italic ?/)
    "xo" 'org-open-at-point
    "xr" (idiig|org-emphasize idiig/org-clear ? )
    "xs" (idiig|org-emphasize idiig/org-strike-through ?+)
    "xu" (idiig|org-emphasize idiig/org-underline ?_)
    "xv" (idiig|org-emphasize idiig/org-verbatim ?=))
  )

(defun idiig//org-declare-prefixes-for-mode ()
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mC" "clocks")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC md" "schedule")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mi" "insert/add/set")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC ms" "subtree")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mt" "table")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mtd" "table delete")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mtt" "table toggle")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mti" "table insert")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mb" "babel")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mT" "toggle")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mx" "region manipulation")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mf" "feed")
  (which-key-declare-prefixes-for-mode 'org-mode "SPC mz" "zetteldeft")
  (which-key-declare-prefixes-for-mode 'org-mode ",C" "clocks")
  (which-key-declare-prefixes-for-mode 'org-mode ",d" "schedule")
  (which-key-declare-prefixes-for-mode 'org-mode ",i" "insert/add/set")
  (which-key-declare-prefixes-for-mode 'org-mode ",s" "subtree")
  (which-key-declare-prefixes-for-mode 'org-mode ",t" "table")
  (which-key-declare-prefixes-for-mode 'org-mode ",td" "table delete")
  (which-key-declare-prefixes-for-mode 'org-mode ",tt" "table toggle")
  (which-key-declare-prefixes-for-mode 'org-mode ",ti" "table insert")
  (which-key-declare-prefixes-for-mode 'org-mode ",b" "babel")
  (which-key-declare-prefixes-for-mode 'org-mode ",T" "toggle")
  (which-key-declare-prefixes-for-mode 'org-mode ",x" "region manipulation")
  (which-key-declare-prefixes-for-mode 'org-mode ",f" "feed"))

;; org functions
;; evil surround 对应 org src
(defun idiig//surround-drawer ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format ":%s:" (upcase (or dname ""))) ":END:")))

(defun idiig//surround-code ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format "#+BEGIN_SRC %s" (or dname "")) "#+END_SRC")))

(defun idiig/org-setup-evil-surround ()
  (with-eval-after-load 'evil-surround
    (add-to-list 'evil-surround-pairs-alist '(?: . idiig//surround-drawer))
    (add-to-list 'evil-surround-pairs-alist '(?# . idiig//surround-code))))
(add-hook 'org-mode-hook 'idiig/org-setup-evil-surround)

;; 快速插入代码块
(defun idiig/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "dot" "gnuplot" "R" "sql" "awk" "haskell" "latex" "lisp" "org"
            "perl" "ruby" "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))
(add-hook 'org-mode-hook '(lambda ()
                                ;; keybinding for editing source code blocks
                                ;; keybinding for inserting code blocks
                                (local-set-key (kbd "C-c i s")
                                               'idiig/org-insert-src-block)))

;; ispell不检查部分
(defun idiig/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'idiig/org-ispell)

;; mac抓取
(defun idiig-org/init-org-mac-link ()
  (use-package org-mac-link
    :commands org-mac-grab-link
    :init
    (progn
      (add-hook 'org-mode-hook
                (lambda ()
                  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))))
    :defer t))

;; chrome url 抓取
(defun idiig/insert-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (idiig/retrieve-chrome-current-tab-url)))

(defun idiig/retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  "	set theUrl to get URL of active tab of first window\n"
                  "	set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

;; deft
(use-package deft
  :defer t
  :diminish
  ;; :after org
  :init
  (progn
    (setq deft-use-filter-string-for-filename t)
    (setq deft-recursive t)
    (setq deft-extension "org")
    (setq deft-directory "~/Nutstore/org-files/research-notes")
    (setq deft-text-mode 'org-mode)
    ;; keybindings
    (which-key-declare-prefixes "SPC od" "deft")
    (which-key-declare-prefixes "C-SPC od" "deft")
    (evil-leader/set-key "odd" 'deft)
    (evil-leader/set-key "odc" 'deft-new-file)
    (evil-define-key 'normal deft-mode-map "q" 'quit-window)
    (evil-leader/set-key-for-mode 'deft-mode
      "c" 'deft-filter-clear
      "d" 'deft-delete-file
      "i" 'deft-toggle-incremental-search
      "n" 'deft-new-file
      "N" 'deft-new-file-named
      "q" 'quit-window
      "o" 'deft-open-file-other-window
      "r" 'deft-rename-file)))

;; ;; zetteldeft for linking notes
;; (use-package zetteldeft
;;   :defer t
;;   ;; :after (org deft)
;;   :init
;;   (progn
;;     (zetteldeft-set-classic-keybindings)
;;     ;; zetteldeft actions in deft mode
;;     (which-key-declare-prefixes-for-mode 'deft-mode
;;       ",z" "zetteldeft"
;;       "SPC mz" "zetteldeft")
;;     (evil-leader/set-key-for-mode 'deft-mode
;;       "zT" 'zetteldeft-tag-buffer
;;       "zn" 'zetteldeft-new-file
;;       )

;;     ;; zetteldeft actions in org mode
;;     (which-key-declare-prefixes-for-mode 'org-mode
;;       ",z" "zetteldeft"
;;       "SPC mz" "zetteldeft")
;;     (evil-leader/set-key-for-mode 'org-mode
;;       "zc" 'zetteldeft-search-current-id
;;       "zf" 'zetteldeft-follow-link
;;       "zt" 'zetteldeft-avy-tag-search
;;       "zN" 'zetteldeft-new-file-and-link
;;       "zr" 'zetteldeft-file-rename
;;       "zi" 'zetteldeft-find-file-id-insert
;;       "zI" 'zetteldeft-find-file-full-title-insert
;;       "zs" 'zetteldeft-search-at-point
;;       "zl" 'zetteldeft-avy-link-search
;;       "zF" 'zetteldeft-avy-file-search-ace-window
;;       "zo" 'zetteldeft-find-file
;;       )
;;     ))

;; org-roam for linking notes
(use-package org-roam
  :defer t
  :hook (after-init . org-roam-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-find-file
             org-roam-graph
             org-roam-insert
             org-roam-switch-to-buffer
             org-roam-dailies-date
             org-roam-dailies-today
             org-roam-dailies-tomorrow
             org-roam-dailies-yesterday)
  :preface
  ;; Set this to nil so we can later detect whether the user has set a custom
  ;; directory for it, and default to `org-directory' if they haven't.
  (defvar org-roam-directory nil)
  :init
  (progn 

    ;; (which-key-declare-prefixes-for-mode 'org-mode "SPC mr" "org-roam")
    (which-key-declare-prefixes-for-mode 'org-mode "SPC mrd" "org-roam-dailies")
    (which-key-declare-prefixes-for-mode 'org-mode "SPC mrt" "org-roam-tags")
    ;; (which-key-declare-prefixes-for-mode 'org-mode ",r" "org-roam")
    (which-key-declare-prefixes-for-mode 'org-mode ",rd" "org-roam-dailies")
    (which-key-declare-prefixes-for-mode 'org-mode ",rt" "org-roam-tags")
    (evil-leader/set-key-for-mode 'org-mode
      "rb" 'org-roam-switch-to-buffer
      "rdy" 'org-roam-dailies-find-yesterday
      "rdt" 'org-roam-dailies-find-today
      "rdT" 'org-roam-dailies-find-tomorrow
      "rdd" 'org-roam-dailies-find-date
      "rf" 'org-roam-find-file
      "rg" 'org-roam-graph
      "ri" 'org-roam-insert
      "rI" 'org-roam-insert-immediate
      "rl" 'org-roam-buffer-toggle-display
      "rta" 'org-roam-tag-add
      "rtd" 'org-roam-tag-delete
      "ra" 'org-roam-alias-add
      "rc" 'org-roam-capture))

  :config
  (progn
    (setq org-roam-directory "~/Nutstore/org-files/research-notes"
          org-roam-verbose nil  ; https://youtu.be/fn4jIlFwuLU
          org-roam-buffer-no-delete-other-windows t ; make org-roam buffer sticky
          org-roam-completion-system 'default)))

;; noter
(use-package org-noter
  :after (org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'right
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path "~/Nutstore/org-files/research-notes"
   ))

;; evil-org
(use-package evil-org
  :diminish (evil-org-mode)
  :after org
  :init
  (progn
    ;; (defun idiig//evil-org-mode ()
    ;;   (evil-org-mode)
    ;;   (evil-normalize-keymaps))
    
    (defvar org-want-todo-bindings nil
      "If non-nil, evil-org's todo bindings are activated.")
    
    (setq evil-org-use-additional-insert t
          evil-org-key-theme `(textobjects
                               navigation
                               additional
                               ,@(when org-want-todo-bindings '(todo)))))
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; org agenda
(use-package org-agenda
  :ensure nil
  :defer t
  :after org
  :custom
  ;; agenda startup 
  (org-agenda-inhibit-startup t) ;; ~50x speedup 
  (org-agenda-use-tag-inheritance nil) ;; 3-4x speedup 
  (org-agenda-span 'day) 
  (org-agenda-window-setup 'current-window)
  ;; face
  (org-agenda-current-time-string "<<<<<<<< now")
  (org-agenda-time-grid ;; Format is changed from 9.1
   '((daily today require-timed)
     (0100 0300 0500 0700 0900 1200 1300
           1500 1700 1900 2000 2200 2400)
     "-"
     "────────────"))
  :hook
  (org-agenda-mode-hook . (lambda ()
                            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
                            (auto-save-mode)))
  :init
  (defun notify-osx (title message)
    ;; 用terminal notifier提示 （mac）
    (call-process "terminal-notifier"
                  nil 0 nil
                  "-group" "Emacs"
                  "-title" title
                  "-message" message
                  "-sender" "org.gnu.Emacs"
                  "-activate" "org.gnu.Emacs"))
  (progn
    (setq org-agenda-restore-windows-after-quit t)
    (which-key-declare-prefixes-for-mode 'org-agenda-mode
      ",C" "clock"
      "SPC mC" "clock"
      ",d" "schedule/deadline"
      "SPC md" "schedule/deadline"
      ",i" "tag/property/priority"
      "SPC md" "tag/property/priority"
      ",s" "refile")
    (evil-leader/set-key-for-mode 'org-agenda-mode
      "," 'org-agenda-ctrl-c-ctrl-c
      "a" 'org-agenda
      "c" 'org-agenda-capture
      "Cc" 'org-agenda-clock-cancel
      "Ci" 'org-agenda-clock-in
      "Co" 'org-agenda-clock-out
      "Cj" 'org-agenda-clock-goto
      "dd" 'org-agenda-deadline
      "ds" 'org-agenda-schedule
      "ie" 'org-agenda-set-effort
      "ip" 'org-agenda-set-property
      "iP" 'org-agenda-priority
      "it" 'org-agenda-set-tags
      "sr" 'org-agenda-refile))
  :config
  (with-eval-after-load 'evil-org-agenda
    (evil-define-key 'motion org-agenda-mode-map "j" 'org-agenda-next-line)
    (evil-define-key 'motion org-agenda-mode-map "k" 'org-agenda-previous-line)
    (evil-define-key 'motion org-agenda-mode-map "K" nil)
    (evil-define-key 'motion org-agenda-mode-map (kbd "C-h") nil) 
    (evil-define-key 'motion org-agenda-mode-map (kbd "M-j") 'org-agenda-next-item)
    (evil-define-key 'motion org-agenda-mode-map (kbd "M-k") 'org-agenda-previous-item)
    (evil-define-key 'motion org-agenda-mode-map (kbd "M-h") 'org-agenda-earlier)
    (evil-define-key 'motion org-agenda-mode-map (kbd "M-l") 'org-agenda-later)
    (evil-define-key 'motion org-agenda-mode-map (kbd "gd") 'org-agenda-toggle-time-grid)
    (evil-define-key 'motion org-agenda-mode-map (kbd "gr") 'org-agenda-redo)
    (evil-define-key 'motion org-agenda-mode-map (kbd "gw") 'org-agenda-week-view)
    (evil-define-key 'motion org-agenda-mode-map (kbd "gm") 'org-agenda-month-view)
    (evil-define-key 'motion org-agenda-mode-map (kbd "M-RET") 'org-agenda-show-and-scroll-up)
    (evil-define-key 'motion org-agenda-mode-map (kbd "q") '(lambda () (interactive)
                                                              (org-save-all-org-buffers)
                                                              (org-agenda--quit))))
  ;;An entry without a cookie is treated just like priority ' B '.
  ;;So when create new task, they are default 重要且紧急
  (setq org-agenda-custom-commands
        '(
          ("w" . "任务安排")
          ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
          ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
          ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
          ("b" "Blog" tags-todo "BLOG")
          ("p" . "项目安排")
          ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"work\"")
          ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"idiig\"")
          ("W" "Weekly Review"
           ((stuck "") ;; review stuck projects as designated by org-stuck-projects
            (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
            )))))

(use-package org-capture
  :ensure nil
  :after org
  :commands (org-capture)
  :init
  (progn
    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
             "* %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
             "* TODO [#B] %?\n  %i\n %U"
             :empty-lines 1)
            ("s" "Code Snippet" entry
             (file org-agenda-file-code-snippet)
             "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
            ("w" "work" entry (file+headline org-agenda-file-gtd "Work")
             "* TODO [#A] %?\n  %i\n %U"
             :empty-lines 1)
            ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n %(idiig/retrieve-chrome-current-tab-url)\n %i\n %U"
             :empty-lines 1)
            ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n  %i\n %a \n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree org-agenda-file-journal)
             "* %?"
             :empty-lines 1))))
  :config
  (progn
    (defun idiig//org-capture-start ()
      "Make sure that the keybindings are available for org capture."
      (evil-leader/set-key-for-mode 'org-capture-mode
        ;; "," 'org-capture-finalize
        "a" 'org-capture-kill
        "c" 'org-capture-finalize
        "k" 'org-capture-kill
        "r" 'org-capture-refile)
      ;; Evil bindins seem not to be applied until at least one
      ;; Evil state is executed
      (evil-normal-state))
    ;; Must be done everytime we run org-capture otherwise it will
    ;; be ignored until insert mode is entered.
    (add-hook 'org-capture-mode-hook 'idiig//org-capture-start)
    
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:"
                     "%?\n")        ;Place the cursor here finally
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"              ;`org-capture' binding + h
                   "Hugo post"
                   entry
                   ;; It is assumed that below file is present in `org-directory'
                   ;; and that it has a "Blog Ideas" heading. It can even be a
                   ;; symlink pointing to the actual location of all-posts.org!
                   (file+headline org-agenda-file-blogposts "Blog Ideas")
                   (function org-hugo-new-subtree-post-capture-template)))))

;; 提醒事项
(use-package appt
  :ensure nil
  :after org-agenda
  :init
  (setq appt-time-msg-list nil)    ;; clear existing appt list
  (setq appt-display-interval '10)  ;; warn every 5 minutes from t - appt-message-warning-time
  (setq
   appt-message-warning-time '20  ;; send first warning 15 minutes before appointment
   appt-display-mode-line nil     ;; don't show in the modeline
   appt-display-format 'window)   ;; pass warnings to the designated window function
  :config
  (appt-activate 1)                ;; activate appointment notification
  ;; (display-time)                   ;; activate time display
  (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
  (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
  (defun idiig-appt-display (min-to-app new-time msg)
    (notify-osx
     (format "Appointment in %s minutes" min-to-app)    ;; passed to -title in terminal-notifier call
     (format "%s" msg)))                                ;; passed to -message in terminal-notifier call
  (setq appt-disp-window-function (function idiig-appt-display)))

;; pomodoro 和时间相关
(use-package org-pomodoro
  :commands org-pomodoro
  :after org-agenda
  :custom
  (org-pomodoro-ask-upon-killing t)
  (org-pomodoro-format "%s")
  (org-pomodoro-short-break-format "☕ %s")
  (org-pomodoro-long-break-format  "☕ %s")
  :init
  (evil-define-key 'motion org-agenda-mode-map (kbd "P") 'org-pomodoro)
  ;; (evil-define-key 'normal org-mode-map (kbd "P") 'org-pomodoro)
  :config
  (progn (add-hook 'org-pomodoro-started-hook '(lambda () (notify-osx
                                                           "org-pomodoro"
                                                           "集。。。中。。。")))
         (add-hook 'org-pomodoro-finished-hook '(lambda () (notify-osx
                                                            "org-pomodoro"
                                                            "☕️咖。。啡。。。。。")))
         (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (notify-osx
                                                                        "org-pomodoro"
                                                                        "继。。继续。。。。。")))
         (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (notify-osx
                                                                       "org-pomodoro"
                                                                       "继。。继续。。。。。")))))

(use-package ox
  :ensure nil
  :after org
  :init
  ;; https://qiita.com/kawabata@github/items/1b56ec8284942ff2646b
  ;; org-mode を他ファイルに出力するときは、日本語文字同士の間の改行が削除される。
  (defun remove-org-newlines-at-cjk-text (&optional _mode)
    "先頭が '*', '#', '|' でなく、改行の前後が日本の文字の場合はその改行を除去する。"
    (interactive)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^|#*\n].+\\)\\(.\\)\n *\\(.\\)" nil t)
      (if (and (> (string-to-char (match-string 2)) #x2000)
               (> (string-to-char (match-string 3)) #x2000))
          (replace-match "\\1\\2\\3"))
      (goto-char (point-at-bol))))
  (add-hook 'org-export-before-processing-hook 'remove-org-newlines-at-cjk-text)
  :config  
  (progn

    ;; hogo
    (use-package ox-hugo) 

    ;; TeX
    (use-package ox-latex
      :ensure nil
      :init
      (progn
        (setq org-latex-compiler "xelatex")
        (setq org-latex-default-packages-alist nil)
        ;; (setq org-latex-packages-alist
        ;;       '(("" "fontspec" t)))
        (setq org-latex-default-class "draft") 
        (setq org-latex-listings 'minted)
        (setq org-latex-minted-options
              '(("frame" "lines")
                ("framesep=2mm")
                ("linenos=true")
                ("baselinestretch=1.2")
                ("fontsize=\\footnotesize")
                ("breaklines")
                ))
        (setq org-preview-latex-default-process 'dvisvgm)
        ;; (setq org-preview-latex-default-process 'imagemagick)
        ;; (setq org-latex-create-formula-image-program 'imagemagick)  ;速度较慢，但支持中文
        (setq org-format-latex-options
              (plist-put org-format-latex-options :scale 2.0))      ;调整 LaTeX 预览图片的大小
        (setq org-format-latex-options
              (plist-put org-format-latex-options :html-scale 2.5)) ;调整 HTML 文件中 LaTeX 图像的大小
        (setq org-preview-latex-process-alist
              '(
                (dvisvgm
                 :programs ("xelatex" "dvisvgm")
                 :description "xdv > svg"
                 :message "you need to install the programs: xelatex and dvisvgm."
                 :image-input-type "xdv"
                 :image-output-type "svg"
                 :image-size-adjust (1.0 . 1.0)
                 :latex-compiler ("xelatex --no-pdf -interaction nonstopmode -output-directory %o %f")
                 :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
                (imagemagick
                 :programs ("latex" "convert")
                 :description "pdf > png"
                 :message "you need to install the programs: xelatex and imagemagick."
                 :image-input-type "pdf"
                 :image-output-type "png"
                 :image-size-adjust (1.0 . 1.0)
                 :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
                 :image-converter
                 ("convert -density %D -trim -antialias %f -quality 100 %O"))))
        ;; 输出设定
        (setq org-latex-pdf-process
              '("xelatex --shell-escape %f"
                "biber %b"
                "xelatex --shell-escape %f"
                "xelatex --shell-escape %f"
                "dvipdfmx %b.dvi"
                "rm -fr %b.bbl %b.dvi %b.tex auto"
                ))))

    (use-package ox-beamer
      :ensure nil)

    (add-to-list 'org-latex-classes
                 '("draft"
                   "
\\documentclass[autodetect-engine,dvi=dvipdfmx,11pt]{article}
\\usepackage{graphicx}
\\usepackage{geometry}
\\usepackage[T1]{fontenc}
\\usepackage{xeCJK}
\\setCJKmainfont[Scale=MatchLowercase]{ipaexm.ttf}
\\geometry{a4paper,left=20mm,right=20mm,top=20mm,bottom=20mm,heightrounded}
\\usepackage[yyyymmdd]{datetime}
\\renewcommand{\\dateseparator}{/}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{textcomp}
\\usepackage{multicol}
\\usepackage{amsmath,amsthm,amssymb}
\\usepackage{bm}
\\usepackage{booktabs}
\\usepackage{csquotes}
\\usepackage{tikz}
\\usepackage{svg}
\\usepackage{minted}
%
% color link
\\usepackage{xcolor}
\\usepackage{hyperref}
\\ifdefined\\kanjiskip
  \\usepackage{pxjahyper}
  \\hypersetup{colorlinks=true,citecolor=blue}
\\else
  \\ifdefined\\XeTeXversion
      \\hypersetup{colorlinks=true,citecolor=blue}
  \\else
    \\ifdefined\\directlua
      \\hypersetup{pdfencoding=auto,colorlinks=true,citecolor=blue}
    \\else
      \\hypersetup{unicode,colorlinks=true,citecolor=blue}
    \\fi
  \\fi
\\fi
%
\\usepackage{url}
%
% bibliography
\\usepackage[natbib,style=apa,backend=biber,giveninits=false]{biblatex}
%
%
\\usepackage{etoolbox}
\\makeatletter
\\patchcmd{\\@verbatim}
  {\\verbatim@font}
  {\\verbatim@font\\footnotesize}
  {}{}
\\makeatother
%
% Footnote setting
\\usepackage{footmisc}
\\DefineFNsymbols{mySymbols}{{\\ensuremath\\dagger}{\\ensuremath\\ddagger}\\S\\P
   *{**}{\\ensuremath{\\dagger\\dagger}}{\\ensuremath{\\ddagger\\ddagger}}}
\\setfnsymbol{mySymbols}
\\renewcommand{\\thefootnote}{\\arabic{footnote}\\enspace}
%
% header
\\usepackage{fancyhdr}
\\pagestyle{fancyplain}
\\fancyhf{}
\\lhead{\\fancyplain{}{draft: \\jobname}}
\\rhead{\\fancyplain{}{latest update: \\today\\enspace\\currenttime}}
\\cfoot{\\fancyplain{}{\\thepage}}
%
% caption
\\usepackage{caption}
\\captionsetup[figure]{labelfont={bf}, labelsep=colon, justification=raggedright, format=hang}
\\captionsetup[table]{labelfont={bf}, labelsep=colon, justification=raggedright, format=hang}
\\usepackage{capt-of}
\\usepackage{subcaption}
% 
% quote
\\usepackage{framed}
\\renewenvironment{quote}[1][\\hsize]
{%
  \\def\\FrameCommand
  {%
    {\\color{gray}\\vrule width 4pt}%
    \\hspace{0pt}%must no space.
    \\fboxsep=\\FrameSep\\colorbox{white}%
  }%
  \\MakeFramed{\\hsize#1\\advance\\hsize-\\width\\FrameRestore}%
}
{\\endMakeFramed}
%
% title
\\makeatletter
\\renewcommand{\\maketitle}{\\bgroup\\setlength{\\parindent}{0pt}
\\begin{flushleft}
  \\Large{\\textsf{\\@title}}\\\\
  \\normalsize\\@author
\\end{flushleft}\\egroup
}
\\makeatother
"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    ))

;; ;; FIXME
;; ;; org-proectile
;; (use-package org-projectile
;;   :commands (org-projectile-location-for-project)
;;   :init
;;   (defun org-projectile/capture (&optional arg)
;;     (interactive "P")
;;     (if arg
;;         (org-projectile-project-todo-completing-read :empty-lines 1)
;;       (org-projectile-capture-for-current-project :empty-lines 1)))

;;   (defun org-projectile/goto-todos ()
;;     (interactive)
;;     (org-projectile-goto-location-for-project (projectile-project-name)))

;;   (progn
;;     (evil-leader/set-key
;;       "op" 'org-projectile/capture
;;       "opo" 'org-projectile/goto-todos)
;;     (with-eval-after-load 'org-capture
;;       (require 'org-projectile)))
;;   :config
;;   (if (file-name-absolute-p org-projectile-file)
;;       (progn
;;         (setq org-projectile-projects-file org-projectile-file)
;;         (push (org-projectile-project-todo-entry :empty-lines 1)
;;               org-capture-templates))
;;     (org-projectile-per-project)
;;     (setq org-projectile-per-project-filepath org-projectile-file)))

;; 上级目录保持可见
(use-package org-sticky-header
  :defer t
  :after org
  :init
  (add-hook 'org-mode-hook 'org-sticky-header-mode))

;; babel languages
(use-package ob-python
  :defer t
  :after org
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:python
   org-babel-execute:python)
  :init
  (progn
    ;; default的语言设置
    (setq org-babel-default-header-args:python
          '((:exports . "results")
            (:tangle .	"yes")
            (:cache . "yes")
            ;; (:session . "*org-python*")
            ))))

(use-package ob-shell
  :defer t
  :after org
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:sh
   ;; org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash)
  :init
  (progn
    ;; default的语言设置
    (setq org-babel-default-header-args:bash
          '((:exports . "results")
            (:cache . "yes")
            ))
    (setq org-babel-default-header-args:sh
          '((:exports . "results")
            (:cache . "yes")
            ))))

(use-package ob-R
  :defer t
  :after org
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:R
   org-babel-expand-body:R)
  :init
  ;; default的语言设置
  (progn
    (setq org-babel-default-header-args:R
          '((:exports . "results")
            (:colnames . "yes")
            (:tangle .	"yes")
            (:rownames . "yes")
            (:cache . "yes")
            (:session . "*org-R*")
            ))))

(use-package ob-emacs-lisp
  :defer t
  :after org
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:emacs-lisp
   org-babel-expand-body:lisp)
  :init
  (progn
    ;; default的语言设置
    (setq org-babel-default-header-args:bash
          '((:cache . "yes")
            ))))

(use-package ob-latex
  :defer t
  :after org
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:latex
   org-babel-expand-body:latex)
  :init
  (progn
    ;; default的语言设置
    (setq org-babel-default-header-args:latex
          '((:imagemagick . "yes")
            (:exports . "results")
            (:results . "graphics file")
            (:headers . "\\usepackage{tikz}")
            (:fit . "yes")
            (:imoutoptions . "-geometry 400")
            (:iminoptions . "-density 600")
            (:cache . "yes")
            ))))

;; other after load org
(use-package org
  :commands (org-agenda org-capture org-store-link)
  :init
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (define-key global-map "\C-cc" 'org-capture)
  :config
  (progn

    (require 'org-compat)
    (require 'org)
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; __, ==, **, //环境
    (defmacro idiig|org-emphasize (fname char)
      "Make function for setting the emphasis in org mode"
      `(defun ,fname () (interactive)
              (org-emphasize ,char)))
    
    ;; keybindings
    (idiig//org-set-keys)
    (idiig//org-declare-prefixes-for-mode)

    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))
    (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile) ;; ?????

    (setq org-stuck-projects
          '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))
    
    (setq org-startup-with-inline-images t)
    (setq org-latex-prefer-user-labels t)
    (setq org-image-actual-width nil)
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
    (setq org-log-done t)
    (setq org-log-done 'time)
    (setq org-imenu-depth 8)

    ;; define the refile targets
    (setq org-agenda-dir "~/Nutstore/org-files")
    (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
    (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
    (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
    (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
    (setq org-agenda-file-blogposts (expand-file-name "all-posts.org" org-agenda-dir))
    (setq org-agenda-files (list org-agenda-dir))

    ;; 时钟
    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line
    ;; Clock out save all org file
    (defun idiig/org-clock-out-save
        (interactive)
      (org-save-all-org-buffers)
      (org-clock-clock-out))
    ;; tag检索无视sublevel
    (setq org-tags-match-list-sublevels nil)
    
    ;; to-do keywords
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

    ;; 折行 
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (diminish 'auto-fill-function)
 
    ;; 代码高亮 
    (setq org-src-fontify-natively t)
    ;; 不询问eval
    ;; (setq org-confirm-babel-evaluate nil)
    ;; org Babel输出图片
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
    
    ;; org mode 图片输出展示
    (add-hook 'org-mode-hook 'org-display-inline-images)
    (when org-inline-image-overlays
      (org-redisplay-inline-images))

    ;; copy from chinese layer
    (defadvice org-html-paragraph (before org-html-paragraph-advice
                                          (paragraph contents info) activate)
      "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
      (let* ((origin-contents (ad-get-arg 1))
             (fix-regexp "[[:multibyte:]]")
             (fixed-contents
              (replace-regexp-in-string
               (concat
                "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
        (ad-set-arg 1 fixed-contents)))

    ;; hack for org headline toc
    (defun org-html-headline (headline contents info)
      "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
      (unless (org-element-property :footnote-section-p headline)
        (let* ((numberedp (org-export-numbered-headline-p headline info))
               (numbers (org-export-get-headline-number headline info))
               (section-number (and numbers
                                    (mapconcat #'number-to-string numbers "-")))
               (level (+ (org-export-get-relative-level headline info)
                         (1- (plist-get info :html-toplevel-hlevel))))
               (todo (and (plist-get info :with-todo-keywords)
                          (let ((todo (org-element-property :todo-keyword headline)))
                            (and todo (org-export-data todo info)))))
               (todo-type (and todo (org-element-property :todo-type headline)))
               (priority (and (plist-get info :with-priority)
                              (org-element-property :priority headline)))
               (text (org-export-data (org-element-property :title headline) info))
               (tags (and (plist-get info :with-tags)
                          (org-export-get-tags headline info)))
               (full-text (funcall (plist-get info :html-format-headline-function)
                                   todo todo-type priority text tags info))
               (contents (or contents ""))
               (ids (delq nil
                          (list (org-element-property :CUSTOM_ID headline)
                                (org-export-get-reference headline info)
                                (org-element-property :ID headline))))
               (preferred-id (car ids))
               (extra-ids
                (mapconcat
                 (lambda (id)
                   (org-html--anchor
                    (if (org-uuidgen-p id) (concat "ID-" id) id)
                    nil nil info))
                 (cdr ids) "")))
          (if (org-export-low-level-p headline info)
              ;; This is a deep sub-tree: export it as a list item.
              (let* ((type (if numberedp 'ordered 'unordered))
                     (itemized-body
                      (org-html-format-list-item
                       contents type nil info nil
                       (concat (org-html--anchor preferred-id nil nil info)
                               extra-ids
                               full-text))))
                (concat (and (org-export-first-sibling-p headline info)
                             (org-html-begin-plain-list type))
                        itemized-body
                        (and (org-export-last-sibling-p headline info)
                             (org-html-end-plain-list type))))
            (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
                  (first-content (car (org-element-contents headline))))
              ;; Standard headline.  Export it as a section.
              (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                      (org-html--container headline info)
                      (org-export-get-reference headline info)
                      (concat (format "outline-%d" level)
                              (and extra-class " ")
                              extra-class)
                      (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                              level
                              preferred-id
                              extra-ids
                              (concat
                               (and numberedp
                                    (format
                                     "<span class=\"section-number-%d\">%s</span> "
                                     level
                                     (mapconcat #'number-to-string numbers ".")))
                               full-text)
                              level)
                      ;; When there is no section, pretend there is an
                      ;; empty one to get the correct <div
                      ;; class="outline-...> which is needed by
                      ;; `org-info.js'.
                      (if (eq (org-element-type first-content) 'section) contents
                        (concat (org-html-section first-content "" info) contents))
                      (org-html--container headline info)))))))
    ))

(provide 'idiig-org)

