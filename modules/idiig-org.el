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
    (setq deft-directory "~/Nutstore/org-notes")
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

;; zetteldeft for linking notes
(use-package zetteldeft
  :defer t
  ;; :after (org deft)
  :init
  (progn
    (zetteldeft-set-classic-keybindings)
    ;; zetteldeft actions in deft mode
    (which-key-declare-prefixes-for-mode 'deft-mode
      ",z" "zetteldeft"
      "SPC mz" "zetteldeft")
    (evil-leader/set-key-for-mode 'deft-mode
      "zT" 'zetteldeft-tag-buffer
      "zn" 'zetteldeft-new-file
      )
    
    ;; zetteldeft actions in org mode
    (which-key-declare-prefixes-for-mode 'org-mode
      ",z" "zetteldeft"
      "SPC mz" "zetteldeft")
    (evil-leader/set-key-for-mode 'org-mode
      "zc" 'zetteldeft-search-current-id
      "zf" 'zetteldeft-follow-link
      "zt" 'zetteldeft-avy-tag-search
      "zN" 'zetteldeft-new-file-and-link
      "zr" 'zetteldeft-file-rename
      "zi" 'zetteldeft-find-file-id-insert
      "zI" 'zetteldeft-find-file-full-title-insert
      "zs" 'zetteldeft-search-at-point
      "zl" 'zetteldeft-avy-link-search
      "zF" 'zetteldeft-avy-file-search-ace-window
      "zo" 'zetteldeft-find-file
      )
    ))

;; org-roam for linking notes
(use-package org-roam
  :defer t
  :diminish (org-roam-mode)
  :hook (after-init . org-roam-mode)
  :init
  (progn
    (setq org-roam-directory "~/Nutstore/org-notes")
    (which-key-declare-prefixes "SPC or" "org-roam")
    (which-key-declare-prefixes "SPC ord" "org-roam-dailies")
    (which-key-declare-prefixes "SPC ort" "org-roam-tags")
    (which-key-declare-prefixes "C-SPC or" "org-roam")
    (which-key-declare-prefixes "C-SPC ord" "org-roam-dailies")
    (which-key-declare-prefixes "C-SPC ort" "org-roam-tags")
    (evil-leader/set-key
      "ordy" 'org-roam-dailies-find-yesterday
      "ordt" 'org-roam-dailies-find-today
      "ordT" 'org-roam-dailies-find-tomorrow
      "ordd" 'org-roam-dailies-find-date
      "orf" 'org-roam-find-file
      "org" 'org-roam-graph
      "ori" 'org-roam-insert
      "orI" 'org-roam-insert-immediate
      "orl" 'org-roam-buffer-toggle-display
      "orta" 'org-roam-tag-add
      "ortd" 'org-roam-tag-delete
      "ora" 'org-roam-alias-add)

    (which-key-declare-prefixes-for-mode 'org-mode "SPC mr" "org-roam")
    (which-key-declare-prefixes-for-mode 'org-mode "SPC mrd" "org-roam-dailies")
    (which-key-declare-prefixes-for-mode 'org-mode "SPC mrt" "org-roam-tags")
    (which-key-declare-prefixes-for-mode 'org-mode ",r" "org-roam")
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
      "ra" 'org-roam-alias-add)))

;; org-journal
(use-package org-journal
  :defer t
  :commands (org-journal-new-entry org-journal-search-forever)
  :init
  (progn
    (setq org-journal-dir "~/Nutstore/org-notes")
    (which-key-declare-prefixes "SPC oj" "org-journal")
    (which-key-declare-prefixes "C-SPC oj" "org-journal")
    (evil-leader/set-key
      "ojf" 'org-journal-open-current-journal-file
      "ojj" 'org-journal-new-entry
      "ojs" 'org-journal-search-forever
      "ojt" 'org-journal-new-scheduled-entry
      "ojv" 'org-journal-schedule-view)

    (evil-leader/set-key-for-mode 'calendar-mode
      "r" 'org-journal-read-entry
      "i" 'org-journal-new-date-entry
      "n" 'org-journal-next-entry
      "p" 'org-journal-previous-entry
      "s" 'org-journal-search-forever
      "w" 'org-journal-search-calendar-week
      "m" 'org-journal-search-calendar-month
      "y" 'org-journal-search-calendar-year)

    (evil-leader/set-key-for-mode 'org-journal-mode
      "j" 'org-journal-new-entry
      "n" 'org-journal-next-entry
      "p" 'org-journal-previous-entry)))

;; org-ref 设定
(use-package org-ref
  :diminish
  :hook (org-mode-hook . (lambda () (require 'org-ref)))
  :defer t
  ;; :after org
  :commands (org-ref-bibtex-next-entry
             org-ref-bibtex-previous-entry
             org-ref-open-in-browser
             org-ref-open-bibtex-notes
             org-ref-open-bibtex-pdf
             org-ref-bibtex-hydra/body
             org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
             org-ref-sort-bibtex-entry
             arxiv-add-bibtex-entry
             arxiv-get-pdf-add-bibtex-entry
             doi-utils-add-bibtex-entry-from-doi
             isbn-to-bibtex
             pubmed-insert-bibtex-from-pmid)
  :init
  (progn
    ;; bibtex keybindings
    (evil-define-key 'normal bibtex-mode-map
      (kbd "C-j") 'org-ref-bibtex-next-entry
      (kbd "C-k") 'org-ref-bibtex-previous-entry
      "M-j" 'org-ref-bibtex-next-entry
      "M-k" 'org-ref-bibtex-previous-entry)

    ;; bibtex-mode keybindings
    (evil-leader/set-key-for-mode 'bibtex-mode
      ;; Navigation
      "j" 'org-ref-bibtex-next-entry
      "k" 'org-ref-bibtex-previous-entry

      ;; Open
      "b" 'org-ref-open-in-browser
      "n" 'org-ref-open-bibtex-notes
      "p" 'org-ref-open-bibtex-pdf

      ;; Misc
      "h" 'org-ref-bibtex-hydra/body
      "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
      "s" 'org-ref-sort-bibtex-entry

      ;; Lookup utilities
      "la" 'arxiv-add-bibtex-entry
      "lA" 'arxiv-get-pdf-add-bibtex-entry
      "ld" 'doi-utils-add-bibtex-entry-from-doi
      "li" 'isbn-to-bibtex
      "lp" 'pubmed-insert-bibtex-from-pmid)

    ;; org mode keybindings
    (which-key-declare-prefixes-for-mode 'org-mode
      ",R" "org-ref"
      "SPC mR" "org-ref"
      ",Rnb" "org-ref-bib-notes"
      "SPC mRb" "org-ref-bib-pdf")
    
    (evil-leader/set-key-for-mode 'org-mode
      "Rn" 'org-ref-open-notes-at-point
      "Rp" 'org-ref-open-pdf-at-point
      "Rbn" 'org-ref-open-bibtex-notes
      "Rbp" 'org-ref-open-bibtex-pdf
      "ic" 'org-ref-helm-insert-cite-link)

    ;; markdown-mode keybindings
    (evil-leader/set-key-for-mode 'markdown-mode
      "ic" 'org-ref-helm-insert-cite-link)

    ;; latex-mode keybindings 
    (evil-leader/set-key-for-mode 'latex-mode
      "ic" 'org-ref-helm-insert-cite-link))

  :config
  (progn
    (setq org-ref-completion-library 'org-ref-ivy-cite)
    (setq reftex-default-bibliography '("~/Nutstore/bibfolder/bibliography.bib"))
    ;; see org-ref for use of these variables
    (setq org-ref-bibliography-notes "~/Nutstore/org-notes/bibnote.org"
          org-ref-default-bibliography '("~/Nutstore/bibfolder/bibliography.bib")
          org-ref-pdf-directory "~/Nutstore/bibfolder/bibpdf")
    (setq bibtex-completion-bibliography "~/Nutstore/bibfolder/bibliography.bib"
          bibtex-completion-library-path "~/Nutstore/bibfolder/bibpdf"
          bibtex-completion-notes-path "~/Nutstore/org-notes/bibnote.org")
    (setq org-ref-default-citation-link "citep")
    (defun org-ref-open-pdf-at-point ()
      "Open the pdf for bibtex key under point if it exists."
      (interactive)
      (let* ((results (org-ref-get-bibtex-key-and-file))
             (key (car results))
             (pdf-file (car (bibtex-completion-find-pdf key))))
        (if (file-exists-p pdf-file)
            (org-open-file pdf-file)
          (message "No PDF found for %s" key))))))

;; org-roam with bibtex creating notes for individual bibtex
(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref))

;; noter
(use-package org-noter
  )


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
  (org-agenda-current-time-string "<<<< now")
  (org-agenda-time-grid ;; Format is changed from 9.1
   '((daily today require-timed)
     (0000 0100 0200 0300 0400 0500 0600 0700 0800 0900 1000 1100 1200
           1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300 2400)
     "-"
     "────────"))
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
  (org-babel-execute:python))

(use-package ob-shell
  :defer t
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-R
  :defer t
  :after org
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:R
   org-babel-expand-body:R))

(use-package ob-emacs-lisp
  :defer t
  :after org
  :ensure org-plus-contrib
  :commands
  (org-babel-execute:emacs-lisp
   org-babel-expand-body:lisp))

;; (use-package ob-latex
;;   :defer t
;;   :after org
;;   :ensure org-plus-contrib
;;   :commands
;;   (org-babel-execute:latex)
;;   (org-babel-expand-body:latex))

;; after load org
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
    (setq org-agenda-dir "~/Nutstore/org-notes")
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

    ;; __, ==, **, //环境
    (defmacro idiig|org-emphasize (fname char)
      "Make function for setting the emphasis in org mode"
      `(defun ,fname () (interactive)
              (org-emphasize ,char)))
    
    ;; keybindings
    (idiig//org-set-keys)
    (idiig//org-declare-prefixes-for-mode)

    ;; 折行 
    (add-hook 'org-mode-hook 'auto-fill-mode)
    (diminish 'auto-fill-function)

    ;; to-do keywords
    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

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
             :empty-lines 1)))

    ;; org capture
    (with-eval-after-load 'org-capture

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
                     (function org-hugo-new-subtree-post-capture-template))))

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

    ;; 代码块
    (add-hook 'org-mode-hook '(lambda ()
                                ;; keybinding for editing source code blocks
                                ;; keybinding for inserting code blocks
                                (local-set-key (kbd "C-c i s")
                                               'idiig/org-insert-src-block)))

    ;; ;; 可使用代码
    ;; (org-babel-do-load-languages
    ;;  'org-babel-load-languages
    ;;  '((perl . t)
    ;;    (ruby . t)
    ;;    (shell . t)
    ;;    (dot . t)
    ;;    (js . t)
    ;;    (latex .t)
    ;;    (python . t)
    ;;    (emacs-lisp . t)
    ;;    (C . t)
    ;;    (R . t)
    ;;    ))

    ;; ;; default的语言设置
    ;; (setq org-babel-default-header-args:R
    ;;       '((:exports . "results")
    ;;         (:colnames . "yes")
    ;;         (:rownames . "yes")
    ;;         (:session . "*org-R*")
    ;;         ))

    ;; 不询问eval
    (setq org-confirm-babel-evaluate nil)

    ;; org Babel输出图片
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
    (add-hook 'org-mode-hook 'org-display-inline-images)
    ;; org mode 图片输出展示
    (when org-inline-image-overlays
      (org-redisplay-inline-images))

    ;; https://qiita.com/kawabata@github/items/1b56ec8284942ff2646b
    ;; latex setting
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

    (with-eval-after-load "ox"
      (add-hook 'org-export-before-processing-hook 'remove-org-newlines-at-cjk-text)
      ;; reveal-js
      (use-package ox-reveal)
      ;; markdown
      (require 'ox-md nil t)
      ;; hogo
      (use-package ox-hugo) 
      ;; TeX
      (require 'ox-latex)
      (require 'ox-beamer)
      ;; (setq org-latex-compiler "xelatex")
      (add-to-list 'org-latex-classes
                   '("jp_notes"
                     "
\\documentclass[autodetect-engine,dvi=dvipdfmx,11pt,ja=standard]{bxjsarticle}
\\usepackage{amsmath}
\\usepackage{graphicx}
\\usepackage[T1]{fontenc}
\\usepackage{geometry}
\\usepackage{xeCJK}
\\geometry{a4paper,left=20mm,right=20mm,top=8mm,bottom=12mm,heightrounded}
\\usepackage{subcaption}
% \\usepackage{otf}
\\usepackage[yyyymmdd]{datetime}
\\renewcommand{\\dateseparator}{/}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{textcomp}
\\usepackage{multicol}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{booktabs}
\\tolerance=1000
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
\\usepackage{tikz}
\\usepackage[authoryear]{natbib}
\\bibpunct[: ]{(}{)}{,}{a}{}{,}
\\usepackage{url}
%
\\usepackage{listings}
\\lstset{
basicstyle=\\small\\ttfamily,
numbers=left,
numberstyle=\\footnotesize,
stepnumber=1,
numbersep=5pt,
backgroundcolor=\\color{white},
showspaces=false,
showstringspaces=false,
showtabs=false,
frame=tb,
tabsize=2,
captionpos=b,
breaklines=true,
breakatwhitespace=false,
escapeinside={\\%*}{*)} 
}
%
\\usepackage{etoolbox}
\\makeatletter
\\patchcmd{\\@verbatim}
  {\\verbatim@font}
  {\\verbatim@font\\small}
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
\\usepackage{fancyhdr}
\\pagestyle{fancyplain}
\\fancyhf{}
\\chead{\\fancyplain{}{draft: \\jobname}}
\\lhead{\\fancyplain{}{latest update: \\today\\enspace\\currenttime}}
\\rhead{\\fancyplain{}{\\includegraphics[width=0.2\\textwidth]{/Users/idiig/Nutstore/tex_related/tokyotechmark.eps}}}
\\cfoot{\\fancyplain{}{\\thepage}}
%
\\usepackage{caption}
\\captionsetup[figure]{labelfont={bf}, labelsep=colon, justification=raggedright, format=hang}
\\captionsetup[table]{labelfont={bf}, labelsep=colon, justification=raggedright, format=hang}
%
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
\\makeatletter
\\renewcommand{\\maketitle}{\\bgroup\\setlength{\\parindent}{0pt}
\\begin{flushleft}
  \\ \\\\
  \\vspace{-0.5em}
  \\Large{\\textsf{\\@title}}\\\\
  \\normalsize\\@author
\\end{flushleft}\\egroup
\\vspace{-1em}
}
\\makeatother
"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (add-to-list 'org-latex-classes
                   '("en_notes"
                     "
\\documentclass[autodetect-engine,dvi=dvipdfmx,11pt]{article}
\\usepackage{amsmath}
\\usepackage{graphicx}
\\usepackage[T1]{fontenc}
\\usepackage{geometry}
\\usepackage{xeCJK}
% \\setCJKmainfont{IPAexMincho}
\\setCJKmainfont[Scale=MatchLowercase]{ipaexm.ttf}
\\geometry{a4paper,left=20mm,right=20mm,top=20mm,bottom=20mm,heightrounded}
\\usepackage{subcaption}
% \\usepackage{otf}
\\usepackage[yyyymmdd]{datetime}
\\renewcommand{\\dateseparator}{/}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{textcomp}
\\usepackage{multicol}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amsmath,amsthm,amssymb}
\\usepackage{bm}
\\usepackage{blkarray}
\\usepackage{booktabs}
\\tolerance=1000
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
\\usepackage{tikz}
% \\usepackage[authoryear]{natbib}
% \\bibpunct[: ]{(}{)}{,}{a}{}{,}
\\usepackage{csquotes}
\\usepackage[natbib,style=apa,backend=biber,giveninits=false]{biblatex}
% \\NewBibliographyString{diplomathesis}
\\DefineBibliographyStrings{english}{phdthesis = {PhD dissertation}}
% \\usepackage[authordate,giveninits=false,natbib,backend=biber,ptitleaddon=space,isbn=false,url=false,eprint=false,doi=false,bibencoding=utf8]{biblatex-chicago}
% \\DeclareFieldFormat{nameaddon}{#1}
\\usepackage{url}
%
\\usepackage{listings}
\\lstset{
basicstyle=\\small\\ttfamily,
numbers=left,
numberstyle=\\footnotesize,
stepnumber=1,
numbersep=5pt,
backgroundcolor=\\color{white},
showspaces=false,
showstringspaces=false,
showtabs=false,
frame=tb,
tabsize=2,
captionpos=b,
breaklines=true,
breakatwhitespace=false,
escapeinside={\\%*}{*)} 
}
%
\\usepackage{etoolbox}
\\makeatletter
\\patchcmd{\\@verbatim}
  {\\verbatim@font}
  {\\verbatim@font\\small}
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
\\usepackage{fancyhdr}
\\pagestyle{fancyplain}
\\fancyhf{}
\\chead{\\fancyplain{}{draft: \\jobname}}
\\lhead{\\fancyplain{}{latest update: \\today\\enspace\\currenttime}}
\\rhead{\\fancyplain{}{\\includegraphics[width=0.2\\textwidth]{/Users/idiig/Nutstore/tex_related/tokyotechmark.eps}}}
\\cfoot{\\fancyplain{}{\\thepage}}
%
\\usepackage{caption}
\\captionsetup[figure]{labelfont={bf}, labelsep=colon, justification=raggedright, format=hang}
\\captionsetup[table]{labelfont={bf}, labelsep=colon, justification=raggedright, format=hang}
%
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
\\makeatletter
\\renewcommand{\\maketitle}{\\bgroup\\setlength{\\parindent}{0pt}
\\begin{flushleft}
  \\ \\\\
  \\vspace{-0.5em}
  \\Large{\\textsf{\\@title}}\\\\
  \\normalsize\\@author
\\end{flushleft}\\egroup
\\vspace{-1em}
}
\\makeatother
"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      ;; 输出设定
      (setq org-latex-pdf-process
            '("xelatex %f"
              "xelatex %f"
              "biber %b"
              "xelatex %f"
              "xelatex %f"
              "dvipdfmx %b.dvi"
              "rm -fr %b.bbl %b.dvi %b.tex auto"
              ))))
  )

(provide 'idiig-org)

