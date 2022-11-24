(use-package citar
  :after oc
  ;; :commands (org-cite-insert)
  ;; :after org markdown latex
  :bind
  ((:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
   (:map markdown-mode-map ("C-c b" . #'org-cite-insert))
   ;; (:map latex-mode-map ("C-c [" . #'org-cite-insert))
   )
  :custom
  ;; bibliography path
  (org-cite-global-bibliography
   '("~/Nutstore/bibfolder/bibliography.bib"))
  (citar-bibliography org-cite-global-bibliography)
  ;; Style file
  (org-cite-csl-styles-dir
   (expand-file-name "~/Nutstore/bibfolder/styles/"))
  ;; Active citar
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  ;; Note
  (citar-notes-paths (list "~/Nutstore/org-files/roam")) ; List of directories for reference nodes
  (citar-open-note-function 'orb-citar-edit-note) ; Open notes in `org-roam'
  (citar-at-point-function 'embark-act)           ; Use `embark'
  :init
  (progn
    (evil-leader/set-key-for-mode 'org-mode
      ;;   "rn" 'org-ref-open-notes-at-point
      ;;   "rp" 'org-ref-open-pdf-at-point
      "ic" #'org-cite-insert)

    ;; markdown-mode keybindings
    (evil-leader/set-key-for-mode 'markdown-mode
      "ic" #'org-cite-insert)

    ;; latex-mode keybindings 
    (evil-leader/set-key-for-mode 'latex-mode
      "ic" #'org-cite-insert)

    ;; template
    (setq citar-templates
          '((main . "${author editor:20}     ${date year issued:4}     ${title:60}")
            (suffix . "    ${=key= id:20}    ${=type=:12}")
            (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
            (note . "Notes on ${author editor}, ${title}")
            ))
    )
  :config
  ;; Cite processor
  (use-package oc-biblatex
      :ensure org-contrib)
  (setq org-cite-export-processors '(
     (md . (csl "apa.csl"))
     (latex biblatex)
     (odt . (csl "apa.csl"))
     (t . (csl "apa.csl")) 
     ))
  )

;; citar embark
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;; cross-ref
(use-package org-ref
  :diminish
  :after org
  :commands org-insert-link)

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
    (setq deft-directory "~/Nutstore/org-files/roam")
    (setq deft-text-mode 'org-mode)
    ;; keybindings
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
;;     (evil-leader/set-key-for-mode 'deft-mode
;;       "zT" 'zetteldeft-tag-buffer
;;       "zn" 'zetteldeft-new-file
;;       )

;;     ;; zetteldeft actions in org mode
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
  :diminish
  ;; :hook (after-init . org-roam-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-node-find
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
    ;; Reference Citation Zettel
    (defun idiig/org-roam-node-from-cite (keys-entries)
      (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
      (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                  "${author editor} :: ${title}")))
        (org-roam-capture- :templates
                           '(("r" "reference" plain "%?" :if-new
                              (file+head "reference/${citekey}.org"
                                         ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                              :immediate-finish t
                              :unnarrowed t))
                           :info (list :citekey (car keys-entries))
                           :node (org-roam-node-create :title title)
                           :props '(:finalize find-file))))
    ;; keybindings
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
    (setq org-roam-directory "~/Nutstore/org-files/roam"
          org-roam-verbose nil  ; https://youtu.be/fn4jIlFwuLU
          org-roam-buffer-no-delete-other-windows t ; make org-roam buffer sticky
          org-roam-completion-system 'default
          )
    ;; My Org-roam capture templates. There is one for each zettel type.
    (setq org-roam-capture-templates
          '(("m" "main" plain
             "%?"
             :if-new (file+head "main/${slug}.org"
                                "#+title: ${title}\n")
             :immediate-finish t
             :unnarrowed t)
            ("r" "reference" plain "%?"
             :if-new
             (file+head "reference/${title}.org" "#+title: ${title}\n")
             :immediate-finish t
             :unnarrowed t)
            ("a" "article" plain "%?"
             :if-new
             (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
             :immediate-finish t
             :unnarrowed t)))
    ;; Creating the property “type” on my nodes.
    (cl-defmethod org-roam-node-type ((node org-roam-node))
      "Return the TYPE of NODE."
      (condition-case nil
          (file-name-nondirectory
           (directory-file-name
            (file-name-directory
             (file-relative-name (org-roam-node-file node) org-roam-directory))))
        (error "")))
    ;; Modifying the display template to show the node “type”.
    (setq org-roam-node-display-template
          (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    ))

;; org roam network vis
(use-package websocket
  :after org-roam)

(use-package org-roam-ui
  ;; :straight
  ;; (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :hook (org-roam . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  )

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
   org-noter-notes-search-path "~/Nutstore/org-files/roam"
   ))

;; org-roam with bibtex creating notes for individual bibtex
(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (progn
    ;; (require 'org-ref)
    ;; (require 'yasnippet-snippets)
    (setq org-roam-bibtex-preformat-keywords
          '("citekey" "keywords" "title" "file" "author" "doi" "url")
          orb-process-file-field t
          orb-process-file-keyword t
          orb-file-field-extensions '("pdf"))
    (setq orb-templates
          '(("n" "ref+noter" plain (function org-roam-capture--get-point)
             ""
             :file-name "${slug}"
             :head "#+TITLE: ${citekey}: ${title}
#+ROAM_KEY: ${ref}
#+ROAM_TAGS:
- tags :: ${keywords}
* TODO Notes
  :PROPERTIES:
  :Custom_ID: ${citekey}
  :NOTER_DOCUMENT: ${file}
  :NOTER_PAGE:
  :AUTHOR: ${author-abbrev}
  :DOI: ${doi}
  :URL: ${url}
  :END:\n\n"
             :unnarrowed t)))))


(provide 'idiig-noter)
