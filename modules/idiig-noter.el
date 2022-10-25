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
    (setq org-roam-directory "~/Nutstore/org-files/roam"
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
   org-noter-notes-search-path "~/Nutstore/org-files/roam"
   ))

;; helm bibtex
(use-package ivy-bibtex
  :defer t
  :hook (
         (org-mode-hook . (lambda () (require 'ivy-bibtex)))
         (markdown-mode-hook . (lambda () (require 'ivy-bibtex)))
         (LaTeX-mode-hook . (lambda () (require 'ivy-bibtex)))
         )
  :config
  (progn
    (setq
     bibtex-completion-notes-path "~/Nutstore/org-files/roam"
     bibtex-completion-bibliography "~/Nutstore/bibfolder/bibliography.bib"
     bibtex-completion-pdf-field "file"
     bibtex-completion-notes-template-multiple-files
     (concat
      "#+TITLE: ${title}\n"
      "#+ROAM_KEY: cite:${citekey}\n"
      "* TODO Notes\n"
      ":PROPERTIES:\n"
      ":Custom_ID: ${citekey}\n"
      ":NOTER_DOCUMENT: ${file}\n"
      ":AUTHOR: ${author-abbrev}\n"
      ":JOURNAL: ${journaltitle}\n"
      ;; ":DATE: ${date}\n"
      ":YEAR: ${year}\n"
      ":DOI: ${doi}\n"
      ":URL: ${url}\n"
      ":END:\n\n"
      )
     )))


;; org-ref 设定
(use-package org-ref
  :diminish
  :after org
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
  :defer t
  :init
  (defun org-ref-open-pdf-at-point ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (car (bibtex-completion-find-pdf key))))
      (if (file-exists-p pdf-file)
          (org-open-file pdf-file)
        (message "No PDF found for %s" key))))
  (progn
    (add-hook 'org-mode-hook
              (lambda ()
                (require 'org-ref-ivy)                
                (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
                      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
                      org-ref-insert-label-function 'org-ref-insert-label-link
                      org-ref-insert-ref-function 'org-ref-insert-ref-link
                      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))
                      )
                )
              )
    (setq reftex-default-bibliography '("~/Nutstore/bibfolder/bibliography.bib"))
    ;; see org-ref for use of these variables
    (setq
     ;; org-ref-bibliography-notes "~/Nutstore/org-files/bibnote.org"
     org-ref-default-bibliography '"~/Nutstore/bibfolder/bibliography.bib"
     org-ref-pdf-directory "~/Nutstore/bibfolder/bibpdf"
     )
    (setq org-ref-note-title-format
          "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
          )
    (setq org-ref-notes-directory "~/Nutstore/org-files/roam"
          org-ref-notes-function 'orb-edit-notes)
    (setq org-ref-default-citation-link "citep")

    ;; basic keybindings
    (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-cite-link)
    ;; (define-key org-mode-map (kbd "s-[") 'org-ref-citation-hydra/body)
    
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
      ",r" "org-ref/roam"
      "SPC mr" "org-ref/roam")

    (evil-leader/set-key-for-mode 'org-mode
      "rn" 'org-ref-open-notes-at-point
      "rp" 'org-ref-open-pdf-at-point
      "ic" 'org-ref-insert-cite-link)

    ;; markdown-mode keybindings
    (evil-leader/set-key-for-mode 'markdown-mode
      "ic" 'org-ref-insert-cite-link)

    ;; latex-mode keybindings 
    (evil-leader/set-key-for-mode 'latex-mode
      "ic" 'org-ref-insert-cite-link))
  )


;; org-roam with bibtex creating notes for individual bibtex
(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (progn
    (require 'org-ref)
    (require 'yasnippet-snippets)
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
