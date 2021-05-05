;; org-ref 设定
(use-package org-ref
  :diminish
  :after org
  ;; :commands (org-ref-insert-cite-link)
  :init
  (progn
    (which-key-declare-prefixes-for-mode 'org-mode
      ",R" "org-ref"
      "SPC mR" "org-ref"
      ",Rnb" "org-ref-bib-notes"
      "SPC mRb" "org-ref-bib-pdf")
    (evil-leader/set-key-for-mode 'org-mode "Rn" 'org-ref-open-notes-at-point)
    (evil-leader/set-key-for-mode 'org-mode "Rp" 'org-ref-open-pdf-at-point)
    (evil-leader/set-key-for-mode 'org-mode "Rbn" 'org-ref-open-bibtex-notes)
    (evil-leader/set-key-for-mode 'org-mode "Rbp" 'org-ref-open-bibtex-pdf)
    ;; (evil-leader/set-key-for-mode 'org-mode "C-c ]" 'org-ref-insert-cite-key)
    )
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


(provide 'idiig-tex)
